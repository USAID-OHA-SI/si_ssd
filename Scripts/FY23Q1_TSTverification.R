# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  verify findings of target setting analysis
# REF ID:   912eea3d 
# LICENSE:  MIT
# DATE:     2023-03-10
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(janitor)
  library(ggtext)
  library(ggplot2)
  library(googlesheets4)
  library(assertthat)
  library(assertr)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "912eea3d"
  load_secrets()
  

# IMPORT ----------------------------------------------------------------------
  
  path <- "PSNU_IM_South_Sudan"
  
  df_recent <- si_path() %>%
    return_latest(path) %>%
    read_psd()
  
  tst_path <- "1qOu3UXgxZ5od0nk4Xo6MrirtVaCztR2xs4ltx884Gns"
  
  tst <- read_sheet(tst_path, skip = 1, col_names = TRUE)
  
  age_xwalk_path <- here::here("Data/")
  
  age_map <- age_xwalk_path %>% 
    return_latest("age_mapping.xlsx") %>% 
    readxl::read_xlsx()

# MUNGE -----------------------------------------------------------------------

  # TX_CURR by PSNU, age, and sex from MER
  
  recent_check <- df_recent %>%
    resolve_knownissues() %>%
    clean_indicator() %>%
    filter(indicator %in% c("TX_CURR"),
           fiscal_year == 2022,
           str_detect(standardizeddisaggregate, "Age/Sex/") == TRUE) %>%
    left_join(age_map, by = c("indicator", "ageasentered" = "age_msd")) %>% 
    mutate(age = ifelse(is.na(age_dp), ageasentered, age_dp)) %>% 
    select(-c(ageasentered, age_dp)) %>% 
    group_by(across(-c(cumulative, targets))) %>% 
    summarise(across(c(cumulative, targets), sum, na.rm = TRUE), .groups = "drop") %>% 
    group_by(fiscal_year, psnu, indicator, age, sex) %>%
    summarize(
      fy22q4_MER = sum(cumulative, na.rm = TRUE)) %>%
    arrange(psnu) %>%
    # pad psnu to match TST and join correctly
    mutate(
      psnu = glue::glue("{psnu} "), 
      fy22q4_MER = replace_na(fy22q4_MER, 0))
  
  # TX_CURR by PSNU, age, and sex from TST
  
  names <- c("prioritization", "psnu", "age", "sex", "fy22q4_TST")
  names(tst) <- names 
  
  tst <- tst %>%
    select(prioritization, psnu, age, sex, fy22q4_TST)
  
  summary <- tst %>%
    filter(prioritization == "Prioritization" & psnu == "PSNU")
  
  tst_filt <- tst %>%
    select(psnu, age, sex, fy22q4_TST) %>%
    filter(psnu != "PSNU") %>%
    mutate(
      fy22q4_TST = replace_na(fy22q4_TST, 0),
      psnu = str_extract(psnu, ".* "),
      fiscal_year = 2022, 
      indicator = "TX_CURR")
  
  # spot check estimates in individual files where we expect them to
  # be equal
  
  # for females ages 15-24 in Juba County, do the TST estimates match the MER?
  
  # MER data
  df_juba_mer <- recent_check %>%
    filter(psnu == "Juba County ", 
           age == "15-24", 
           sex == "Female")
  
  # TST data 
  df_juba_tst <- tst_filt %>%
    filter(psnu == "Juba County ", 
           age == "15-24", 
           sex == "Female")
  
  validate_that(are_equal(df_juba_mer$fy22q4_MER, 
                          df_juba_tst$fy22q4_TST) == TRUE, 
                msg = "Alert! MER and TST values do not match 
                       as expected. Please check the TST values")

  # join MER and TST to catch any differences in entire dataset
  
  full_df <- recent_check %>%
    full_join(tst_filt, by = c("fiscal_year", 
                               "indicator",
                               "psnu", 
                               "age", 
                               "sex")) %>%
    # remove psnus not PEPFAR supported included in TST
    drop_na(fy22q4_MER) 
    
    
   # review mis-matches
    mismatches <- full_df %>%
    filter(fy22q4_MER != fy22q4_TST) 
    
    mismatches %>%
    write_sheet("1U7Mq8oQuAL2SHi-5EMLV_97o-_Lbcd6vcN2VZE6GfVU", "Sheet 1")
  