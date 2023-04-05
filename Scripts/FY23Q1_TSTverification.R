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
  library(tameDP)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "912eea3d"
  load_secrets()
  
# IMPORT ----------------------------------------------------------------------
  
  path <- "PSNU_IM_South_Sudan"
  
  df_recent <- si_path() %>%
    return_latest(path) %>%
    read_psd()
  
  #v4
  tst_path <- here::here("Data/")
  
  tst <- tst_path %>%
    return_latest("DATA_PACK_South Sudan") %>%
    tame_dp()
  
  age_xwalk_path <- here::here("Data/")
  
  age_map <- age_xwalk_path %>% 
    return_latest("age_mapping.xlsx") %>% 
    readxl::read_xlsx()

# MUNGE -----------------------------------------------------------------------

  # TX_CURR by PSNU, age, and sex from MER
  
  recent_check <- df_recent %>%
    resolve_knownissues() %>%
    clean_indicator() %>%
    filter(str_detect(standardizeddisaggregate, "Age/Sex/") == TRUE) %>%
    left_join(age_map, by = c("indicator", "ageasentered" = "age_msd")) %>% 
    mutate(age = ifelse(is.na(age_dp), ageasentered, age_dp)) %>% 
    select(-c(ageasentered, age_dp)) %>% 
    group_by(across(-c(cumulative, targets))) %>% 
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>% 
    group_by(fiscal_year, psnu, indicator, age, sex) %>%
    summarize(
      fy22_MER = sum(cumulative, na.rm = TRUE)) %>%
    arrange(psnu) %>%
    # pad psnu to match TST and join correctly
    mutate(
      psnu = glue::glue("{psnu} "), 
      fy22_MER = replace_na(fy22_MER, 0))
  
  # rename cumulative in TST to compare to MER
  
  tst_rename <- tst %>%
    rename(fy22_TST = cumulative)
  
  # spot check estimates in individual files where we expect them to
  # be equal
  
  # for females ages 15-24 in Juba County, do the TST estimates match the MER?
  
  # MER data
  df_juba_mer <- recent_check %>%
    filter(psnu == "Juba County ", 
           age == "15-24", 
           sex == "Female", 
           indicator == "TX_CURR", 
           fiscal_year == 2022)
  
  # TST data 
  df_juba_tst <- tst_rename %>%
    filter(psnu == "Juba County", 
           ageasentered == "15-24", 
           sex == "Female", 
           indicator == "TX_CURR", 
           fiscal_year == 2022)
  
  validate_that(are_equal(df_juba_mer$fy22_MER, 
                          df_juba_tst$fy22_TST) == TRUE, 
                msg = glue::glue("Alert! MER and TST values do not match 
                       as expected. Please check the TST values"))

  # join MER and TST to catch any differences in entire dataset
  
  full_df <- recent_check %>%
    full_join(tst_filt, by = c("fiscal_year", 
                               "indicator",
                               "psnu", 
                               "age", 
                               "sex")) %>%
    # remove psnus not PEPFAR supported included in TST
    drop_na(fy22_MER) 
    
    
   # review mis-matches
    mismatches <- full_df %>%
    filter(fy22_MER != fy22_TST) 
    
    mismatches %>%
    write_sheet("1U7Mq8oQuAL2SHi-5EMLV_97o-_Lbcd6vcN2VZE6GfVU", "Sheet 1")
  