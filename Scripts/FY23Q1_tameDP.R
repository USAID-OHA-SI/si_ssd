# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  exploring the draft TST
# REF ID:   9316532e 
# LICENSE:  MIT
# DATE:     2023-03-16
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(tameDP)

  # for checking against MSD
  library(gagglr)
  library(readxl)

  # export to google sheets
  library(googlesheets4)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "9316532e"
  load_secrets()
  
  tst_path_1 <- "Data/DATA_PACK_South Sudan_20230214202235_Mar_15_2023_Draft V1___________________.xlsx"
  tst_path_2 <- "Data/DATA_PACK_South Sudan_20230214202235_Mar_20_2023_Draft V2.xlsx"
  
  psnu_path <- "Genie_PSNU_IM_South_Sudan"
  
  # update age bands
  
  age_xwalk_path <- here::here("Data/")

# IMPORT ----------------------------------------------------------------------
  
  tst_df <- tame_dp(tst_path_2)
  
  msd_df <- si_path() %>%
    return_latest(psnu_path) %>%
    read_psd()
  
  get_metadata(type = "PSNU_IM")
  metadata_msd_psnu <- metadata
  
  age_map <- age_xwalk_path %>% 
    return_latest("age_mapping.xlsx") %>% 
    readxl::read_xlsx()
  
# MUNGE -----------------------------------------------------------------------
    
 # psnu_level TST
  tst_psnu <- tst_df %>%
    clean_indicator() %>%
    ungroup() %>%
    group_by(
      operatingunit, country,
      psnu,
      fiscal_year,
      indicator, standardizeddisaggregate,snuprioritization,
      ageasentered, sex, numeratordenom) %>%
    summarize(
      targets = sum(targets), 
      cumulative = sum(cumulative)) %>%
    arrange(psnu, fiscal_year, indicator, ageasentered, sex) %>%
    ungroup() %>%
    select(country,psnu,fiscal_year,
             indicator, snuprioritization,   standardizeddisaggregate, ageasentered, sex, 
           cumulative, targets) %>%
    rename(
      cumulative_tst = cumulative, 
      targets_tst = targets, 
      age = ageasentered)
  
  # psnu level MSD to match
  msd_psnu <- msd_df %>%
    clean_indicator() %>%
    resolve_knownissues() %>%
    left_join(age_map, by = c("indicator", "ageasentered" = "age_msd")) %>% 
    mutate(age = ifelse(is.na(age_dp), ageasentered, age_dp)) %>% 
    select(-c(ageasentered, age_dp)) %>% 
    group_by(
      operatingunit, country,psnu,fiscal_year,indicator, age, sex, 
      standardizeddisaggregate, snuprioritization) %>%
    # group_by(across(-c(cumulative, targets))) %>% 
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    filter(psnu %in% tst_psnu$psnu,
           indicator %in% tst_psnu$indicator,
           snuprioritization %in% tst_psnu$snuprioritization,
           standardizeddisaggregate %in% tst_psnu$standardizeddisaggregate,
           fiscal_year %in% tst_psnu$fiscal_year) %>%
    rename(
      cumulative_msd = cumulative, 
      targets_msd = targets)
  
  # combine + compare
  
  combined <- full_join(tst_psnu, msd_psnu, by = c("country","psnu","fiscal_year",
                                                   "indicator","snuprioritization",
                                                   "standardizeddisaggregate",
                                                   "age", "sex")) %>%
    replace_na()
  
    write_sheet(combined, "18dNyrg6tseuXJHA-SynmuwCVi_9DaBFZx1pch3rlhsI", 
                "Checkpoint 1")
  
  #matches
  comb_matches_tgts <- combined %>%
    filter(
      targets_tst == targets_msd)
  
    write_sheet(comb_matches_tgts, "1IvLeqJWcPqeJ84HlWN6yt2hx5d0WCl3osUh6msemgF4", 
                "Checkpoint 1")
    
  #matches
  comb_matches_cml <- combined %>%
    filter(
      cumulative_msd == cumulative_tst)
  
    write_sheet(comb_matches_cml, "11lK3RWXAapwl4FpIRp5wRtVH1qMjJcL7gVKwTsUM96Y", 
                "Checkpoint 1")
  
  #mismatches
  comb_mismatches_tgts <- combined %>%
    filter(
      targets_tst != targets_msd)
  
    write_sheet(comb_mismatches_tgts, "1XWQKWGeyusokL0Z9Jv56KZOhhin9kA_OAttuzFir93w", 
                "Checkpoint 1")
  
  #mismatches
  comb_mismatches_cml <- combined %>%
    filter(
      cumulative_msd != cumulative_tst)
  
    write_sheet(comb_mismatches_cml, "1zL9xUKECyQdhYNjFyfVWAH9YEHknEYSeUMPeDF8boKA", 
                "Checkpoint 1")
    