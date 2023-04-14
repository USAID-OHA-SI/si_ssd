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
  
  # TST filepath
  path <- here::here("Data/")
  
  tst_path <- path  %>%
    return_latest("DATA_PACK_South Sudan")
  
  # PSNUxIM
  tst_psnuim_path <- path  %>%
    return_latest("South Sudan_PSNUxIM")
  
  psnu_path <- si_path() %>%
    return_latest("Genie_PSNU_IM_South_Sudan")
  
  # update age bands
  
  age_xwalk_path <- here::here("Data/")

# IMPORT ----------------------------------------------------------------------
  
  tst_df <- path %>%
    return_latest("DATA_PACK_South Sudan") %>%
    tame_dp()
  
  tst_psnuim <- path %>%
    return_latest("South Sudan_PSNUxIM") %>%
    tame_dp(type = "PSNUxIM") 
  
  msd_df <- si_path() %>%
    return_latest("Genie_PSNU_IM_South_Sudan") %>%
    read_psd()
  
  get_metadata(type = "PSNU_IM")
  metadata_msd_psnu <- metadata
  
  age_map <- age_xwalk_path %>% 
    return_latest("age_mapping.xlsx") %>% 
    readxl::read_xlsx()
  
# MUNGE -----------------------------------------------------------------------
  
  msd_final <- align_msd_disagg(msd_path = psnu_path, dp_path = tst_path) %>%
    align_ageband(collapse = FALSE)
  
  df_msd_tst <- msd_final %>%
    bind_rows(tst_df)
    
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
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
    filter(psnu %in% tst_psnu$psnu,
           indicator %in% tst_psnu$indicator,
           snuprioritization %in% tst_psnu$snuprioritization,
           standardizeddisaggregate %in% tst_psnu$standardizeddisaggregate,
           fiscal_year %in% tst_psnu$fiscal_year) %>%
    rename(
      cumulative_msd = cumulative,
      targets_msd = targets)
  
  # munge 2022 COP targets and results for TX_CURR
  
  msd_2022 <- msd_df %>%
    group_by(
      operatingunit, country,psnu,fiscal_year,indicator) %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") %>%
    filter(indicator == "TX_CURR", 
           fiscal_year == "2022") %>%
    mutate(psnu = if_else(psnu == "_Military South Sudan", "Military", psnu), 
           targets = as.numeric(targets),
           psnu = fct_reorder(factor(psnu), targets, .desc = FALSE))
  
  tst_2023 <- tst_psnuim %>%
    mutate(fiscal_year = as.character(fiscal_year), 
           targets = as.numeric(targets)) %>%
    filter(indicator == "TX_CURR") %>%
    group_by(
      operatingunit, country,psnu,fiscal_year,indicator) %>%
    summarise(across(c(cumulative, targets), \(x) sum(x, na.rm = TRUE)), 
              .groups = "drop") %>%
    mutate(psnu = if_else(psnu == "_Military South Sudan", "Military", psnu), 
           psnu = fct_reorder(factor(psnu), as.numeric(targets), .desc = FALSE))

  # combine + compare ----------------------------------------------------------
  
  combined <- full_join(tst_psnu, msd_psnu, by = c("country","psnu","fiscal_year",
                                                   "indicator","snuprioritization",
                                                   "standardizeddisaggregate",
                                                   "age", "sex")) 
  
    write_sheet(combined, "18dNyrg6tseuXJHA-SynmuwCVi_9DaBFZx1pch3rlhsI", 
                "Checkpoint 2")
  
  #matches
  comb_matches_tgts <- combined %>%
    filter(
      targets_tst == targets_msd)
  
    write_sheet(comb_matches_tgts, "1IvLeqJWcPqeJ84HlWN6yt2hx5d0WCl3osUh6msemgF4", 
                "Checkpoint 2")
    
  #matches
  comb_matches_cml <- combined %>%
    filter(
      cumulative_msd == cumulative_tst)
  
    write_sheet(comb_matches_cml, "11lK3RWXAapwl4FpIRp5wRtVH1qMjJcL7gVKwTsUM96Y", 
                "Checkpoint 2")
  
  #mismatches
  comb_mismatches_tgts <- combined %>%
    filter(
      targets_tst != targets_msd)
  
    write_sheet(comb_mismatches_tgts, "1XWQKWGeyusokL0Z9Jv56KZOhhin9kA_OAttuzFir93w", 
                "Checkpoint 2")
  
  #mismatches
  comb_mismatches_cml <- combined %>%
    filter(
      cumulative_msd != cumulative_tst)
  
    write_sheet(comb_mismatches_cml, "1zL9xUKECyQdhYNjFyfVWAH9YEHknEYSeUMPeDF8boKA", 
                "Checkpoint 2")
    
# check PSNUxIM tab ------------------------------------------------------------

    cop_23 <- tst_2023 %>%
      ggplot(aes(y = psnu)) +
      geom_col(aes(x = as.numeric(targets)), fill = genoa, alpha = 0.7) +
      scale_fill_identity() +
      scale_y_discrete() +
      scale_x_continuous(label = scales::label_number(scale_cut = scales::cut_short_scale()),
                         expand = c(.005, .005)) +
      labs(x = NULL, y = NULL,
           title = "COP23 TX_CURR Targets",
           subtitle = "",
           caption = glue::glue(
             "Source: COP 23 PSNUxIM tab April 14 2023 ", .sep = " | ")) +
      si_style_ygrid() +
      theme(legend.position = "none",
            strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13))
    
    cop_22 <- msd_2022 %>%
      ggplot(aes(y = psnu)) +
      geom_col(aes(x = as.numeric(targets)), fill = genoa, alpha = 0.7) +
      scale_fill_identity() +
      scale_y_discrete() +
      scale_x_continuous(label = scales::label_number(scale_cut = scales::cut_short_scale()),
                         expand = c(.005, .005)) +
      labs(x = NULL, y = NULL,
           title = "COP22 TX_CURR Targets",
           subtitle = "",
           caption = glue::glue(
             "Source: FY23Q1c MSD",
             "USAID SI Analytics, ref id = {ref_id}", .sep = " | ")) +
      si_style_ygrid() +
      theme(legend.position = "none",
            strip.text.x = element_text(family = "Source Sans Pro SemiBold", size = 13))
    
    
    cowplot::plot_grid(cop_22,
                       cop_23, 
                       ncol = 2, 
                       align = "hv", axis = "bt", rel_heights = c(1, 1))
    
    si_save("Images/COP_22_23_TX_CURR_targets.png")
    
# using tameDP to combine and look at targets across time ----------------------
    
    msd_final <- align_msd_disagg(msd_path = psnu_path, dp_path = tst_path) %>%
      align_ageband(collapse = TRUE)
    
    df_msd_tst <- msd_final %>%
      bind_rows(tst_df)
    
    # spot check with combination method above 
    # note that this will result in 2 rows:
    # one from DATIM and one from the TST
    # where the targets from DATIM are not blank but the one
    # from the TST is even though 'combined' has a value for this
    
    spot <- df_msd_tst %>%
      filter(psnu == "Ezo County", 
             fiscal_year == "2022", 
             indicator == "TX_CURR", 
             ageasentered == "01-09", 
             sex == "Female")
    
    #ask KS about this     

    