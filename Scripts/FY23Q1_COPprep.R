# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  COP22 viz
# REF ID:   5eedfd8a 
# LICENSE:  MIT
# DATE:     2023-01-24
# Notes: adapted from 
#         Treatment/6MMD: agitprop/29_treat_qtr_mmd-usaid.R
#         VLC/VLS: catch-22/20211018_USAID_VLC-by-country.R

# DEPENDENCIES ----------------------------------------------------------------

  source("Scripts/COP_funcs.R")

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "5eedfd8a"
  ou_path <- "OU_IM_South_Sudan"
  psnu_path <- "Genie_PSNU_IM_South_Sudan"
  authors <- "Jessica Hoehner"

# IMPORT ----------------------------------------------------------------------
  
  ou_df <- si_path() %>%
    return_latest(ou_path) %>%
    read_psd()
  
  get_metadata(type = "OU_IM")
  metadata_msd_ou <- metadata
  rm(metadata)
  
  psnu_df <- si_path() %>%
    return_latest(psnu_path) %>%
    read_psd()
  
  get_metadata(type = "PSNU_IM")
  metadata_msd_psnu <- metadata
  rm(metadata)
  
  df_nat <- si_path() %>% 
    return_latest("NAT") %>% 
    read_psd()
  
  get_metadata(type = "NAT_SUBNAT")
  metadata_natsubnat <- metadata
  rm(metadata)

# MUNGE -----------------------------------------------------------------------
  
  # Treatment --------------------------------
  # How many patients on 6MMD across PEPFAR?
  
  df_mmd <- ou_df %>% 
    filter(indicator == "TX_CURR",
           fiscal_year >= 2021,
           standardizeddisaggregate %in% 
             c("Total Numerator", "Age/Sex/ARVDispense/HIVStatus")) %>% 
    mutate(otherdisaggregate = case_when(is.na(otherdisaggregate) ~ "total",
                               TRUE ~ str_remove(otherdisaggregate, 
                                       "ARV Dispensing Quantity - "))) %>%
    clean_mechs() %>%
    clean_agency() %>%
    # add in agency agg and reshape
    group_by(fiscal_year, indicator, otherdisaggregate) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd() %>% 
    filter(value > 0) %>%
    # create group for o3mo and o6mo via reshaping for plotting
    select(-period_type) %>% 
    pivot_wider(names_from = otherdisaggregate) %>% 
    rowwise() %>% 
    mutate(
      #unknown = total - sum(`Less than 3 months`, `3 to 5 months`, `6 or more months`, na.rm = TRUE),
      #unknown = ifelse(unknown < 0, 0, unknown),
      o3mmd = sum(`3 to 5 months`, `6 or more months`, na.rm = TRUE)) %>%
    ungroup() %>% 
    rename(o6mmd = `6 or more months`) %>% 
    select(-`Less than 3 months`, -`3 to 5 months`) %>% 
    pivot_longer(-c(period, indicator, total), 
                 names_to = "otherdisaggregate",
                 values_to = "tx_mmd") %>% 
    rename(tx_curr = total) %>%
    group_by(period, otherdisaggregate) %>%
    arrange(otherdisaggregate, period) %>% 
    mutate(otherdisaggregate = recode(otherdisaggregate,
                                      "o3mmd" = "MMD - 3 months or more",
                                      "o6mmd" = "MMD - 6 months or more"),
           share = tx_mmd / tx_curr, 
           bar_color = ifelse(otherdisaggregate == "MMD - 6 months or more", scooter, scooter_light),
           otherdisaggregate_md = glue("<span style='color:{bar_color}'>{otherdisaggregate}</span>"),
           lab_max = case_when(period == max(period) ~ share),
           lab_other = case_when(period != max(period) ~ share))
  
  
  # PLHIV reached by PEPFAR in SSD on ART ---------------
  
  # PLHIV and unmet need for treatment
  # country-wide
  df_nat_ssd <- df_nat %>%
    filter(operatingunit == "South Sudan",
           fiscal_year %in% c(2023), 
           indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(operatingunit, fiscal_year, trendscoarse, indicator) %>%
    summarise(across(targets, sum, na.rm = TRUE)) %>%
    pivot_wider(names_from = indicator, values_from = targets) %>%
    mutate(
      fiscal_year = as.character(fiscal_year),
      gap = TX_CURR_SUBNAT / PLHIV,
      gap_pct = percent(gap),
      need_label = comma(PLHIV - TX_CURR_SUBNAT),
      PLHIV_label = comma(PLHIV),
      TX_CURR_SUBNAT_label = comma(TX_CURR_SUBNAT)) %>%
    drop_na(TX_CURR_SUBNAT)
  
  # SNU level
   df_nat_snu <- df_nat %>%
     filter(operatingunit == "South Sudan",
            fiscal_year %in% c(2023),  
            indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
            standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
     group_by(fiscal_year, snu1, trendscoarse, indicator) %>%
     summarise(across(targets, sum, na.rm = TRUE)) %>%
     pivot_wider(names_from = indicator, values_from = targets) %>%
     mutate(
       fiscal_year = as.character(fiscal_year),
       gap = TX_CURR_SUBNAT / PLHIV,
       gap_pct = percent(gap),
       need_label = comma(PLHIV - TX_CURR_SUBNAT),
       PLHIV_label = comma(PLHIV),
       TX_CURR_SUBNAT_label = comma(TX_CURR_SUBNAT)) %>%
     drop_na(TX_CURR_SUBNAT)
  
   
  # OVC --------------------------------------
  # how many people receive OVC services
  # what percentage of OVC beneficiaries <18 know their status?
  # what percentage of OVCLHIV are on ART?

  # What is the growth of TX_CURR since COP22?----------
  # How has case-finding changed since COP22?-----------
  
  # How has VL testing coverage/VLS (3rd 95) changed since COP22?---------
  # - specific populations
  # - infant (2 months) diagnosis (HTS_TST_POS?)
   
   df_vls <- psnu_df %>% 
     filter(fiscal_year == "2022",
            indicator %in% c("TX_CURR", "TX_PVLS", "TX_CURR_Lag2"),
            standardizeddisaggregate %in% c("Total Numerator", "Total Denominator")) %>%
     clean_indicator() %>% 
     group_by(fiscal_year, snu1, indicator) %>% 
     summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
     reshape_msd() %>%
     select(-period_type) %>%
     pivot_wider(names_from = indicator,
                 names_glue = "{tolower(indicator)}") %>% 
     filter(tx_pvls_d > 0) %>% 
     mutate(
       fiscal_year = if_else(stringr::str_detect(period, "22"), 2022, 0)) %>%
     group_by(fiscal_year, period, snu1) %>%
      mutate(
        vlc = tx_pvls_d/tx_curr_lag2,
        vls = tx_pvls/tx_pvls_d,
        snu1 = stringr::str_replace(snu1, "State", ""),
            snu1 = recode(snu1, 
                          "_Military South Sudan" = "Military"),
            vlc_nat_avg = if_else(period == metadata$curr_pd, 
                                  round(mean(df_vls$vlc), 2), 0.0), 
            vls_nat_avg = if_else(period == metadata$curr_pd, 
                                 round(mean(df_vls$vls), 2), 0.0),
            vls_alt = tx_pvls/tx_curr_lag2,
            vls_goal_gap = round(.9*tx_pvls_d, 0) - tx_pvls,
            vls_goal_gap = ifelse(vls_goal_gap < 0, 0, vls_goal_gap),
            fill_color = ifelse(vlc > 0.56, scooter, scooter_light), 
            color_bar = ifelse(period == metadata$curr_pd, scooter, trolley_grey_light))
   
   # VLC/VLS by SNU and age
   # needs psnu df, cntry, pd_hist = 5
   df_vlcs_snu_age <- prep_viral_load_snu_age(psnu_df, "South Sudan")
   
   #identify where 80% of TX_CURR is for viz facet
   df_grps_adj <- df_vls_grp %>% 
     count(snu1, mech_name, wt = tx_curr) %>% 
     arrange(desc(n)) %>% 
     mutate(cumsum = cumsum(n),
            share = cumsum/sum(n))
   
   # Treatment Coverage Gaps and Treatment Initiations
   
   # needs MSD and country 
   df_nat_curr <- prep_txcoverage_age_sex(df_nat, "South Sudan")
   
   # needs MSD, country
   df_msd_netnew <- prep_txnetnew_age_sex(psnu_df, "South Sudan")
   
   # Adapted from hardapoart
   # needs MSD and country 
   # vlc/s gaps between different population groups by PSNU
   df_msd_kp_agyw <- prep_viral_load_kp_agyw(psnu_df, "South Sudan")
   
   # requested to keep only AGYW/non-AGYW
   df_msd_AGYW <- df_msd_kp_agyw %>%
     filter(group == "AGYW")

# VIZ --------------------------------------------------------------------------
  
  # PEPFAR-wide Trend in 6MMD ------------------------------
  df_mmd %>% 
    filter(otherdisaggregate == "MMD - 6 months or more") %>%
    ggplot(aes(period, tx_mmd)) + 
    geom_col(aes(y = tx_curr), fill = trolley_grey_light, alpha = .5) +
    geom_col(aes(fill = bar_color)) +
    # geom_text(aes(label = percent(lab_other, 1)), vjust = 1.2, na.rm = TRUE,
    #           family = "Source Sans Pro SemiBold", color = "white") +
    geom_text(aes(label = percent(lab_max, 1)), vjust = -2, na.rm  = TRUE,
              family = "Source Sans Pro SemiBold", color = matterhorn) +
    geom_text(aes(label = comma(tx_mmd)), na.rm  = TRUE,
              y = 2000, family = "Source Sans Pro SemiBold", color = "white") +
    geom_errorbar(aes(ymax = tx_curr, ymin = tx_curr), color = trolley_grey) +
    # facet_wrap(~otherdisaggregate) +
    facet_wrap(~otherdisaggregate_md, nrow = 1, ncol = 3) +
    scale_x_discrete(breaks = c("FY20Q2", "FY20Q4", "FY21Q2", "FY21Q4", "FY22Q2", "FY22Q4")) +
    scale_fill_identity() +
    scale_y_continuous(labels = label_number(scale_cut = cut_si("unit")),
                       position = "right", expand = c(.005, .005), 
                       limits = c(0, 50000)) +
    labs(x = NULL, y = NULL,
         title = "Strong 6 month multi-month dispensing (MMD) rates into COP23, even more patients reached",
         subtitle = "[Context on dips in FY22Q3]",
         caption = glue("Source: {metadata$source} | SI Analytics | Ref ID: {ref_id}")) +
    si_style_ygrid() +
    theme(legend.position = "none", 
          strip.text.x = element_markdown(family = "Source Sans Pro SemiBold", size = 13))

  # How many PLHIV reached by PEPFAR in SSD are on ART and 
  # how has the unmet need changed since 2021?
  
  # PLHIV and unmet need for treatment
 
  # KP cascade -------------------------------

   cascade::return_cascade_plot(psnu_df)
   
   return_cascade(psnu_df, 1)
   
   si_save(glue("Images/{metadata$curr_pd}_SSD_GP_cascade.png"),
           scale = 1.3)  

  # VL coverage/VLS (3rd 95)

   # Adapted from hardapoart
   # Viz TX VLC/S Gaps by PSNU
   # needs df from  prep_viral_load_kp_agyw
   viz_viral_load_kp_agyw(df_msd_AGYW)
   
   si_save(glue("Images/SSD_VLC_VLS_gaps_AGYW-nonAGYW.png"),
           scale = 1.3) 
   
   # Adapted from hardapoart
   # VLC/S by age and SNU
   # needs df from prep_viral_load_snu_age and can save T/F
   viz_viral_load_snu(df_vlcs_snu_age, save = T)
   
   si_save(glue("Images/SSD_VLC_VLS_age_snu.png"),
           scale = 1.3) 
 