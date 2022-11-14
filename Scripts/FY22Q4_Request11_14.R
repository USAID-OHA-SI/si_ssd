# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Complete request for visuals from 2022-11-14
# REF ID:   8d2cb761 
# LICENSE:  MIT
# DATE:     2022-11-14
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(lubridate)
  library(gagglr)
  library(glue)
  library(scales)
  library(extrafont)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(ggrepel)
  library(janitor)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "8d2cb761"
  
  # OU
  ou_path <- "Data/Genie_OU_IM_South_Sudan_Daily_2022-11-09.zip"
  
  # PSNU 
  psnu_path <- "Data/Genie_PSNU_IM_South_Sudan_Daily_2022-11-14.zip"
   
  # Site
  site_path <- "Data/Genie_SITE_IM_South_Sudan_Daily_2022-11-14.zip"
  
  peds <- c("<01", "01-04", "05-09", "10-14", "<15")
  
  # periods
  
  get_metadata(ou_path)


# IMPORT -----------------------------------------------------------------------
  
  ou_df <- read_msd(ou_path) 
  
  psnu_df <- read_msd(psnu_path)  
  
  site_df <- read_msd(site_path) 
  
  # time period data comes from OUxIM but should be the same for all levels
  full_pds <- (min(ou_df$fiscal_year) - 1) %>%
    paste0("-10-01") %>%
    as_date() %>%
    seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>%
    convert_date_to_qtr()
  
  pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")
  
  pd_prior <- (convert_qtr_to_date(metadata$curr_pd) - months(3)) %>%
    convert_date_to_qtr()

# OU x IM ----------------------------------------------------------------------
  
  ou_df_filt <-  ou_df %>%
    filter(indicator %in% c("TX_CURR", "TX_ML", "TX_NEW", "TX_NET_NEW"))
  
  # ● FY22Q4 PLHIV in need of ART in PEPFAR supported Countries (all funding agencies)
  
  # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) at OU DATIM data)
  
  # ● IIT by age and sex at OU
  
  df_iit <- ou_df_filt %>% 
    filter(funding_agency == "USAID") %>%
    pluck_totals() %>%
    group_by(fiscal_year, operatingunit, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit <- df_iit %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()

  vct_itt_cntry <- ou_df_filt %>% 
    pluck_totals() %>%
    group_by(fiscal_year, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    filter(period == metadata$curr_pd) %>% 
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(
      iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pull() %>% 
    percent()
  
  df_iit %>%
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = scooter,
               alpha = .2) +
    geom_smooth(aes(weight = as.numeric(tx_curr_lag1), group = operatingunit),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                linewidth = 1.5, color = golden_sand) +
    scale_size(label = comma, guide = NULL) +
    scale_x_discrete(labels = full_pds) +
    scale_y_continuous(limits = c(0,.15),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("USAID ENDED THE YEAR WITH IIT of {vct_itt_cntry}, trending downwards from Q3") %>% toupper,
         subtitle = "IIT increased to by 1% in Q3 but reached Q1 levels again by Q4",
         caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 25%
                        {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(paste0(metadata$curr_pd, "_SSD_OU_iit.png"),
          path = "Images",
          scale = 1.5)
  
# PSNUxIM ----------------------------------------------------------------------
  
  snu_df_filt <-  snu_df %>%
    filter(indicator %in% c("TX_CURR", "TX_RTT", "TX_NEW", "TX_ML", "TX_NET_NEW"))
  
  # TX-CURR Target Performance by SNU County/ military
  
  snu_df_curr <-  snu_df_filt %>%
    filter(
      fiscal_year == metadata$curr_fy,
      !funding_agency %in% c("Dedup", "HHS/CDC"),
      indicator == "TX_CURR",
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
        (standardizeddisaggregate == "Total Numerator")) %>%
    mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                         "Total", "Peds")) %>%
    group_by(fiscal_year, snu1, indicator, type) %>%
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd("quarters") %>%
    select(-results_cumulative) %>%
    arrange(type, snu1, period)
  
  snu_df_curr <- snu_df_curr %>%
    mutate(
      growth_rate_req =
        case_when(period == metadata$curr_pd ~
                    ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
    group_by(type, snu1) %>%
    fill(growth_rate_req, .direction = "updown") %>%
    mutate(
      growth_rate = (results / lag(results, order_by = period)) - 1,
      growth_rate = na_if(growth_rate, Inf)) %>%
    ungroup() %>%
    mutate(
      geo_gr_lab = case_when(
        is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"), # metadata$curr_qtr == 4 | is.infinite(growth_rate_req)
        growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
        TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
      gr_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate, 1)),
      gr_label_position = 0,
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
      amount_diff = targets - results, 
      pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
  
  df_achv_curr <- snu_df_curr %>%
    filter(period == metadata$curr_pd) %>%
    count(type, results >= targets) %>%
    filter(`results >= targets` == TRUE)
  
  pct_change_curr <- snu_df_curr %>%
    filter(type == "Total") %>%
    select(pct_change) %>%
    filter(pct_change == max(as.numeric(pct_change))) %>%
    pull()
      
  snu_df_curr %>%
    filter(type == "Total") %>%
    mutate(
      snu_label = case_when(
        snu1 == "Western Equatoria State" ~ "Western Equatoria",
        snu1 == "Central Equatoria State" ~ "Central Equatoria",
        snu1 == "_Military South Sudan" ~ "Military")) %>%
    ggplot(aes(period, results, fill = as.character(period))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), 
                  linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = results, y = gr_label_position),
               family = "Source Sans Pro", color = "white", size = 9 / .pt,
               vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(snu_label, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(snu_df_curr$period)[grep("Q(4)", unique(snu_df_curr$period))]) +
    scale_fill_manual(values = c(scooter_light, scooter_light, scooter_light, scooter,
                                 scooter_light, scooter_light, scooter_light, scooter,
                                 scooter_light, scooter_light, scooter_light, scooter)) +
    labs(
      x = NULL, y = NULL,
      title = glue("Central Equatoria State has continued to exceed Annual Treatment Targets Since Q2") %>% toupper(),
      subtitle = "Military facilities have made steady progress while coverage in facilities in Western Equatoria State has remained stagnant",
      caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(
      legend.position = "none",
      panel.spacing = unit(.5, "picas"),
      axis.text.x = element_text(size = 8))
  
  si_save(paste0(metadata$curr_pd, "_SSD-_tx-curr-targets_snu.png"),
          path = "Images",
          scale = 1.5)
  
  # ● TX-NEW Target Performance by SNU County/ military (FY22 Q4 cumulative for TX_NEW)

  snu_df_new <-  snu_df_filt %>%
    filter(
      fiscal_year == metadata$curr_fy,
      !funding_agency %in% c("Dedup", "HHS/CDC"),
      indicator == "TX_NEW",
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
        (standardizeddisaggregate == "Total Numerator")) %>%
    mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                         "Total", "Peds")) %>%
    group_by(fiscal_year, snu1, indicator, type) %>%
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd("quarters") %>%
    select(-results_cumulative) %>%
    arrange(type, snu1, period)
  
  snu_df_new <- snu_df_new %>%
    mutate(
      growth_rate_req =
        case_when(period == metadata$curr_pd ~
                    ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
    group_by(type, snu1) %>%
    fill(growth_rate_req, .direction = "updown") %>%
    mutate(
      growth_rate = (results / lag(results, order_by = period)) - 1,
      growth_rate = na_if(growth_rate, Inf)) %>%
    ungroup() %>%
    mutate(
      geo_gr_lab = case_when(
        is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"), # metadata$curr_qtr == 4 | is.infinite(growth_rate_req)
        growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
        TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
      gr_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate, 1)),
      gr_label_position = 0,
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
      amount_diff = targets - results, 
      pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
  
  df_achv_new <- snu_df_new %>%
    filter(period == metadata$curr_pd) %>%
    count(type, results >= targets) %>%
    filter(`results >= targets` == TRUE)
  
  pct_change_new <- snu_df_new %>%
    filter(type == "Total") %>%
    select(pct_change) %>%
    filter(pct_change == max(as.numeric(pct_change))) %>%
    pull()
  
  snu_df_new %>%
    filter(type == "Total") %>%
    mutate(
      snu_label = case_when(
        snu1 == "Western Equatoria State" ~ "Western Equatoria",
        snu1 == "Central Equatoria State" ~ "Central Equatoria",
        snu1 == "_Military South Sudan" ~ "Military")) %>%
    ggplot(aes(period, results, fill = as.character(period))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), 
                  linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = results, y = gr_label_position),
              family = "Source Sans Pro", color = "white", size = 9 / .pt,
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(snu_label, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(snu_df_new$period)[grep("Q(4)", unique(snu_df_new$period))]) +
    scale_fill_manual(values = c(moody_blue_light, moody_blue_light, moody_blue_light, moody_blue,
                                 moody_blue_light, moody_blue_light, moody_blue_light, moody_blue,
                                 moody_blue_light, moody_blue_light, moody_blue_light, moody_blue)) +
    labs(
      x = NULL, y = NULL,
      title = glue("Performance in Annual New Enrollment Targets Has Fallen Since Q2 for all SNUs") %>% toupper(),
      subtitle = "Central Equatoria State and Military facilities saw drops in Q4 while Western Equatoria State increased by 1",
      caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(
      legend.position = "none",
      panel.spacing = unit(.5, "picas"),
      axis.text.x = element_text(size = 8))
  
  si_save(paste0(metadata$curr_pd, "_SSD-_tx-new-targets_snu.png"),
          path = "Images",
          scale = 1.5)
  
  # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) by State
  
  
  
  
# SitexIM ----------------------------------------------------------------------
  
  # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) for sites contributing to 80% of IIT
  # ● IIT by age and sex for two facilities with highest IIT
  # ● IIT and return to care (TX_RTT) Trends at OU, Partner and 2 sites with highest IIT
  
  