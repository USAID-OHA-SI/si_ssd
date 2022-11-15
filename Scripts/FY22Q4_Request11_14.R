# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Complete request for visuals from 2022-11-14
# REF ID:   8d2cb761 
# LICENSE:  MIT
# DATE:     2022-11-14
# UPDATED:  2022-11-15

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
  library(cascade)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "8d2cb761"
  
  peds <- c("<01", "01-04", "05-09", "10-14", "<15")
  
  # metadata
  ou_path <- si_path() %>% 
    return_latest("Genie_OU_IM_South_Sudan_Daily")
  
  get_metadata(ou_path)

# IMPORT -----------------------------------------------------------------------
  
  # Nat SubNat
  
  nat_subnat_df <- si_path() %>%
    return_latest("NAT_SUBNAT_FY15-23") %>%
    read_msd()
  
  # OU
  ou_df <- si_path() %>% 
    return_latest("Genie_OU_IM_South_Sudan_Daily") %>%
    read_msd()
  
  # PSNU 
  psnu_df <- si_path() %>% 
    return_latest("Genie_PSNU_IM_South_Sudan_Daily") %>%
    read_msd()
  
  # Site
  site_df <- si_path() %>% 
    return_latest("Genie_SITE_IM_South_Sudan_Daily") %>%
    read_msd()
  
  # time period data comes from OUxIM but should be the same for all levels
  full_pds <- (min(ou_df$fiscal_year) - 1) %>% 
    paste0("-10-01") %>% 
    as_date() %>% 
    seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>% 
    convert_date_to_qtr()
  
  pd_prior <- (convert_qtr_to_date(metadata$curr_pd) - months(3)) %>% 
    convert_date_to_qtr()

# NAT SUBNAT ----------------------------------------------------------------------
  
  # ● FY22Q4 PLHIV in need of ART in PEPFAR supported Countries 
  
  df_nat <- nat_subnat_df %>%
    filter(indicator %in% c("TX_CURR_SUBNAT", "PLHIV"),
           fiscal_year == 2022,
           standardizeddisaggregate == "Age/Sex/HIVStatus") %>%
    group_by(fiscal_year, indicator) %>% 
    summarise(across(targets, sum, na.rm = TRUE)) %>% 
    pivot_wider(names_from = indicator, values_from = targets) %>% 
    mutate(gap = TX_CURR_SUBNAT/PLHIV, 
           pct = percent(gap),
           need_label = comma(PLHIV - TX_CURR_SUBNAT), 
           PLHIV_label = comma(PLHIV), 
           TX_CURR_SUBNAT_label = comma(TX_CURR_SUBNAT))
  
  unmetneed_pct <- df_nat %>%
    ungroup() %>%
    select(pct) %>%
    pull()
  
  unmetneed_num <- df_nat %>%
    ungroup() %>%
    select(need_label) %>%
    pull()

# OU x IM ----------------------------------------------------------------------
  
  # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) at OU DATIM data
  
  df_nn <- ou_df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           fiscal_year == "2022", 
           funding_agency == "USAID") %>%
    pluck_totals() %>% 
    group_by(country, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", scooter, scooter_light),
           indicator = glue("{toupper(indicator)}"),
           share = value / tx_curr)
  
  df_nn %>%
    filter(tx_curr != 0) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~fct_reorder2(country, period, tx_curr),
               scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_manual(values = c("TX_NEW" = scooter,
                                 "TX_NET_NEW" = scooter_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("GAP BETWEEN <span style='color:{scooter_light}'>NET_NEW</span> AND <span style='color:{scooter}'>TX_NEW</span> NARROWING OVER FY22"),
         subtitle = glue("Substantial Drop in Q3 Likely Tempered Progress"),
         caption = glue("Source: {metadata$caption} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_tx-new-nn.png"),
          scale = 1.1)  
  
  # ● IIT and return to care (TX_RTT) Trends at OU
 
  df_iit_rtt <- ou_df %>% 
    filter(funding_agency == "USAID", 
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
           fiscal_year == "2022") %>%
    pluck_totals() %>%
    group_by(fiscal_year, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_rtt <- df_iit_rtt %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  vct_itt_cntry <- ou_df %>% 
    filter(funding_agency == "USAID", 
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
           fiscal_year == "2022") %>%
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
  
  df_iit_rtt %>%
    mutate(fiscal_year = str_sub(period, end = 4)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_point(position = position_jitter(width = .2, seed = 42),
               na.rm = TRUE, color = denim,
               alpha = .2) +
    geom_smooth(aes(weight = tx_curr_lag1, group = country),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                linewidth = 1.5, color = golden_sand) +
    scale_size(label = comma, guide = NULL) +
    scale_y_continuous(limits = c(0,.15),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("USAID MAINTAINED A RELATIVELY CONSTANT {vct_itt_cntry} IIT OVER FY22"),
         subtitle = glue("A 1% increase Occured Q3 with a return to Q1 levels by Q4 
                    while <span style='color:{denim}'>TX_CURR_LAG1</span> increased by approx. 1.6k people over the year"),
         caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
                        {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_OU_iit.png"),
          scale = 1.5)  
  
  df_iit_rtt %>%
    mutate(fiscal_year = str_sub(period, end = 4)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, tx_rtt)) +
    geom_col(fill = "#2F2E6F", 
             alpha = .75,
             position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("DESPITE A BRIEF INCREASE IN IIT IN Q3, <span style='color:#2F2E6F'>TX_RTT</span> CONTINUED TO INCREASE OVER FY22"),
         caption = glue("Source: {metadata$caption} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_OU_rtt.png"),
          scale = 1.5)  
  
  # ● IIT by age and sex at OU
  
  df_iit <- ou_df %>%
  filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", 
                          "TX_CURR_Lag1", "TX_RTT", "TX_NET_NEW"), 
         standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"), 
         fiscal_year == "2022", 
         funding_agency == "USAID") %>% 
    mutate(
      age_cat = case_when(
        ageasentered %in% c("<01") ~ "<01", 
        ageasentered %in% c("01-04", "05-09") ~ "01-09",
        ageasentered %in% c("10-14", "15-19") ~ "10-19",
        ageasentered %in% c("20-24", "25-29") ~ "20-29",
        ageasentered %in% c("30-34", "35-39") ~ "30-39",
        ageasentered %in% c("40-44", "45-49") ~ "40-49", 
        ageasentered %in% c("50-54", "55-59") ~ "50-59",
        ageasentered %in% c("60-64", "65+") ~ "60+")) %>%
    group_by(fiscal_year, indicator, age_cat, sex) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() 
  
  df_iit <- df_iit %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_iit %>%
    filter(tx_curr_lag1 != 0,
           tx_curr != 0,
           is.na(iit) == FALSE) %>%
    ggplot(aes(period, iit, group = sex, color = sex)) +
    geom_smooth(aes(weight = as.numeric(tx_curr_lag1),
                    group = sex, color = sex),
                method = "loess",formula = "y ~ x", 
                se = FALSE, na.rm = TRUE, linewidth = 1.5) +
    facet_wrap(~age_cat, ncol = 3, nrow = 3) +
    scale_y_continuous(limits = c(0,.15),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = c(moody_blue_light, genoa_light), 
                       label = NULL) +
     labs(x = NULL, y = NULL,
          title = glue("The increase in Q3 IIT had disparate impacts by age group and sex") %>% toupper,
          subtitle = glue("<span style='color:{moody_blue_light}'>Females</span> in most age groups continue to experience increased IIT into Q4<br>
                           <span style='color:{genoa_light}'>Males</span> in all age groups experienced a decrease in IIT from Q3 to Q4"),
          caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
                         {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown(),
          legend.position="none")
  
  si_save(paste0(metadata$curr_pd, "_SSD_OU_age_sex_iit.png"),
          path = "Images",
          scale = 1.5)
  
  # ● IIT and return to care (TX_RTT) Trends Partner
  
  df_iit_partner <- ou_df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW"), 
           fiscal_year == "2022") %>%
    pluck_totals() %>%
    group_by(fiscal_year, prime_partner_name, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_partner <- df_iit_partner %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new,
           prime_partner_name = if_else(prime_partner_name == "Trustees Of Columbia University In The City Of New York", 
                                        "Columbia Corporation", prime_partner_name)) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_partner_lab <- df_iit_partner %>% 
    filter(period == metadata$curr_pd) %>% 
    count(prime_partner_name, wt = tx_curr, sort = TRUE) %>% 
    mutate(partner_lab = ifelse(n == max(n), 
                       glue("{prime_partner_name} - {label_number(1, scale_cut = cut_short_scale())(n)} [TX_CURR {pd_prior}]"),
                       glue("{prime_partner_name} - {label_number(1, scale_cut = cut_short_scale())(n)}"))) %>%
    select(-n)
  
  partner_tx_order <- df_iit_partner %>% 
    filter(period == metadata$curr_pd) %>% 
    count(prime_partner_name, wt = tx_curr, sort = TRUE) %>% 
    pull(prime_partner_name)
  
  vct_itt_partner <- ou_df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW"),
           fiscal_year == "2022") %>%
    pluck_totals() %>%
    group_by(fiscal_year, prime_partner_name, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    filter(period == metadata$curr_pd) %>% 
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pull() %>% 
    percent()
  
  df_iit_partner %>%
    left_join(df_partner_lab, by = "prime_partner_name") %>% 
    mutate(partner_lab_lab = factor(partner_lab, df_partner_lab$partner_lab),
           fiscal_year = str_sub(period, end = 4)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, iit)) +
    geom_smooth(aes(weight = tx_curr_lag1, group = partner_lab, 
                    color = prime_partner_name),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5) +
    facet_wrap(~partner_lab) +
    scale_size(label = comma, guide = NULL) +
    scale_y_continuous(limits = c(0,.15),
                       label = percent_format(1),
                       oob = oob_squish) +
    scale_color_manual(values = c(scooter_light, denim, genoa_light)) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("USAID AND CDC PARTNERS EXPERIENCED AN INCREASE IN Q3 IIT AND DECREASE IN Q4 
                      WHILE DOD PARTNER SAW THE OPPOSITE"),
         subtitle = "OU IIT rates by Partner Name",
         caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 25%
                        {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown(), 
          legend.position = "none")
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_partner_iit.png"),
          scale = 1.5)  
  
  # RTT by partner
  
  df_iit_rtt_partner <- ou_df %>%
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"),
           fiscal_year == "2022") %>%
    pluck_totals() %>%
    group_by(fiscal_year, prime_partner_name, indicator) %>%
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    reshape_msd(include_type = FALSE) %>%
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")

  df_iit_rtt_partner <- df_iit_rtt_partner %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new,
           prime_partner_name = if_else(prime_partner_name == "Trustees Of Columbia University In The City Of New York", 
                                        "Columbia Corporation", prime_partner_name)) %>%
    rowwise() %>%
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>%
    ungroup()

   df_iit_rtt_partner %>%
     left_join(df_partner_lab, by = "prime_partner_name") %>% 
     mutate(partner_lab_lab = factor(partner_lab, df_partner_lab$partner_lab),
            fiscal_year = str_sub(period, end = 4)) %>% 
     filter(tx_curr_lag1 != 0) %>%
     ggplot(aes(period, tx_rtt, fill = prime_partner_name)) +
     geom_col(alpha = .75,
              position = position_dodge(width = 1)) +
     facet_grid(~partner_lab) +
     scale_fill_manual(values = c(scooter_light, denim, genoa_light)) +
     scale_y_continuous(limits = c(0, 2000), 
                        label = label_number(scale_cut = cut_short_scale())) +
     labs(x = NULL, y = NULL, fill = NULL,
          title = glue("All Partners saw an increase in TX_RTT from Q1") %>% toupper,
          subtitle = glue("CDC Partner Columbia Corporation also saw an increase in RTT in Q2 
                          while USAID and DOD Partners did not see a similar Q2 increase"),
          caption = glue("Source: {metadata$caption} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())

   si_save(glue("Images/{metadata$curr_pd}_SSD_partner_rtt.png"),
           scale = 1.5)  
 
  # ● OU FY22Q4 Target performance for KP and Priority Population
   plot_name
   
   # kp data
   kp_cascade_data <- return_cascade(ou_df %>%
                                       filter(fiscal_year == "2022"), 13) %>%
     mutate(population = "KP")
   
   # priority populations data
   # peds
   peds_cascade_data <- return_cascade(ou_df %>%
                                       filter(fiscal_year == "2022"), 4) %>%
     mutate(population = "peds")
   # AGYW
   aypf_cascade_data <- return_cascade(ou_df %>%
                                         filter(fiscal_year == "2022"), 8)%>%
     mutate(population = "AGYW")

  # ● FSW and KP Clients HIV Testing quarterly Trend
  
# PSNUxIM ----------------------------------------------------------------------
  
  # TX-CURR Target Performance by SNU County/ military
  
  psnu_df_curr <-  psnu_df %>%
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
  # ● IIT and return to care (TX_RTT) Trends 2 sites with highest IIT
  # ● KP Target performance by Towns (Reach and Testing)
  # ● KP and Priority Population HIV Testing and Yield by Towns
  
  