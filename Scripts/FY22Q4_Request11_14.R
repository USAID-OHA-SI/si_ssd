# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Complete request for visuals from 2022-11-14, used to make 
#           SSD_FY22Q4DataReview.pptx
# REF ID:   8d2cb761 
# LICENSE:  MIT
# DATE:     2022-11-14
# UPDATED:  2022-11-17

# DEPENDENCIES -----------------------------------------------------------------
  
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
    return_latest("OU_IM_South_Sudan")
  
  get_metadata(ou_path)

# IMPORT -----------------------------------------------------------------------
  
  # Nat SubNat
  
  nat_subnat_df <- si_path() %>%
    return_latest("NAT_SUBNAT_FY15-23") %>%
    read_msd()
  
  # OU
  ou_df <- si_path() %>% 
    return_latest("OU_IM_FY20-23") %>%
    read_msd()
  
  # PSNU 
  snu1df <- si_path() %>% 
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
  
  # TX-CURR Target Performance by SNU County/ military
  
  ou_df_curr <-  ou_df %>%
    filter(
      operatingunit = "South Sudan",
      fiscal_year == metadata$curr_fy,
      indicator == "TX_CURR",
      (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
        (standardizeddisaggregate == "Total Numerator")) %>%
    mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                         "Total", "Peds")) %>%
    group_by(fiscal_year, country, indicator, type) %>%
    summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd("quarters") %>%
    select(-results_cumulative) %>%
    arrange(type, country, period)
  
  ou_df_curr <- ou_df_curr %>%
    mutate(
      growth_rate_req =
        case_when(period == metadata$curr_pd ~
                    ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
    group_by(type) %>%
    fill(growth_rate_req, .direction = "updown") %>%
    mutate(
      growth_rate = (results / lag(results, order_by = period)) - 1,
      growth_rate = na_if(growth_rate, Inf)) %>%
    ungroup() %>%
    mutate(
      geo_gr_lab = case_when(
        is.infinite(growth_rate_req) ~ glue("{toupper(country)}"),
        growth_rate_req < 0 ~ glue("{toupper(country)}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{toupper(country)}\n{percent(growth_rate_req, 1)}"),
        TRUE ~ glue("{toupper(country)}\n{percent(growth_rate_req, 1)}")),
      gr_lab = case_when(fiscal_year == metadata$curr_fy ~ 
                           glue("{percent(growth_rate, 1)} ({comma(results)})")),
      gr_lab = stringr::str_replace(gr_lab, "NA", "0"),
      gr_label_position = 7300,
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
      amount_diff = targets - results, 
      pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
  
  # percentage change from q1 to q4
  oupct_change_curr <- ou_df_curr %>%
    filter(type == "Total") %>%
    select(pct_change) %>%
    filter(pct_change == max(as.numeric(pct_change))) %>%
    pull()
  
  ou_df_curr %>%
    filter(type == "Total") %>%
    ggplot(aes(period, results, fill = as.character(period))) +
    geom_col(aes(y = targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), 
                  linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = gr_lab, y = gr_label_position),
              family = "Source Sans Pro", color = "white", size = 9 / .pt,
              vjust = -.5, na.rm = TRUE) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(ou_df_curr$period)[grep("Q(4)", unique(ou_df_curr$period))]) +
    scale_fill_manual(values = c(scooter_light, scooter_light, scooter_light, scooter)) +
    labs(
      x = NULL, y = NULL,
      title = glue("aaabbbccc") %>% toupper(),
      subtitle = "xxxyyy",
      caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(
      legend.position = "none",
      panel.spacing = unit(.5, "picas"),
      axis.text.x = element_text(size = 8))
  
  si_save(paste0(metadata$curr_pd, "_SSD_tx-curr-targets.png"),
          path = "Images",
          scale = 0.8)
  
  # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) at OU DATIM data
  
  df_nn <- ou_df %>% 
    filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
           fiscal_year == "2022", 
           funding_agency == "USAID",
           operatingunit = "South Sudan") %>%
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
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_manual(values = c("TX_NEW" = scooter,
                                 "TX_NET_NEW" = scooter_light)) +
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("<span style='color:{scooter_light}'>TX_NET_NEW</span> DROPPED IN Q3, REBOUNDED BY Q4"),
         subtitle = glue("TX_NEW stayed relatively constant"),
         caption = glue("{metadata$caption} | US Agency for International Development")) +
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
           fiscal_year == metadata$curr_fy,
           operatingunit = "South Sudan") %>%
    pluck_totals() %>%
    group_by(fiscal_year, country, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_rtt <- df_iit_rtt %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE), 
           iit_label = percent(iit), 
           qtr_label = glue("{period} ({iit_label})")) %>% 
    ungroup()
  
  vct_itt_cntry <- ou_df %>% 
    filter(funding_agency == "USAID", 
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
           fiscal_year == metadata$curr_fy,
           operatingunit = "South Sudan") %>%
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
    ggplot(aes(qtr_label, iit)) +
    geom_smooth(aes(weight = tx_curr_lag1, group = country),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                linewidth = 1.5, color = old_rose_light) +
    scale_y_continuous(limits = c(0,.15),
                       label = NULL,
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         size = "Site TX_CURR (1 period prior)",
         title = glue("USAID MAINTAINED A RELATIVELY CONSTANT {vct_itt_cntry} IIT OVER FY22"),
         subtitle = glue("A 1% increase Occurred in Q3"),
         caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
                        {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_OU_iit.png"),
          scale = 0.8)  
  
  df_iit_rtt %>%
    mutate(fiscal_year = str_sub(period, end = 4),
           share = tx_rtt/tx_curr,
           share_label = percent(share)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, tx_rtt, fill = as.character(period))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_manual(values = c(denim_light, denim_light, denim_light, denim)) +
    
    labs(x = NULL, y = NULL, fill = NULL,
         title = glue("DESPITE A BRIEF INCREASE IN IIT IN Q3, <span style='color:{denim_light}'>TX_RTT</span> CONTINUED TO INCREASE OVER FY22"),
         caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_OU_rtt.png"),
          scale = 0.8)  
  
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
           is.na(iit) == FALSE, 
           age_cat != "<01") %>%
    ggplot(aes(period, iit, group = sex, color = sex)) +
    geom_smooth(aes(weight = as.numeric(tx_curr_lag1),
                    group = sex, color = sex),
                method = "loess",formula = "y ~ x", 
                se = FALSE, na.rm = TRUE, linewidth = 1.5) +
    facet_wrap(~age_cat, ncol = 3, nrow = 3) +
    scale_y_continuous(limits = c(0,.10),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
    scale_color_manual(values = c(moody_blue_light, genoa_light), 
                       label = NULL) +
     labs(x = NULL, y = NULL,
          title = glue("The increase in Q3 IIT had disparate impacts by age group and sex") %>% toupper,
          subtitle = glue("<span style='color:{moody_blue_light}'>Females</span> in most age groups continue to experience increased IIT into Q4<br>
                           <span style='color:{genoa_light}'>Males</span> in all age groups experienced a decrease in IIT from Q3 to Q4"),
          caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 10%
                         {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown(),
          legend.position="none")
  
  si_save(paste0(metadata$curr_pd, "_SSD_OU_age_sex_iit.png"),
          path = "Images",
          scale = 0.8)
  
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
    mutate(fiscal_year = str_sub(period, end = 4)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, iit)) +
    geom_smooth(aes(weight = tx_curr_lag1, group = prime_partner_name, 
                    color = prime_partner_name),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                size = 1.5) +
    facet_wrap(~prime_partner_name) +
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
          scale = 0.8)  
  
  # RTT by partner
  
  df_iit_rtt_partner <- ou_df %>%
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"),
           fiscal_year == metadata$curr_fy,
           operatingunit =) %>%
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
     mutate(fiscal_year = str_sub(period, end = 4),
            partner_period = glue("{prime_partner_name}_{period}")) %>% 
     filter(tx_curr_lag1 != 0) %>%
     ggplot(aes(period, tx_rtt, fill = partner_period)) +
     geom_col(alpha = .75,
              position = position_dodge(width = 1)) +
     facet_grid(~prime_partner_name) +
     scale_fill_manual(values = c(scooter_light, scooter_light, scooter_light, scooter,
                                  denim_light, denim_light, denim_light, denim,
                                  genoa_light, genoa_light, genoa_light, genoa)) +
     scale_x_discrete(breaks = 
                        unique(df_iit_rtt_partner$period)[grep("Q(4)|Q(2)", 
                        unique(df_iit_rtt_partner$period))]) +
     scale_y_continuous(limits = c(0, 2000), 
                        label = label_number(scale_cut = cut_short_scale())) +
     labs(x = NULL, y = NULL, fill = NULL,
          title = glue("All Partners saw an increase in TX_RTT by Q4") %>% toupper,
          subtitle = glue("CDC Partner Columbia Corporation also saw an increase in Q2 while USAID and DOD Partners did not"),
          caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())

   si_save(glue("Images/{metadata$curr_pd}_SSD_partner_rtt.png"),
           scale = 0.8)  
 
  # ● OU FY22Q4 Target performance for KP and Priority Population --------------
   # plot_name
   # 
   # usaid_df <- ou_df %>%
   #   filter(funding_agency == "USAID")
   # 
   # # # Plot the cascade
   # # # You will be prompted to enter a cascade number
   # return_cascade_plot(ou_df, export = F)
   # 
   # curr_fy <-metadata$curr_fy
   # curr_pd <- metadata$curr_pd
   #
   # Un-comment and enter the name of the plot you selected in between " and the first _
   # ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
   #  plot_file_name = glue("AGYW_Cascade_{metadata$curr_pd}")
   #  13 = glue("KP_Cascade_{metadata$curr_pd}")
   #  1 =  glue("Standard_Cascade_{metadata$curr_pd}")
   # 
   # # save the plot to the Images folder
   # si_save(glue("Images/{plot_file_name}_{ref_id}.png"),
   #          height = 9,
   #          width = 13)


  # ● FSW and KP Clients HIV Testing quarterly Trend
   
   df_kp <- ou_df %>% 
     filter(operatingunit == "South Sudan",
            indicator == "HTS_TST", 
            (standardizeddisaggregate == "KeyPop/Result"),
            fiscal_year %in% c(2021, 2022)) %>%
     group_by(country, fiscal_year, indicator) %>%
     summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
     reshape_msd("quarters") %>% 
     arrange(period)
   
   df_kp <- df_kp %>% 
     mutate(
       growth_rate_req =
         case_when(period == metadata$curr_pd ~
                     ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
     fill(growth_rate_req, .direction = "updown") %>%
     mutate(
       growth_rate = case_when(period %in% c("FY21Q2", "FY21Q3", "FY21Q4", 
                                             "FY22Q2", "FY22Q3", "FY22Q4")
         ~ (results_cumulative / lag(results_cumulative, order_by = period)) - 1),
       growth_rate = na_if(growth_rate, Inf)) %>%
     ungroup() %>%
     mutate(
       geo_gr_lab = case_when(
         is.infinite(growth_rate_req) ~ glue("{toupper(country)}"),
         growth_rate_req < 0 ~ glue("{toupper(country)}\nTarget achieved"),
         growth_rate_req < .1 ~ glue("{toupper(country)}\n{percent(growth_rate_req, 1)}"),
         TRUE ~ glue("{toupper(country)}\n{percent(growth_rate_req, 1)}")),
       gr_lab = glue("{percent(growth_rate, 1)}"),
       disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
       amount_diff = targets - results, 
       pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
   
   df_achv_kp <- df_kp %>% 
     filter(period == metadata$curr_pd) %>%
     count(results >= targets) %>%
     filter(`results >= targets` == TRUE)
   
   df_kp %>%
     ggplot(aes(period, results_cumulative, fill = as.character(period))) +
     geom_blank(aes(y = results_cumulative + 1000)) +
     geom_col(aes(y = targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
     geom_col(na.rm = TRUE) +
     geom_errorbar(aes(ymin = targets, ymax = targets), 
                   linetype = "dashed", width = .95, na.rm = TRUE) +
     geom_text(aes(label = case_when(!gr_lab == "NA" ~ gr_lab), y = results_cumulative-1000),
               family = "Source Sans Pro", color = "white", size = 9 / .pt,
               vjust = -.5, na.rm = TRUE) +
     scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
     scale_x_discrete(breaks = unique(df_kp$period)[grep("Q(4)", unique(df_kp$period))]) +
     scale_fill_manual(values = c(moody_blue_light, moody_blue_light, moody_blue_light, moody_blue_light,
                                  moody_blue_light, moody_blue_light, moody_blue_light, moody_blue)) +
     labs(
       x = NULL, y = NULL,
       title = glue("PEPFAR Performance in Testing Targets (HTS_TST) Among Key Populations (FSW) 
                    Has Maintained Improvements in Achievement Made since FY21") %>% toupper(),
       caption = glue("{metadata$caption} | US Agency for International Development")) +
     si_style_ygrid() +
     theme(
       legend.position = "none",
       panel.spacing = unit(.5, "picas"),
       axis.text.x = element_text(size = 8))
   
   si_save(paste0(metadata$curr_pd, "_SSD_KP_HTS_TST_TRENDS.png"),
           path = "Images",
           scale = 0.8)
  
# PSNUxIM ----------------------------------------------------------------------
   
   # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) by State
   
   df_nn_snu <- snu1df %>% 
     filter(indicator %in% c("TX_CURR", "TX_NEW", "TX_NET_NEW"),
            fiscal_year == "2022", 
            funding_agency %in% c("USAID", "DOD")) %>%
     pluck_totals() %>% 
     group_by(snu1, indicator, fiscal_year) %>% 
     summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
     reshape_msd(include_type = FALSE) %>% 
     pivot_wider(names_from = indicator,
                 names_glue = "{tolower(indicator)}") %>%
     pivot_longer(c(tx_net_new, tx_new), 
                  names_to = "indicator") %>% 
     mutate(fill_color = ifelse(indicator == "tx_net_new", scooter, scooter_light),
            indicator = glue("{toupper(indicator)}"),
            share = value / tx_curr, 
            snu_label = case_when(
                snu1 == "Western Equatoria State" ~ "Western Equatoria",
                snu1 == "Central Equatoria State" ~ "Central Equatoria",
                snu1 == "_Military South Sudan" ~ "Military"))
   
   df_nn_snu %>%
     filter(tx_curr != 0) %>%
     mutate(indicator_snu = glue("{indicator}_{snu_label}")) %>%
     ggplot(aes(period, value, fill = fct_rev(indicator_snu))) +
     geom_col(alpha = .75,
              position = position_dodge(width = .5)) +
     geom_hline(yintercept = 0) +
     facet_wrap(~ fct_reorder2(snu_label, period, value), scales = "fixed") +
     scale_y_continuous(limits = c(-10, 900),
       label = label_number(scale_cut = cut_short_scale())) +
     scale_fill_manual(values = c("TX_NEW_Central Equatoria" = denim,
                                  "TX_NET_NEW_Central Equatoria" = denim_light,
                                  "TX_NEW_Western Equatoria" = burnt_sienna,
                                  "TX_NET_NEW_Western Equatoria" = burnt_sienna_light,
                                  "TX_NEW_Military" = genoa,
                                  "TX_NET_NEW_Military" = genoa_light)) +
      labs(x = NULL, y = NULL, fill = NULL,
           title = glue("USAID'S TX_NET_NEW QUARTERLY TRAJECTORY VARIED BY STATE "),
           subtitle = glue("TX_NEW stayed relatively constant in Central Equatoria while it declined over the year in Western Equatoria"),
           caption = glue("{metadata$caption} | US Agency for International Development")) +
     si_style_ygrid() +
     theme(panel.spacing = unit(.5, "line"),
           legend.position = "none",
           plot.title = element_markdown(),
           strip.text = element_markdown())
   
   si_save(glue("Images/{metadata$curr_pd}_SSD_State_tx-new-nn.png"),
           scale = 1.1)  
   
  # TX-CURR Target Performance by SNU County/ military
  
  snu1df_curr <-  snu1df %>%
    filter(
      fiscal_year == metadata$curr_fy,
      !funding_agency %in% c("HHS/CDC"),
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
  
  snu1df_curr <- snu1df_curr %>%
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
        is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"),
        growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
        TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
      gr_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate, 1)),
      gr_label_position = results - 1500,
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
      amount_diff = targets - results, 
      pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
  
  df_achv_curr <- snu1df_curr %>%
    filter(period == metadata$curr_pd) %>%
    count(type, results >= targets) %>%
    filter(`results >= targets` == TRUE)
  
  pct_change_curr <- snu1df_curr %>%
    filter(type == "Total") %>%
    select(pct_change) %>%
    filter(pct_change == max(as.numeric(pct_change))) %>%
    pull()
      
  snu1df_curr %>%
    filter(type == "Total") %>%
    mutate(
      snu_label = case_when(
        snu1 == "Western Equatoria State" ~ "Western Equatoria",
        snu1 == "Central Equatoria State" ~ "Central Equatoria",
        snu1 == "_Military South Sudan" ~ "Military")) %>%
    ggplot(aes(period, results, fill = as.character(snu1))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), 
                  linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = comma(results), y = gr_label_position),
               family = "Source Sans Pro", color = "white", size = 9 / .pt,
               vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(snu_label, period, targets), scales = "fixed") +
    scale_y_continuous(limits = c(0, 7500),
      label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(snu1df_curr$period)[grep("Q(4)", unique(snu1df_curr$period))]) +
    scale_fill_manual(values = c(genoa_light, denim_light, burnt_sienna_light)) +
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
          scale = 0.8)
  
  # ● TX-NEW Target Performance by SNU County/ military (FY22 Q4 cumulative for TX_NEW)

  snu1_df_new <-  snu1df %>%
    filter(
      fiscal_year == metadata$curr_fy,
      !funding_agency %in% c("HHS/CDC"),
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
  
  snu1_df_new <- snu1_df_new %>%
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
        is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"),
        growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget achieved"),
        growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
        TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
      gr_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate, 1)),
      gr_label_position = 0,
      disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
      amount_diff = targets - results, 
      pct_change = round_half_up((results - targets)/abs(targets) * 100),0)
  
  df_achv_new <- snu1_df_new %>%
    filter(period == metadata$curr_pd) %>%
    count(type, results >= targets) %>%
    filter(`results >= targets` == TRUE)
  
  pct_change_new <- snu1_df_new %>%
    filter(type == "Total") %>%
    select(pct_change) %>%
    filter(pct_change == max(as.numeric(pct_change))) %>%
    pull()
  
  snu1_df_new %>%
    filter(type == "Total") %>%
    mutate(
      snu_label = case_when(
        snu1 == "Western Equatoria State" ~ "Western Equatoria",
        snu1 == "Central Equatoria State" ~ "Central Equatoria",
        snu1 == "_Military South Sudan" ~ "Military")) %>%
    ggplot(aes(period, results, fill = as.character(snu1))) +
    geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
    geom_col(na.rm = TRUE) +
    geom_errorbar(aes(ymin = targets, ymax = targets), 
                  linetype = "dashed", width = .95, na.rm = TRUE) +
    geom_text(aes(label = comma(results), y = gr_label_position),
              family = "Source Sans Pro", color = "white", size = 9 / .pt,
              vjust = -.5, na.rm = TRUE) +
    facet_wrap(~ fct_reorder2(snu_label, period, targets), scales = "free_y") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_x_discrete(breaks = unique(snu1_df_new$period)[grep("Q(4)", unique(snu1_df_new$period))]) +
    scale_fill_manual(values = c(genoa_light, denim_light, burnt_sienna_light)) +
    labs(
      x = NULL, y = NULL,
      title = glue("Performance in Annual New Enrollment Targets Has Fallen Since Q1 for Two out of three SNUs") %>% toupper(),
      subtitle = "Military facilities and those in Western Equatoria State saw drops while Central Equatoria State saw growth",
      caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(
      legend.position = "none",
      panel.spacing = unit(.5, "picas"),
      axis.text.x = element_text(size = 8))
  
  si_save(paste0(metadata$curr_pd, "_SSD-_tx-new-targets_snu.png"),
          path = "Images",
          scale = 0.8)
  
# SitexIM ----------------------------------------------------------------------
  
  # ● Treatment Gain / Loss Trends (TX-NET-NEW and Gain/Loss) for largest sites 
  # contributing to IIT
  
  df_iit_site <- site_df %>% 
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW"),
           funding_agency == "USAID",
           fiscal_year == "2022", 
           sitename != "Data reported above Site level") %>%
    pluck_totals() %>%
    group_by(fiscal_year, psnu, sitename, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_site <- df_iit_site %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_iit_site %>%
    filter(period == "FY22Q4") %>%
    mutate(site_label = stringr::str_replace_all(sitename, 
                                                "Primary Health Care Centre", 
                                                "PHCC"), 
           large_iit = case_when(iit > 0.05 ~ TRUE), 
           iit_label = percent(iit, accuracy = 1), 
           tx_curr_label = comma(tx_curr)) %>%
  drop_na(iit_label) %>%
  ggplot(aes(x = tx_curr, y = iit_label, size = tx_curr)) +
    geom_point(alpha = 0.7, aes(color = large_iit)) +
    geom_text(aes(label = case_when(large_iit == TRUE ~ site_label)),
              family = "Source Sans Pro", color = "black", size = 9 / .pt,
              vjust = -.5, na.rm = TRUE) +
    si_style_ygrid() +
    
    labs(
      x = NULL, y = NULL,
      title = glue("5 Largest Sites with the Highest Q4 IIT") %>% toupper(),
      subtitle = "Size of points scaled by TX_CURR",
      caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(
      legend.position = "none",
      panel.spacing = unit(.5, "picas"),
      axis.text.x = element_text(size = 8))
  
  si_save(paste0(metadata$curr_pd, "_SSD_Sitelevel_IIT_TX_CURR.png"),
          path = "Images",
          scale = 0.8)
  
  df_nn_site <- site_df %>% 
    mutate(sitename = stringr::str_to_title(sitename)) %>%
    filter(funding_agency == "USAID", 
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
           fiscal_year == "2022",
            sitename %in% c("Gurei Primary Health Care Centre",
                            "Munuki Primary Health Care Centre",
                            "Kator Primary Health Care Centre",
                            "Nyakuron Primary Health Care Centre", 
                            "Juba Protection Of Civilians")) %>%
    pluck_totals() %>%
    group_by(sitename, indicator, fiscal_year) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>%
    pivot_longer(c(tx_net_new, tx_new), 
                 names_to = "indicator") %>% 
    mutate(fill_color = ifelse(indicator == "tx_net_new", scooter, scooter_light),
           indicator = glue("{toupper(indicator)}"),
           share = value / tx_curr)
  
  df_nn_site %>%
    filter(tx_curr != 0) %>%
    mutate(site_label = stringr::str_replace_all(sitename, 
                                                 "Primary Health Care Centre", 
                                                 "PHCC")) %>%
    ggplot(aes(period, value, fill = fct_rev(indicator))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0) +
    facet_wrap(~fct_reorder2(site_label, period, tx_curr),
               scales = "fixed") +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_manual(values = c("TX_NEW" = scooter,
                                 "TX_NET_NEW" = scooter_light)) +
      labs(x = NULL, y = NULL, fill = NULL,
           caption = glue("{metadata$caption} | US Agency for International Development")) +
    #       title = glue("AMONG SITES WITH THE HIGHEST IIT, <span style='color:{scooter_light}'>TX_NET_NEW</span> AND <span style='color:{scooter}'>TX_NEW</span> NARROWING OVER FY22"),
    #       subtitle = glue("Substantial Drop in Q3 Likely Tempered Progress"),
    #       caption = glue("{metadata$caption} | US Agency for International Development | {ref_id}")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(paste0(metadata$curr_pd, "_SSD_tx-new-net-new_sites.png"),
          path = "Images",
          scale = 0.8)
  
  # ● IIT by age and sex for two facilities with highest IIT
  
  df_iit_site <- site_df %>%
    mutate(sitename = stringr::str_to_title(sitename)) %>%
    filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", 
                            "TX_CURR_Lag1", "TX_RTT", "TX_NET_NEW"), 
           standardizeddisaggregate %in% c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus"), 
           fiscal_year == "2022", 
           funding_agency == "USAID",
           sitename %in% c("Munuki Primary Health Care Centre",
                          "Gurei Primary Health Care Centre")) %>% 
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
    group_by(fiscal_year, sitename, indicator, age_cat, sex) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}") %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() 
  
  df_iit_site <- df_iit_site %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup() %>%
    filter(!age_cat == "<01")
  
  df_iit_site %>%
    filter(sitename == "Munuki Primary Health Care Centre",
           
           tx_curr_lag1 != 0,
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
    # labs(x = NULL, y = NULL,
    #      title = glue("The increase in Q3 IIT had disparate impacts by age group and sex") %>% toupper,
    #      subtitle = glue("<span style='color:{moody_blue_light}'>Females</span> in most age groups continue to experience increased IIT into Q4<br>
    #                        <span style='color:{genoa_light}'>Males</span> in all age groups experienced a decrease in IIT from Q3 to Q4"),
    #      caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
    #                      {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown(),
          legend.position="none")
  
  si_save(paste0(metadata$curr_pd, "_SSD_Munuki_age_sex_iit.png"),
          path = "Images",
          scale = 0.8)
  
  df_iit_site %>%
    filter(sitename == "Gurei Primary Health Care Centre") %>%
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
    # labs(x = NULL, y = NULL,
    #      title = glue("The increase in Q3 IIT had disparate impacts by age group and sex") %>% toupper,
    #      subtitle = glue("<span style='color:{moody_blue_light}'>Females</span> in most age groups continue to experience increased IIT into Q4<br>
    #                        <span style='color:{genoa_light}'>Males</span> in all age groups experienced a decrease in IIT from Q3 to Q4"),
    #      caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
    #                      {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown(),
          legend.position="none")
  
  si_save(paste0(metadata$curr_pd, "_SSD_Gurei_age_sex_iit.png"),
          path = "Images",
          scale = 0.8)

  # ● IIT and return to care (TX_RTT) Trends 2 sites with highest IIT
  
  df_iit_rtt_site <- site_df %>% 
    mutate(sitename = stringr::str_to_title(sitename)) %>%
    filter(funding_agency == "USAID", 
           indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW", "TX_RTT"), 
           fiscal_year == "2022", 
           sitename %in% c("Munuki Primary Health Care Centre",
                           "Gurei Primary Health Care Centre")) %>%
    pluck_totals() %>%
    group_by(fiscal_year, sitename, indicator) %>% 
    summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
    reshape_msd(include_type = FALSE) %>% 
    pivot_wider(names_from = "indicator",
                names_glue = "{tolower(indicator)}")
  
  df_iit_rtt_site <- df_iit_rtt_site %>%
    mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
    rowwise() %>% 
    mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
    ungroup()
  
  df_iit_rtt_site %>%
    mutate(fiscal_year = str_sub(period, end = 4)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, iit, size = tx_curr_lag1)) +
    geom_smooth(aes(weight = tx_curr_lag1, group = sitename, 
                    color = sitename),
                method = "loess",
                formula = "y ~ x", se = FALSE, na.rm = TRUE,
                linewidth = 1.5) +
    facet_grid(~sitename) +
    scale_color_manual(values = c(moody_blue, golden_sand)) +
    scale_size(label = comma, guide = NULL) +
    scale_y_continuous(limits = c(0,.15),
                       label = percent_format(1),
                       oob = oob_squish) +
    coord_cartesian(clip = "off") +
     labs(x = NULL, y = NULL,
          size = "Site TX_CURR (1 period prior)",
          title = glue("OVER FY22, BOTH OF THE TWO SITES CONTRIBUTING MOST TO HIGH IIT IN Q4 WERE TRENDING DOWNWARD"),
          subtitle = glue("Gurei PHCC saw a sharp increase in Q2 while Munuki PHCC saw a sharp increase in Q3"),
          caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 15%
                         {metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          axis.text = element_text(size = 8),
          plot.subtitle = element_markdown(),
          legend.position = "none")
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_Site_iit.png"),
          scale = 0.8)  
  
  df_iit_rtt_site %>%
    mutate(fiscal_year = str_sub(period, end = 4)) %>% 
    filter(tx_curr_lag1 != 0) %>%
    ggplot(aes(period, tx_rtt, fill = as.character(sitename))) +
    geom_col(alpha = .75,
             position = position_dodge(width = .5)) +
    geom_hline(yintercept = 0) +
    facet_grid(~sitename) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    scale_fill_manual(values = c(moody_blue, golden_sand)) +
     labs(x = NULL, y = NULL, fill = NULL,
          title = glue("TX_RTT HAD RISEN AT GUREI PHCC WHILE IT FELL AT MUNUKI PHCC"),
          caption = glue("{metadata$caption} | US Agency for International Development")) +
    si_style_ygrid() +
    theme(panel.spacing = unit(.5, "line"),
          legend.position = "none",
          plot.title = element_markdown(),
          strip.text = element_markdown())
  
  si_save(glue("Images/{metadata$curr_pd}_SSD_site_rtt.png"),
          scale = 0.8)  
  
  # ● KP Target performance by Towns (Reach and Testing)
  # town?
  # KP PREV
  # hts_tst
  # KP/Result
  
  # ● KP and Priority Population HIV Testing and Yield by Towns
  # town?
  # hts_tst
  # yield = hts_tst_pos / (hts_tst_pos + hts_tst_neg)
  # AGYW
  # Peds
  # KP/Result
