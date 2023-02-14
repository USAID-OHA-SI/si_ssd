# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  practice with COP22 viz
# REF ID:   5eedfd8a 
# LICENSE:  MIT
# DATE:     2023-01-24
# Notes: adapted from 
#         Treatment/6MMD: agitprop/29_treat_qtr_mmd-usaid.R
#         VLC/VLS: catch-22/20211018_USAID_VLC-by-country.R

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(ggtext)
  library(patchwork)
  library(cascade)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "5eedfd8a"
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  clean_mechs <- function(.data) {
    
    # Check for valid column name
    .data %>%
      assertr::verify("mech_name" %in% names(.))

    # clean column data
    .data <- .data %>%
      dplyr::mutate(mech_name = recode(mech_name, 
            "International Center for AIDS Care and Treatment Programs, Columbia University" = "ICAP", 
            "Advancing HIV/AIDS Epidemic Control (AHEC)" = "AHEC", 
            "RTI Care and Treatment" = "RTI", 
            "ICAP HQ" = "ICAP"))
  }
  
  # Summarizes patient gain/loss quarterly trend
  
  ou_patient_delta <- function(.path, .df, .ou, .fiscal_year, .type, .subtitle,
                               .funding_agency = NULL, ...) {
    # metadata
    si_path() %>%
      return_latest(.path) %>%
      get_metadata()
    
    # add a unit test to check that tx_new and tx_net_new exist in .df
    # add a unit test to check that path, df, ou, years, and agency(if not null)
    # are not empty and exist in df
    
    
    # filter for type, Total or Adults/Children
    if (.type == "Total") {
      .df <- .df %>%
        filter(
          indicator %in% c("TX_NEW", "TX_NET_NEW"),
          operatingunit == .ou,
          fiscal_year %in% .fiscal_year) %>%
        pluck_totals()
      
    } else {
      peds <- c("<01", "01-04", "05-09", "10-14")
      adults <- c(
        "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65+")
      
      .df <- .df %>%
        filter(
          operatingunit == .ou,
          (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
            (standardizeddisaggregate == "Total Numerator") |
            (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults)) %>%
        mutate(type = case_when(
          standardizeddisaggregate == "Total Numerator" ~ "Total",
          standardizeddisaggregate == "Age/Sex/HIVStatus" &
            ageasentered %in% adults ~ "Adults",
          standardizeddisaggregate == "Age/Sex/HIVStatus" &
            ageasentered %in% peds ~ "Pediatric")) %>%
        filter(type == .type)
    }
    
    # filter for funding agency
    if (!is.null(.funding_agency)) {
      df <- df %>%
        filter(funding_agency == .funding_agency)
    }
    
    df_iit <- .df %>%
      clean_indicator() %>%
      group_by(operatingunit, fiscal_year, indicator) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>%
      pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}") %>%
      mutate(
        fiscal_year = str_extract(period, "FY[0-2]{2}"),
        patient_loss_gain = tx_net_new - tx_new) %>%
      ungroup() %>%
      pivot_longer(c(patient_loss_gain),
                   names_to = "indicator") %>%
      mutate(
        delta_lab = if_else(indicator == "patient_loss_gain",
                            comma(value), ""),
        max = max(value, na.rm = TRUE) + round(value))
    
    df_iit %>%
      ggplot(aes(x = period)) +
      geom_blank(aes(y = max)) +
      geom_col(aes(y = value, fill = fiscal_year), alpha = .7) +
      # note, ncol can be changed usingthe function if > 2 fiscal years are used
      facet_wrap(~fiscal_year, scales = "free_x", ncol = 1) +
      geom_text(aes(label = delta_lab, y = value, color = fiscal_year),
                position = position_dodge(width = 0.75),
                family = "Source Sans Pro", size = 12 / .pt,
                vjust = -.5, na.rm = TRUE) +
      scale_color_manual(values = c(usaid_medblue, usaid_lightblue)) +
      scale_fill_manual(values = c(usaid_lightblue, usaid_medblue)) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_iit$period)[grep("Q(4)", unique(df_iit$period))]) +
      si_style_ygrid() +
      theme(
        panel.spacing = unit(.5, "line"),
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown()) +
      labs(
        x = NULL, y = NULL, fill = NULL,
        subtitle = glue("{.subtitle}"),
        caption = glue(" Note: patient gain/loss = TX_NET_NEW - TX_NEW
                             Adults = Ages 15 +, Children = Ages < 15
                  {metadata$caption} | US Agency for International Development"))
  }
  
  # Summarizes the quarterly trend in IIT, RTT, and unexplained gain/loss in patients
  ou_iit_rtt_trend <- function(.path, .df, .ou, .fiscal_year,.type,
                               .subtitle, .funding_agency = NULL, ...) {
    # metadata
    si_path() %>%
      return_latest(.path) %>%
      get_metadata()
    
    # add a unit test to check that required indicators exist in .df
    
    # filter for type, Total or Adults/Children
    if (.type == "Total") {
      
      
      .df <- .df %>%
        filter(
          indicator %in% c(
            "TX_ML", "TX_ML_IIT", "TX_CURR",
            "TX_NEW", "TX_NET_NEW", "TX_RTT"),
          standardizeddisaggregate %in%
            c("Age/Sex/ARTNoContactReason/HIVStatus", "Total Numerator"),
          is.na(otherdisaggregate) |
            str_detect(
              otherdisaggregate,
              "No Contact Outcome - Interruption in Treatment"),
          !(standardizeddisaggregate == "Total Numerator" & indicator == "TX_ML"),
          fiscal_year %in% .fiscal_year,
          operatingunit == .ou) %>%
        filter(
          standardizeddisaggregate == "Total Numerator" |
            (standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus" &
               indicator == "TX_ML"))
      
      
    } 
    
    else {
      
      peds <- c("<01", "01-04", "05-09", "10-14")
      adults <- c(
        "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
        "50-54", "55-59", "60-64", "65+")
      
      .df <- .df %>%
        filter(
          indicator %in% c(
            "TX_ML", "TX_ML_IIT", "TX_CURR",
            "TX_NEW", "TX_NET_NEW", "TX_RTT"),
          standardizeddisaggregate %in%
            c("Age/Sex/ARTNoContactReason/HIVStatus", "Total Numerator"),
          is.na(otherdisaggregate) |
            str_detect(otherdisaggregate,"No Contact Outcome - Interruption in Treatment"),
          !(standardizeddisaggregate == "Total Numerator" &
              indicator == "TX_ML"),
          fiscal_year %in% .fiscal_year,
          operatingunit == .ou) %>%
        mutate(
          type = case_when(
            standardizeddisaggregate == "Total Numerator" ~ "Total",
            standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus" &
              ageasentered %in% adults ~ "Adults",
            standardizeddisaggregate == "Age/Sex/ARTNoContactReason/HIVStatus" &
              ageasentered %in% peds ~ "Pediatric")) %>%
        filter(type %in% c("Total", glue("{.type}")))
    }
    
    if (!is.null(.funding_agency)) {
      .df <- .df %>%
        filter(funding_agency == .funding_agency)
      
      # how can we dynamically title the agency specific graphs?
    }
    
    df_iit <- .df %>%
      clean_indicator() %>%
      group_by(operatingunit, fiscal_year, indicator) %>%
      mutate(
        indicator = if_else(
          indicator == "TX_ML", "TX_ML_IIT", indicator)) %>%
      summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
      reshape_msd(include_type = FALSE) %>%
      pivot_wider(
        names_from = indicator,
        names_glue = "{tolower(indicator)}") %>%
      rowwise() %>%
      mutate(
        tx_curr_lag1 = as.numeric(tx_curr - tx_net_new),
        share_rtt_curr = as.numeric(tx_rtt / tx_curr),
        share_rtt_label = percent(share_rtt_curr),
        tx_curr_label = comma(round(tx_curr)),
        iit_label = comma(tx_ml_iit),
        fiscal_year = str_extract(period, "FY[0-2]{2}"),
        period_num = str_extract(period, "Q[1-4]"),
        period_num = as.numeric(str_extract(period_num, "[1-4]")),
        delta_patients = -tx_ml_iit + as.numeric(tx_rtt),
        tx_rtt_gain = (tx_curr_lag1 + tx_new + tx_rtt),
        unexplained_loss_gain = (tx_rtt_gain - tx_ml_iit - tx_curr),
        gain_loss_colors = if_else(unexplained_loss_gain > 0,
                                   usaid_lightgrey, "#FFFFFF")) %>%
      ungroup() %>%
      pivot_longer(c(tx_ml_iit, unexplained_loss_gain, tx_rtt),
                   names_to = "indicator") %>%
      mutate(
        unexplained_lab = if_else(indicator == "unexplained_loss_gain",
                                  comma(value), ""),
        value = if_else(indicator == "tx_ml_iit", -value, value),
        value_filt = if_else(indicator == "unexplained_loss_gain",
                             0, value))
    
    df_iit %>%
      ggplot(aes(x = period, fill = fct_rev(indicator))) +
      geom_col(aes(y = value_filt), alpha = .7) +
      facet_wrap(~fiscal_year, scales = "free_x", ncol = 1) +
      geom_text(aes(label = unexplained_lab, y = 0, color = gain_loss_colors),
                position = position_dodge(width = 0.75),
                family = "Source Sans Pro", size = 12 / .pt,
                vjust = -.5, na.rm = TRUE) +
      scale_color_manual(values = c("#FFFFFF", "#FFFFFF")) +
      scale_fill_manual(values = c(
        "tx_ml_iit" = usaid_lightblue,
        "tx_rtt" = usaid_medblue,
        "unexplained_loss_gain" = "#FFFFFF")) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_iit$period)[grep("Q(4)", unique(df_iit$period))]) +
      si_style_ygrid() +
      theme(
        panel.spacing = unit(.5, "line"),
        # temporary since this is in grayscale
        legend.position = "none",
        plot.title = element_markdown(),
        strip.text = element_markdown()) +
      labs(
        x = NULL, y = NULL, fill = NULL,
        subtitle = glue("{.subtitle}"),
        caption = glue(" Notes: 
                        tx_ml_iit = TX_ML where patient experienced IIT;
                        tx_rtt_gain = tx_curr_lag1 + tx_new + tx_rtt;
                        unexplained_loss_gain =  tx_rtt_gain - tx_ml_iit  - tx_curr
                   Number displayed is the unexplained loss or gain
                   Adults = Ages 15 +, Children = Ages < 15
                  {metadata$caption} | US Agency for International Development"))
  }
  
  # Summarizes quarterly progress on selected indicators
  ou_achv_qtr <- function(.path, .df, .indicator, .ou, .type, .subtitle,
                          .funding_agency = NULL, ...) {
    # metadata
    si_path() %>%
      return_latest(.path) %>%
      get_metadata()
    
    peds <- c("<01", "01-04", "05-09", "10-14")
    adults <- c(
      "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
      "50-54", "55-59", "60-64", "65+")
    
    if (!is.null(.funding_agency)) {
      .df <- .df %>%
        filter(
          funding_agency == .funding_agency
        )
    }
    
    df_new <- .df %>%
      filter(
        operatingunit == .ou,
        indicator == .indicator,
        (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
          (standardizeddisaggregate == "Total Numerator") |
          (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults)) %>%
      mutate(type = case_when(standardizeddisaggregate == "Total Numerator" ~ "Total", 
                              standardizeddisaggregate == "Age/Sex/HIVStatus" & 
                                ageasentered %in% adults ~ "Adults",
                              standardizeddisaggregate == "Age/Sex/HIVStatus" & 
                                ageasentered %in% peds ~ "Pediatric")) %>%
      filter(type == .type) %>%
      group_by(fiscal_year, operatingunit, indicator, type) %>%
      summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
                .groups = "drop"
      ) %>%
      reshape_msd("quarters") %>%
      select(-results_cumulative) %>%
      arrange(type, operatingunit, period)
    
    df_new <- df_new %>%
      mutate(
        growth_rate_req =
          case_when(period == metadata$curr_pd ~
                      ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)
      ) %>%
      group_by(type) %>%
      fill(growth_rate_req, .direction = "updown") %>%
      mutate(
        growth_rate = (results / lag(results, order_by = period)) - 1,
        growth_rate = na_if(growth_rate, Inf)
      ) %>%
      ungroup() %>%
      mutate(
        geo_gr_lab = case_when(
          is.infinite(growth_rate_req) ~ glue("{toupper(operatingunit)}"),
          growth_rate_req < 0 ~ glue("{toupper(operatingunit)}\nTarget achieved"),
          growth_rate_req < .1 ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}"),
          TRUE ~ glue("{toupper(operatingunit)}\n{percent(growth_rate_req, 1)}")
        ),
        gr_lab = case_when(fiscal_year == metadata$curr_fy ~
                             glue("{percent(growth_rate, 1)}")),
        gr_lab = stringr::str_replace(gr_lab, "NA", "0"),
        gr_label_position = 0,
        results_lab = case_when(fiscal_year == metadata$curr_fy ~
                                  glue("{comma(results)}")),
        disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets),
        unit_label = glue("(Operating Unit)"),
        amount_diff = targets - results,
        pct_change = round_half_up((results - targets) / abs(targets) * 100), 0
      )
    
    # percentage change from q1 to q4
    pct_change_new <- df_new %>%
      filter(type == .type) %>%
      select(fiscal_year, pct_change) %>%
      filter(pct_change == max(as.numeric(pct_change))) %>%
      pull()
    
    df_new %>%
      filter(type == .type) %>%
      ggplot(aes(period, results, fill = as.character(period))) +
      geom_col(na.rm = TRUE, alpha = .7, width = 1) +
      geom_text(aes(label = results_lab, y = results),
                family = "Source Sans Pro", color = usaid_darkgrey, size = 9 / .pt,
                vjust = -.5, na.rm = TRUE
      ) +
      geom_text(aes(label = gr_lab, y = gr_label_position),
                family = "Source Sans Pro", color = "white", size = 9 / .pt,
                vjust = -.5, na.rm = TRUE
      ) +
      scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
      scale_x_discrete(breaks = unique(df_new$period)[grep("Q(4)", unique(df_new$period))]) +
      scale_fill_manual(values = c(
        usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
        usaid_lightgrey, usaid_lightgrey, usaid_lightgrey, usaid_lightgrey,
        usaid_darkgrey, usaid_darkgrey, usaid_darkgrey, usaid_darkgrey
      )) +
      labs(
        x = NULL, y = NULL,
        title = NULL,
        subtitle = glue("{.subtitle}"),
        caption = glue("Note: Adults = Ages 15+ and Children= Ages <15
                     {metadata$caption} | US Agency for International Development")
      ) +
      si_style_ygrid() +
      theme(
        legend.position = "none",
        panel.spacing = unit(.5, "picas"),
        axis.text.x = element_text(size = 8)
      )
  }
  
  ou_path <- "OU_IM_South_Sudan"
  psnu_path <- "PSNU_IM_South_Sudan_Frozen_2023-13-02"
  authors <- "Jessica Hoehner"

# IMPORT ----------------------------------------------------------------------
  
  ou_df <- si_path() %>%
    return_latest(ou_path) %>%
    read_msd()
  
  psnu_df <- si_path() %>%
    return_latest(psnu_path) %>%
    read_msd()
  
  df_nat <- si_path() %>% 
    return_latest("NAT") %>% 
    read_msd()
  
  metadata_nat <- si_path() %>%
    return_latest("NAT") %>%
    get_metadata()
  
  
    get_metadata()

# MUNGE -----------------------------------------------------------------------
  
  # Successes
  
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
  
  
   
  # KP 

   
   
   
   
   
  # Challenges --------------------------------
  
  # What is the growth of TX_CURR since COP22?
  # How has case-finding changed since COP22?
  
  # How has VL testing coverage/VLS (3rd 95) changed since COP22?
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
   
   #identify where 80% of TX_CURR is for viz facet
   df_grps_adj <- df_vls_grp %>% 
     count(snu1, mech_name, wt = tx_curr) %>% 
     arrange(desc(n)) %>% 
     mutate(cumsum = cumsum(n),
            share = cumsum/sum(n))
  
  # Priority Strategies ------------------------
  
  # Index Testing achievement against targets since COP22
  # TX_ML_IIT, RTT by partner
  # VL testing achievement against targets, EID, TB

  
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
 
  # OVC --------------------------------------
  # how many OVC receive services?
  # Ask OVC ISME
  # how is achievement against targets?
  # what percentage of OVC beneficiaries <18 know their status?
  # what percentage of OVCLHIV are on ART?
   
   # KP cascade
   return_cascade_plot(psnu_df, 13)
   
   
   si_save(glue("Images/{metadata$curr_pd}_SSD_AYPFemalcascade.png"),
           scale = 1.3)  
  

  # Challenges
  
  # What is the growth of TX_CURR since COP22?
   
  # How has case-finding changed since COP22?
  
  # How has VL testing coverage/VLS (3rd 95) 
  #                    changed since COP22?
  # - specific populations
  # - infant (2 months) diagnosis (HTS_TST_POS?)
   
   df_vls %>% 
     filter(period %in% c("FY22Q1", "FY22Q2", "FY22Q3", "FY22Q4")) %>%
     ggplot(aes(vlc, fct_reorder(snu1, vlc, na.rm = TRUE))) +
     geom_blank() +
     annotate("rect", xmin = -Inf, xmax = .9, ymin = 0, ymax = Inf,
              fill = trolley_grey_light, alpha = .4) +
     geom_vline(xintercept = .9, linetype = "dashed") +
     geom_point(aes(size = tx_curr, color = fill_color), alpha = .6,
                position = position_jitter(width = 0, height = 0.1, seed = 42), na.rm = TRUE) +
     geom_errorbar(aes(xmin = vlc_nat_avg, xmax = vlc_nat_avg, color = color_bar), 
                   linewidth = 1.1) + 
     scale_x_continuous(label = percent_format(1)) +
     facet_grid(
       #period ~ ., 
                cols = vars(period),
                scale = "free_y", space = "free") +
     scale_size(labels = number_format(.1, scale = 1e-6, suffix = "M"),
                range = c(2,10)) +
     scale_color_identity() +
     coord_cartesian(clip = "off") +
     expand_limits(x = .75) +
     # labs(y = NULL, x = "Viral Load Coverage Rate (TX_PVLS_D/TX_CURR)",
     #      title = glue("Viral Load Coverage (VLC) rate improved over FY22 with 
     #                    
     #                   at {percent(df_usaid_adj$vlc, 1)} in {pd}, 
     #      the agency has significant work to reach the goal of 90% VLC") %>% toupper() %>% str_wrap(),
     #      size = glue("Current on Treatment (metadata$curr_pd)"),
     #      caption = glue("Source: {metadata$source}, 
     #                     Created by: USAID OHA SI Team")) +
     si_style(facet_space = .5) +
     theme(legend.position = "none",
           axis.title = element_blank(),
           axis.text = element_text(size = 9),
           strip.text = element_text(size = 9),
           axis.text.y = element_text(family = "Source Sans Pro"),
           strip.text.y = element_text(family = "Source Sans Pro"))  
  
  # Priority Strategies:
  
  # Index Testing achievement against targets since COP22
  
  # TX_ML_IIT, RTT
   
   ou_iit_rtt_trend(.path = ou_path, 
                    .df = ou_df, 
                    .fiscal_year = metadata$curr_fy, 
                    .type = "Total", 
                    .ou = "South Sudan", 
                    .subtitle = glue::glue("South Sudan | {metadata$curr_pd}")
                    )
   
   ou_iit_rtt_trend(.path = ou_path, 
                    .df = ou_df, 
                    .fiscal_year = metadata$curr_fy, 
                    .type = "Adult", 
                    .ou = "South Sudan", 
                    .subtitle = glue::glue("South Sudan | {metadata$curr_pd}")
   )
   

   ou_iit_rtt_trend(.path = ou_path, 
                    .df = ou_df, 
                    .fiscal_year = metadata$curr_fy, 
                    .type = "Pediatric", 
                    .ou = "South Sudan", 
                    .subtitle = glue::glue("South Sudan | {metadata$curr_pd}")
   )
   
     
    ou_patient_delta(.path = ou_path, 
                      .df = ou_df, 
                      .fiscal_year = metadata$curr_fy, 
                      .type = "Adults", 
                      .ou = "South Sudan", 
                      .subtitle = glue::glue("South Sudan | {metadata$curr_pd}")
     )
   
   
  
  # VL testing achievement against targets, EID, TB
   
   

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  