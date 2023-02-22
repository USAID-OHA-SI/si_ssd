# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  All functions used in pre-COP and COP dataviz for SSD
# REF ID:   2b64ae30 
# LICENSE:  MIT
# DATE:     2023-02-17
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------

  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(glue)
  library(scales)
  library(ggtext)
  library(patchwork)
  library(cascade)
  library(ggrepel)

# FUNCTIONS --------------------------------------------------------------------

# Cleaning ----

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

# Trends ----

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

# Achivement ----
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

# Prevalence ----

# Adapted from hardapoart!

#' @title Prep HIV Prevalence Source Data
#' 
#' @param cntry     OU/Country name
#' @param add_style Append color code
#' 
prep_hiv_prevalence <- function(df, cntry,
                                add_style = T) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  ## SNU/Age/Sex Summaries
  df_pops <- df %>% 
    dplyr::filter(fiscal_year == max(fiscal_year),
                  country == cntry) %>% 
    dplyr::group_by(fiscal_year, operatingunit, country, snu1uid, snu1,
                    indicator, ageasentered, sex) %>% 
    dplyr::summarise(value = sum(targets, na.rm = T), .groups = "drop")
  
  #clean exit if no data
  if(nrow(df_pops) == 0)
    return(NULL)
  
  ## Add OU/Country Summary
  
  df_pops <- df_pops %>% 
    group_by(fiscal_year, operatingunit, country, indicator, ageasentered, sex) %>% 
    summarise(value = sum(value, na.rm = T), .groups = "drop") %>% 
    mutate(snu1 = "COUNTRY") %>% 
    bind_rows(df_pops, .)
  
  # SNU Only Summaries
  df_pops_snu <- df_pops %>%
    group_by(fiscal_year, operatingunit, country,
             snu1uid, snu1, indicator) %>%
    summarise(value = sum(value, na.rm = T), .groups = "drop")
  
  # Sex Only Summaries
  df_pops_sex <- df_pops %>%
    group_by(fiscal_year, operatingunit, country,
             snu1uid, snu1, indicator, sex) %>%
    summarise(value = sum(value, na.rm = T), .groups = "drop")
  
  ## Compute Prevalence
  df_prev_sex <- df_pops_sex %>% 
    group_by(fiscal_year, operatingunit, country, snu1uid, snu1, sex) %>% 
    reframe(prevalence = value[indicator == "PLHIV"] / 
              value[indicator == "POP_EST"]) %>% 
    ungroup() 
  
  df_prev_snu <- df_pops_snu %>% 
    group_by(fiscal_year, operatingunit, country, snu1uid, snu1) %>% 
    reframe(snu_prev = sum(value[indicator == "PLHIV"], na.rm = T) / 
              sum(value[indicator == "POP_EST"], na.rm = T)) %>% 
    ungroup() 
  
  df_prev <- df_prev_sex %>% 
    left_join(df_prev_snu,
              by = c("fiscal_year","operatingunit", "country", 
                     "snu1uid", "snu1"))
  
  ## Add SI Style for viz
  if (add_style) {
    
    df_prev_gap <- df_prev %>% 
      select(-snu_prev) %>% 
      mutate(sex = tolower(sex)) %>% 
      pivot_wider(names_from = sex,
                  values_from = prevalence) %>% 
      mutate(color_gap = grey30k)
    
    df_prev <- df_prev %>% 
      left_join(df_prev_gap,
                by = c("fiscal_year","operatingunit", "country", 
                       "snu1uid", "snu1")) %>% 
      mutate(
        color_sex = case_when(
          sex == "Female" ~ moody_blue,
          sex == "Male" ~ genoa,
          TRUE ~ grey30k
        ),
        snu_label = case_when(
          snu1 %in% c("COUNTRY", "OU") ~ paste0("<span style='color:", usaid_black, "'><strong>", snu1, "</strong></span>"),
          TRUE ~ snu1
        ),
      ) %>% 
      group_by(operatingunit) %>% 
      mutate(
        threshold = case_when(
          snu_prev < prevalence[snu1 == "COUNTRY"] ~ .3,
          TRUE ~ 1
        )
      ) %>% 
      ungroup()
  }
  
  return(df_prev)
}

#' @title Visualize HIV Prevalence by SNU/Gender
#' 
#' 
#' 
viz_hiv_prevalence <- function(df, save = F) {
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  # OU/Country Reference line
  
  ref_id <- "8fb89847"
  ref_snu <- "COUNTRY"
  vrsn <- 1 
  
  # Guides
  gap_max <- df %>% 
    filter(snu1 %ni% c("COUNTRY", "OU")) %>% 
    pull(prevalence) %>%
    max() %>%
    round(2)
  
  gap_step <- .01
  
  # Control the number of vlines
  if (gap_max > .10) {
    gap_step <- .05
  } else if (gap_max <= .02) {
    gap_step <- .005
  }
  
  # Display only a subset
  df_viz <- df %>% 
    dplyr::slice_max(order_by = snu_prev, n = 21 * 2) 
  
  if ("COUNTRY" %ni% df_viz$snu1) {
    df_viz <- df %>% 
      filter(snu1 == "COUNTRY") %>% 
      bind_rows(df_viz, .)
  }
  
  # Viz
  viz <- df_viz %>% 
    ggplot(aes(x = reorder(snu1, female), 
               y = prevalence,
               fill = color_sex)) +
    geom_hline(yintercept = seq(from = 0, 
                                to = gap_max, 
                                by = gap_step),
               linewidth = .8, linetype = "dashed", color = grey20k) +
    geom_vline(xintercept = ref_snu,
               linewidth = .8, linetype = "dashed", color = usaid_darkgrey) +
    geom_segment(aes(xend = reorder(snu1, female),
                     y = female, 
                     yend = male,
                     color = color_gap),
                 linewidth = 2) +
    geom_point(shape = 21, size = 5, color = grey10k) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_y_continuous(labels = percent, position = "right") +
    coord_flip() +
    labs(x = "", y = "", 
         title = glue::glue("{toupper(unique(df$country))} - {unique(df$fiscal_year)} HIV PREVALANCE"),
         subtitle = glue::glue("HIV Prevalence Gap between <span style='color:{genoa}'>Male</span> & <span style='color:{moody_blue}'>Female</span> by SNU"),
         caption = glue::glue("{metadata_nat$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    si_style_nolines() +
    theme(plot.subtitle = element_markdown(),
          axis.text.y = element_markdown())
  
  print(viz)
  
  if (save) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Graphics/{unique(df$fiscal_year)} - {toupper(unique(df$country))} HIV Prevalence.png"))
  }
}

# VL ----

#' @title Prep TX VL Datasets
#' 
prep_viral_load <- function(df, cntry, pd_hist = 5) {
  
  #clean exit if no data
  if(cntry %in% unique(df$country) 
     #| 
     #agency %ni% unique(df$funding_agency)
  )
    return(NULL)
  
  # Filter
  df_tx <- df %>% 
    filter(
      #funding_agency == agency, 
      country == cntry,
      indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                      "Age/Sex/Indication/HIVStatus")) 
  
  if(nrow(df_tx) == 0)
    return(NULL)
  
  # Summarise results by age - bands
  df_tx <- df_tx %>%
    select(-cumulative, -targets) %>% 
    filter(ageasentered != "Unknown Age") %>% 
    mutate(age = case_when(
      trendscoarse == "<15" ~ trendscoarse,
      ageasentered == "15-19" ~ ageasentered,
      ageasentered %in% "20-24" ~ ageasentered,
      TRUE ~ "25+")) %>% 
    group_by(fiscal_year, 
             #funding_agency, 
             operatingunit, country, indicator, snu1, age) %>%
    #summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    #summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    summarise(qtr1 = sum(qtr1, na.rm = TRUE), 
              qtr2 = sum(qtr2, na.rm = TRUE),
              qtr3 = sum(qtr3, na.rm = TRUE),
              qtr4 = sum(qtr4, na.rm = TRUE),
              .groups = "drop") 
  
  # Reshape long and calculate VLC/S
  df_vl <- df_tx %>% 
    reshape_msd() %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    rename_with(str_to_lower) %>% 
    group_by(
      #funding_agency, 
      operatingunit, country, snu1, age) %>% 
    mutate(
      vlc = tx_pvls_d / dplyr::lag(tx_curr, 2, order_by = period),
      vls = tx_pvls / tx_pvls_d) %>% 
    ungroup()
  
  ## Limits history to last 5 quaters
  
  hist_pds <- df_vl %>% 
    distinct(period) %>% 
    arrange(desc(period)) %>% 
    pull() 
  
  # reset pd hisory to 4 for anything outside of 2:8
  if (pd_hist <= 1 | pd_hist > length(hist_pds)) {
    usethis::ui_warn(glue::glue("History length ({pd_hist}) is behind 1 and {length(hist_pds)}. Value was reset to 4."))
    pd_hist <- 4
  }
  
  hist_pds <- hist_pds %>% 
    magrittr::extract(1:pd_hist) %>% 
    sort()
  
  df_vl %>% filter(period %in% hist_pds)
}

# Adapted from hardapoart!

prep_txcoverage_age_sex <- function(df_nat, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$cntry))
    return(NULL)
  
  ind_sel <- c("PLHIV", "DIAGNOSED_SUBNAT" ,"TX_CURR_SUBNAT", "VL_SUPPRESSION_SUBNAT")
  
  df_gap <- df %>% 
    dplyr::filter(country %in% cntry,
                  fiscal_year == max(fiscal_year),
                  indicator %in% ind_sel,
                  standardizeddisaggregate == "Age/Sex/HIVStatus") 
  
  if(nrow(df_gap) == 0)
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::count(country, indicator, ageasentered, sex, wt = targets, name = "value") %>% 
    tidyr::pivot_wider(names_from = indicator,
                       names_glue = "{tolower(indicator)}") 
  
  if("plhiv" %ni% names(df_gap))
    return(NULL)
  
  df_gap <- df_gap %>% 
    dplyr::mutate(cov_status = diagnosed_subnat/plhiv,
                  cov_tx = tx_curr_subnat/plhiv)
  
  df_viz <- df_gap %>% 
    dplyr::mutate(plhiv_marker = dplyr::case_when(tx_curr_subnat > plhiv ~ plhiv),
                  fill_color = ifelse(sex == "Male", glitr::genoa, glitr::moody_blue)) %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(ctry_name = glue::glue("{unique(df_gap$country)}<br>{label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(tx_curr_subnat, na.rm = TRUE))}/{label_number(accuracy = .1, scale_cut = cut_short_scale())(sum(plhiv, na.rm = TRUE))}"),
                  lab_gap = scales::percent(cov_tx, 1)) %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(cntry = country)
  
  return(df_viz)
  
} 

prep_txnetnew_age_sex <- function(df_psnu, cntry) {
  
  #clean exit if no data
  if(cntry %ni% unique(df_psnu$country))
    return(NULL)
  
  clean_number <- function(x, digits = 0){
    dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                     x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                     x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                     TRUE ~ glue("{x}"))
  }
  
  df <- df_psnu %>% 
    dplyr::filter(country %in% cntry,
                 fiscal_year == max(fiscal_year),
                  indicator == "TX_NET_NEW",
                  standardizeddisaggregate == "Age/Sex/HIVStatus") %>% 
    dplyr::group_by(country, fiscal_year, indicator, age_2019, sex) %>% 
    dplyr::summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
                     .groups = "drop") %>%
    gophr::reshape_msd() 
  
  df_viz <- df %>% 
    dplyr::mutate(ctry_name = glue::glue("{unique(df$country)}<br>")) %>%
    dplyr::mutate(fill_color = ifelse(sex == "Male", glitr::genoa_light, glitr::moody_blue_light)) %>% 
    dplyr::rename(cntry = country) %>% 
    dplyr::group_by(cntry, period, indicator, age_2019, sex) %>% 
    dplyr::mutate(val_lab = glue::glue("{clean_number(value)}")) %>% 
    dplyr::ungroup()
  
  
  return(df_viz)
  
  
}

# Adapted from hardapoart
# vlc/s gaps between different population groups by PSNU
prep_viral_load_kp_agyw <- function(df, cntry){
  
  young <- c("10-14","15-19", "20-24") #DREAMS AGYW age band based on DREAMS guidance
  
  #filter to select indicators + country
  df_vl <- df %>%  
    clean_indicator() %>%
    filter(indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
           country == cntry)
  
  #clean exit for missing data
  if(nrow(df_vl) == 0)
    return(NULL)
  
  #create overall value
  df_vl <- df_vl %>% 
    bind_rows(df_vl %>% 
                mutate(psnu = "OVERALL"))
  
  #clean define groups - AGYW, Non-AGYW
  df_vl <- df_vl %>% 
    mutate(type = case_when(sex=="Female" & ageasentered %in% young ~ "AGYW",
                            str_detect(standardizeddisaggregate, "Total|KeyPop", negate = TRUE) ~ "Non-AGYW",
                            TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")))
  
  #aggregate & reshape long
  df_vl <- df_vl %>% 
    group_by(fiscal_year, country, psnu, indicator, type) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  #pivot wide in order to subtract KP from GP (Total)
  df_vl <- df_vl %>% 
    pivot_wider(names_from = type,
                values_fill = 0) 
  
  #create KeyPop if missing
  if("KeyPop" %ni% names(df_vl))
    df_vl <- mutate(df_vl, KeyPop = 0)
  
  #subtract KP from GP (Total)
  df_vl <- df_vl %>%
    mutate(GenPop = Total - KeyPop,
           GenPop = ifelse(GenPop < 0, 0, GenPop)) %>% 
    select(-Total) %>% 
    pivot_longer(-where(is.character),
                 names_to = "type") %>% 
    mutate(group = case_when(type %in% c("KeyPop", "GenPop") ~ "KP-GP",
                             type %in% c("AGYW", "Non-AGYW") ~ "AGYW"),
           .before = type)
  
  #reshape wider by indicator and create lag for VLC
  df_vl <- df_vl %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(country, psnu, group, type) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, order_by = period)) %>% 
    ungroup()
  
  #calculate VLC/S
  df_vl <- df_vl %>%
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d) %>% 
    filter(!is.nan(vlc))
  
  #limit to latest period
  df_vl <- filter(df_vl, period == max(period))
  
  #color
  df_viz <- df_vl %>% 
    select(-c(tx_pvls, tx_pvls_d, tx_curr_lag2)) %>% 
    mutate(fill_color = case_when(type == "KeyPop" ~ scooter,
                                  type == "AGYW" ~ genoa,
                                  TRUE ~ grey30k)) %>% 
    pivot_longer(c(vls, vlc), 
                 names_to = "indicator") %>% 
    mutate(group_viz = ifelse(group == "AGYW",
                              glue("**<span style='color:{genoa}'>AGYW</span> vs <span style='color:{grey30k}'>non-AGYW</span> {toupper(indicator)}**"),
                              glue("**<span style='color:{scooter}'>KeyPop</span> vs <span style='color:{grey30k}'>GenPop</span> {toupper(indicator)}**")))
  
  return(df_viz)
}

#Adapted from hardapoart
# vlc/s trends by age and snu
prep_viral_load_snu_age <- function(df, cntry, pd_hist = 5) {
  
  #clean exit if no data
  if(cntry %ni% unique(df$country))
    return(NULL)
  
  # Filter
  df_tx <- df %>% 
    clean_indicator() %>%
    filter(
      country == cntry,
      indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
      standardizeddisaggregate %in% c("Age/Sex/HIVStatus", 
                                      "Age/Sex/Indication/HIVStatus")) %>%
    mutate(snu1 = stringr::str_replace_all("_Military South Sudan", 
                                             "Military South Sudan"))
  
  if(nrow(df_tx) == 0)
    return(NULL)
  
  # Summarise results by age - bands
  df_tx <- df_tx %>%
    select(-cumulative, -targets) %>% 
    filter(ageasentered != "Unknown Age") %>% 
    mutate(age = case_when(
      trendscoarse == "<15" ~ trendscoarse,
      ageasentered == "15-19" ~ ageasentered,
      ageasentered %in% "20-24" ~ ageasentered,
      TRUE ~ "25+")) %>% 
    group_by(fiscal_year, operatingunit, country, snu1, indicator, age) %>%
    #summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>%
    #summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
    summarise(qtr1 = sum(qtr1, na.rm = TRUE), 
              qtr2 = sum(qtr2, na.rm = TRUE),
              qtr3 = sum(qtr3, na.rm = TRUE),
              qtr4 = sum(qtr4, na.rm = TRUE),
              .groups = "drop") %>%
    reshape_msd() 
  
  if(nrow(df_tx) == 0)
    return(NULL)
  
  # Reshape long and calculate VLC/S
  df_vl <- df_tx %>% 
    select(-period_type) %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    rename_with(str_to_lower) %>% 
    group_by(operatingunit, country, snu1, age) %>% 
    mutate(
      vlc = tx_pvls_d / dplyr::lag(tx_curr, 2, order_by = period),
      vls = tx_pvls / tx_pvls_d, 
      age_snu = glue::glue("{snu1} | {age}")) %>% 
    ungroup()
  
  ## Limits history to last 5 quaters
  
  hist_pds <- df_vl %>% 
    distinct(period) %>% 
    arrange(desc(period)) %>% 
    pull() 
  
  # reset pd hisory to 4 for anything outside of 2:8
  if (pd_hist <= 1 | pd_hist > length(hist_pds)) {
    usethis::ui_warn(glue::glue("History length ({pd_hist}) is behind 1 and {length(hist_pds)}. Value was reset to 4."))
    pd_hist <- 4
  }
  
  hist_pds <- hist_pds %>% 
    magrittr::extract(1:pd_hist) %>% 
    sort()
  
  df_vl %>% 
    filter(period %in% hist_pds)
}

# VIZ ---------------------------------------------------------------------

# Adapted from hardapoart
# tx_curr by age ands sex and txnetnew by age and sex
viz_txcoverage_age_sex <- function(df) {
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "fe2dcee4"
  
  df %>% 
    ggplot2::ggplot(aes(plhiv, ageasentered, fill = fill_color, color = fill_color)) +
    ggplot2::geom_blank(aes(plhiv*1.1)) +
    ggplot2::geom_col(fill = NA, width = .8, alpha = .8) +
    ggplot2::geom_col(aes(tx_curr_subnat), width = .8, alpha = .8) +
    ggplot2::geom_errorbar(aes(xmin = plhiv_marker, xmax = plhiv_marker), 
                           na.rm = TRUE, color = "white", linetype = "dotted") +
    ggplot2::geom_text(aes(label = lab_gap), na.rm = TRUE,
                       family = "Source Sans Pro", color = glitr::suva_grey,
                       size = 10/.pt, hjust = -.5) +
    ggplot2::facet_grid(sex ~ forcats::fct_reorder(ctry_name, plhiv, sum, na.rm = TRUE, .desc = TRUE),
                        switch = "y", scales = "free_x"
    ) +
    ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
                                expand = c(.005, .005)) +
    ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
    ggplot2::labs(x = NULL, y = NULL,
                  title = glue::glue("{metadata_natsubnat$curr_fy_lab} {unique(df$cntry)} Treatment coverage gaps") %>% toupper,
                  subtitle = "TX_CURR_SUBNAT coverage of PLHIV by age and sex",
                  caption = " ") +
    ggplot2::coord_cartesian(clip = "off") +
    glitr::si_style_xgrid() +
    ggplot2::theme(strip.text.y = element_text(hjust = .5),
                   strip.text.x = element_markdown(),
                   strip.placement = "outside",
                   panel.spacing.x = unit(1, "lines"),
                   panel.spacing.y = unit(.5, "lines")
    )
  
}

viz_txnetnew_age_sex <- function(df) {
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "fe2dcee4"
  vrsn <- 1 
  
  df %>% 
    dplyr::filter(age_2019 != "Unknown Age",
                  period == metadata_msd_psnu$curr_pd) %>% 
    ggplot2::ggplot(aes(value, age_2019, fill = fill_color, color = fill_color)) +
    ggplot2::geom_col(aes(fill = fill_color), width = .8, alpha = .8) +
    #facet_wrap(~sex, switch = "y", scales = "free_x") +
    ggplot2::facet_grid(sex ~ forcats::fct_reorder(ctry_name, value, sum, na.rm = TRUE, .desc = TRUE),
                        switch = "y", scales = "free_x"
    ) +
    ggplot2:: geom_text(aes(label = val_lab), na.rm = TRUE,
                        family = "Source Sans Pro", color = suva_grey,
                        size = 10/.pt, hjust = -.5) +
    ggplot2::scale_fill_identity(aesthetics = c("fill", "color")) +
    ggplot2::scale_x_continuous(label = scales::label_number(scale_cut = cut_short_scale()),
                                expand = c(.005, .005)) +
    glitr::si_style_xgrid() +
    ggplot2::labs(x = NULL, y = NULL,
                  title = glue::glue("{metadata_msd_psnu$curr_pd} {unique(df$cntry)} quarterly net change in ART patients") %>% toupper,
                  subtitle = "TX_NET_NEW by age and sex",
                  caption = glue::glue("{metadata_msd_psnu$caption} | USAID/OHA/SIEI |  Ref Id: {ref_id} v{vrsn}")) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      strip.text.y = element_blank(), #element_text(hjust = .5),
      strip.text.x = element_markdown(),
      strip.placement = "outside",
      panel.spacing.x = unit(1, "lines"),
      panel.spacing.y = unit(.5, "lines"),
      axis.text.y = element_blank())
}

viz_tx_all <- function(cntry) {
  
  v1 <- prep_txcoverage_age_sex(df_nat, cntry) %>% 
    viz_txcoverage_age_sex()
  
  v2 <- prep_txnew_age_sex(df_psnu, cntry) %>% 
    viz_txnew_age_sex()
  
  if(is.null(v1) && is.null(v2)){
    viz_tx <- NULL
    print(paste("No data available."))
  } else if(is.null(v2)){
    viz_tx <- v1
  } else if(is.null(v1)){
    viz_tx <- v2
  } else {
    suppressWarnings(
      viz_tx <- cowplot::plot_grid(v1, v2, ncol = 2, align = 'v')
    )
  }
  
  return(viz_tx)
  
  
}

#(v1 + v2) + plot_layout(widths = c(2, 1), heights = c(10))

# Adapted from hardapoart
# Viz TX VLC/S Gaps by PSNU

viz_viral_load_kp_agyw <- function(df){
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "02dc818a" #id for adorning to plots, making it easier to find on GH
  
  vrsn <- 1 
  
  cap_note <- ifelse(nrow(df) > 21, "| Limited to the largest 20 TX_CURR PSNUs\n", "")
  
  #limit to 21 bars (overall + 20 psnus)
  v_top <- df %>% 
    filter(group == "AGYW") %>% 
    count(psnu, wt = tx_curr, sort = TRUE) %>% 
    slice_head(n = 21) %>% 
    pull(psnu)
  
  df <- filter(df, psnu %in% v_top) 
  
  #viz
  df %>% 
    ggplot(aes(value, fct_reorder(psnu, tx_curr, max, na.rm = TRUE), color = fill_color, group = psnu)) +
    geom_vline(xintercept = 0, color = "#D3D3D3") +
    geom_vline(xintercept = 1, color = "#D3D3D3", linetype = "dashed") +
    geom_line(color = "#d3d3d3", na.rm = TRUE) +
    geom_point(size = 2, color = "white", na.rm = TRUE) +
    geom_point(size = 2, alpha = .6, na.rm = TRUE) +
    scale_color_identity() +
    facet_wrap(~group_viz, nrow = 1) +
    scale_x_continuous(labels = scales::percent, name = NULL, 
                       limits = c(0,1.1), 
                       breaks = seq(0,1.1, by = .25),
                       oob = oob_squish) + 
    labs(x = NULL, y = NULL,
         subtitle = glue("{unique(df$period)} {unique(df$country)} VLC/S gaps between different population groups"),
         caption = glue("Note: VL capped at 110% {cap_note}{metadata_msd_psnu$caption} | USAID/OHA/SIEI | Ref id: {ref_id} v{vrsn}")) +
    si_style_xline() +
    theme(legend.position = "none",
          strip.text = element_markdown(),
          panel.spacing = unit(.5, "picas"))
  
}

#Adapted from hardapoart
# vlc/s trends by age and snu
viz_viral_load_snu <- function(df, save = F) {
  
  if(is.null(df) || nrow(df) == 0)
    return(print(paste("No data available.")))
  
  ref_id <- "5ca95c24"
  vrsn <- 1 
  
  # Filter all SNUs with > 95% VLS
  n_snu <- df %>% 
    distinct(snu1) %>% 
    pull() %>% 
    length()
  
  cap_note <- ""
  
  if (n_snu > 12) {
    snus <- df %>% 
      dplyr::filter(period == max(period), vls < .95) %>% 
      dplyr::slice_max(order_by = vls, n = 12)  %>% 
      dplyr::distinct(snu1) %>% 
      dplyr::pull()
    
    df <- df %>% 
      dplyr::filter(snu1 %in% snus)
    
    cap_note <- "Note: Limited to the 12 lowest PSNUs with VLS < 95%\n"
  }
  
  # Generate the plot
  viz <- df %>% 
    ggplot(aes(x = period, group = 1)) +
    geom_line(aes(y = vlc), color = burnt_sienna, linewidth = 1, na.rm = TRUE) +
    geom_point(aes(y = vlc), fill = burnt_sienna, color = grey10k, 
               shape = 21, size = 3.5, na.rm = TRUE) +
    geom_text_repel(aes(y = vlc, label = percent(vlc, 1)), 
                    family = "Source Sans Pro", max.overlaps = 50, force = 10,
                    size = 3, color = burnt_sienna, na.rm = TRUE) +
    geom_line(aes(y = vls), color = genoa, linewidth = 1, na.rm = TRUE) +
    geom_point(aes(y = vls), fill = genoa, color = grey10k, 
               shape = 21, size = 3.5, na.rm = TRUE) +
    geom_text_repel(aes(y = vls, label = percent(vls, 1)), 
                    family = "Source Sans Pro", max.overlaps = 50, force = 10,
                    size = 3, color = genoa, na.rm = TRUE) +
    scale_y_continuous(labels = percent) +
    labs(x = "", y = "",
         title = glue::glue("{toupper(unique(df$country))} - VIRAL LOAD TRENDS by SNU AND AGE"),
         subtitle = glue::glue("VL <span style='color:{burnt_sienna}'>Coverage</span> & <span style='color:{genoa}'>Suppression</span> for the last 5 quarters"),
         caption = glue::glue("{metadata_msd_psnu$caption} | USAID/OHA/SIEI |  Ref id: {ref_id} v{vrsn}")) +
    coord_cartesian(clip = "off") +
    facet_wrap(~snu1, ncol = 4) +
    si_style_nolines() +
    theme(plot.title = element_markdown(),
          plot.subtitle = element_markdown(),
          axis.text.y = element_blank(),
          strip.text = element_text(size = 12, face = "bold"),
          strip.clip = "off",
          strip.placement = "outside",
          panel.spacing = unit(0, "lines"))
  
  print(viz)
  
  if (save) {
    glitr::si_save(
      plot = viz,
      filename = glue::glue("./Images/{max(df$period)} - {toupper(unique(df$country)} VLCS Trends by SNU and Age.png"))
  }
}