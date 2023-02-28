# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Identify changes in TX_CURR program targets between COP19-20
# REF ID:   16fbc66a 
# LICENSE:  MIT
# DATE:     2023-02-28
# NOTES: Data obtained from here:
# https://tableau.usaid.gov/#/views/OHATreatmentWorkbook/AchievementTrends_AgencyIM?:iid=3

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(readr)
  library(scales)
  library(ggtext)
  library(glue)
  library(janitor)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "16fbc66a"

# IMPORT ----------------------------------------------------------------------
  
  
  path <- "Data/achievement_cop19_20.csv"
  
  df <- read_csv(path, show_col_types = FALSE)

# MUNGE ------------------------------------------------------------------------
  
  # How have targets changed between FY20-21/COP19-20?
  
  df_filt <- df %>%
    clean_names() %>%
    rename(agency = funding_agency_usaid_cdc_do_d_pc_other) %>%
    select(-ends_with("_percent_ach")) %>%
    mutate(
     # ((V2 - V1) / V1) × 100
      pct_change = ((fy21_targets - fy20_targets)/fy20_targets))  %>%
    pivot_longer(ends_with("_results") | ends_with("_targets"), 
                 names_sep = "_", names_to = c("fiscal_year", "type")) %>%
    pivot_wider(
      names_from = type) %>%
    filter(fiscal_year %in% c("fy20", "fy21")) %>%
    mutate(
      fiscal_year = if_else(fiscal_year == "fy20", "2020", "2021"), 
      agency_indicator = glue("{indicator} ({agency})"))
  
# VIZ --------------------------------------------------------------------------
  
  # each agency, different y axis
  
  df_filt %>%
    ggplot(aes(x = fiscal_year, color = agency, 
               fill = agency, group = agency)) +
    geom_area(aes(y = targets, alpha = 0.4)) +
    geom_text(aes(y = targets, label = if_else(fiscal_year == "2021", percent(pct_change), "")),
              #vjust = -0.3,
              color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~agency_indicator, scales = "free_y") +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown()) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title = "Between COP19/FY20 - COP20/FY21, Targets increased for USAID in TX_CURR and TX_NEW, 
               CDC and DOD saw increases in TX_NEW and TX_CURR respectively ", 
      subtitle = "Percent Change in TX_CURR target between COP19/FY20 - COP20/FY21 labelled",
      caption = glue("Source: OHA Treatment Workbook, MSD |{ref_id} | OHA SI | US Agency for International Development"))
  
  si_save("Images/TargetChange_20_21_different_yaxes.png")
  
  # same graph, same y-axis
  
  df_filt %>%
    ggplot(aes(x = fiscal_year, color = agency, 
               fill = agency, group = agency)) +
    geom_area(aes(y = targets, alpha = 0.4)) +
    geom_text(aes(y = targets, label = if_else(fiscal_year == "2021", percent(pct_change), "")),
              color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~agency_indicator) +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown()) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title = glue("Between COP19/FY20 - COP20/FY21, Targets increased for USAID in TX_CURR and TX_NEW"), 
      subtitle = "CDC and DOD saw increases in TX_NEW and TX_CURR respectively",
      caption = glue("Note: Percent Change in target between COP19/FY20 - COP20/FY21 labelled
                      Source: OHA Treatment Workbook, MSD |{ref_id} | OHA SI | US Agency for International Development"))
  
  si_save("Images/TargetChange_20_21_same_yaxis.png")
  