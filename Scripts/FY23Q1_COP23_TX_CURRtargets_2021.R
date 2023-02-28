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

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "16fbc66a"

# IMPORT ----------------------------------------------------------------------
  
  
  path <- "Data/tx_curr_cop19_20.csv"
  
  df <- read_csv(path) %>%
    mutate(value = as.numeric(value), 
           fiscal_year = as.character(fiscal_year))

# MUNGE ------------------------------------------------------------------------
  
  # How have targets changed between FY20-21/COP19-20?
  
  targets <- df %>%
    filter(type == "target")
  
  df_pivot <- df %>%
    pivot_wider(
      names_from = type) %>%
    pivot_wider(
      names_from = fiscal_year, 
      values_from = result:achv) %>%
    mutate(
     # ((V2 - V1) / V1) × 100
      pct_change = ((target_2021 - target_2020)/target_2020))  %>%
    pivot_longer(starts_with("result_") | starts_with("target_") |starts_with("achv_"), 
                 names_sep = "_", names_to = c("type", "fiscal_year")) %>%
    pivot_wider(
      names_from = type) 
# VIZ --------------------------------------------------------------------------
  
  # each agency, different y axis
  
  df_pivot %>%
    ggplot(aes(x = fiscal_year, color = agency, 
               fill = agency, group = agency)) +
    geom_area(aes(y = target, alpha = 0.4)) +
    geom_text(aes(y = target, label = if_else(fiscal_year == "2021", percent(pct_change), "")),
              vjust = -0.65,
              color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~agency, scales = "free_y") +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown()) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title = "Between COP19/FY20 - COP20/FY21, TX_CURR Targets increased for USAID and DOD, decreased for CDC", 
      subtitle = "Percent Change in TX_CURR target between COP19/FY20 - COP20/FY21 labelled",
      caption = glue("Source: OHA Treatment Workbook, MSD |{ref_id} | OHA SI | US Agency for International Development"))
  
  si_save("Images/TX_CURR_TargetChange_20_21_different_yaxes.png")
  
  # same graph, same y-axis
  
  df_pivot %>%
    ggplot(aes(x = fiscal_year, color = agency, 
               fill = agency, group = agency)) +
    geom_area(aes(y = target, alpha = 0.4)) +
    geom_text(aes(y = target, label = if_else(fiscal_year == "2021", percent(pct_change), "")),
              vjust = -0.65,
              color = "#202020", family = "Source Sans Pro", size = 10/.pt) +
    scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
    facet_wrap(~agency) +
    si_style_yline() +
    theme(
      panel.spacing = unit(.5, "line"),
      legend.position = "none",
      plot.title = element_markdown(),
      strip.text = element_markdown()) +
    labs(
      x = NULL, y = NULL, fill = NULL,
      title = "Between COP19/FY20 - COP20/FY21, TX_CURR Targets increased for USAID and DOD, decreased for CDC", 
      subtitle = "Percent Change in TX_CURR target between COP19/FY20 - COP20/FY21 labelled",
      caption = glue("Source: OHA Treatment Workbook, MSD |{ref_id} | OHA SI | US Agency for International Development"))
  
  si_save("Images/TX_CURR_TargetChange_20_21_same_yaxis.png")
  