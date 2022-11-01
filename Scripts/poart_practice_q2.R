# PROJECT:  SSD POART recreation
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  to practice recreating visuals from Q2 POARTs
# REF ID:   4bbb695a 
# LICENSE:  MIT
# DATE:     2022-11-01
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(glitr)
  library(glamr)
  library(gophr)
  library(glue)
  library(janitor)

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "4bbb695a"

# IMPORT ------------------------------------------------------------------
  
  df <- si_path() %>%
    return_latest("Genie_PSNU_IM_South_Sudan_Frozen_c625785e-d0a8-4c73-9d02-6fe786882ad4") %>%
    read_tsv(guess_max = 120000, show_col_types = FALSE) %>%
    select(funding_agency, snu1, psnu, typemilitary, 
           prime_partner_name, mech_name, modality,
           standardizeddisaggregate, indicator, trendscoarse, 
           sex, fiscal_year, qtr1:cumulative)

# MUNGE -------------------------------------------------------------------
  
# case-finding by mechanism over time, slide 5
  
  bymech <-  df  %>%
    filter(indicator %in% c("HTS_TST", "HTS_TST_POS"), 
           funding_agency == "USAID", 
           standardizeddisaggregate == "Total Numerator") %>%
    select(indicator, mech_name, fiscal_year, qtr1:cumulative) %>%
    pivot_longer(qtr1:cumulative, 
                 names_to = "quarter", values_to = "value") %>%
    mutate(period = as.character(glue("{fiscal_year} - {quarter}"))) %>%
    pivot_wider(names_from = c(mech_name, indicator, period),
                values_from = value)
  
  
    filter(quarter != "cumulative") %>%
    drop_na(value) %>%
    group_by(mech_name, period) %>%
    summarize(cum_val = round_half_up(mean(value), 0))
  
  ggplot(bymech, 
         aes(x = period, y = cum_val, 
             group = mech_name, 
             color = mech_name)) +
    geom_line() +
    scale_y_continuous(limits = c(0, 20))

# case-finding trends by modality, IP
  
  bymod_ip <-  df  %>%
    filter(indicator == "HTS_TST", 
           funding_agency == "USAID") %>%
    select(mech_name, modality, fiscal_year, qtr1:cumulative) %>%
    pivot_longer(qtr1:cumulative, 
                 names_to = "quarter", values_to = "value") %>%
    mutate(period = as.character(glue("{fiscal_year} - {quarter}"))) %>%
    filter(quarter != "cumulative") %>%
    drop_na(value) %>%
    group_by(mech_name, modality, period) %>%
    summarize(cum_val = round_half_up(sum(value, 0)))
  
# IIT
  
  df_iit <- df  %>%
    filter(indicator %in% c("TX_RTT", "TX_ML_IIT_six_more_mo"), 
           funding_agency == "USAID") %>%
    select(indicator, fiscal_year, qtr1:cumulative) %>%
    pivot_longer(qtr1:cumulative, 
                 names_to = "quarter", values_to = "value") %>%
    mutate(period = as.character(glue("{fiscal_year} - {quarter}"))) %>%
    filter(quarter != "cumulative") %>%
    drop_na(value) %>%
    group_by(indicator, period) %>%
    summarize(cum_val = round_half_up(sum(value, 0)))
  
  ggplot(df_iit, 
         aes(x = period, y = cum_val, , 
             fill = indicator)) +
    geom_col() +
    scale_y_continuous(limits = c(-4000, 4000))
  
# TX_CURR among military
  
  df_curr <- df  %>%
    filter(indicator == "TX_CURR", 
           typemilitary == "Y", 
           fiscal_year == "2022", 
           standardizeddisaggregate == "Total Numerator") %>%
    pivot_longer(qtr1:cumulative, 
                 names_to = "quarter", values_to = "value") %>%
    mutate(period = as.character(glue("{fiscal_year} - {quarter}"))) %>%
    filter(quarter == "qtr1") %>%
    drop_na(value) %>%
    select(indicator, period, value) %>%
    group_by(indicator, period, value) %>%
    summarize(cum_val = round_half_up(sum(value, 0)))
  
  
  
  
  
  