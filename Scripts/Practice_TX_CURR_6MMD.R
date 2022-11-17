# PROJECT:
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:
# REF ID:   b9c45ac5
# LICENSE:  MIT
# DATE:     2022-11-17
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(glamr)
library(gophr)
library(scales)
library(glue)

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- "b9c45ac5"

# IMPORT ------------------------------------------------------------------

ou_df <- si_path() %>%
  return_latest("Genie_OU_IM_South_Sudan_Daily") %>%
  read_msd()

# MUNGE -------------------------------------------------------------------

# FY22 Q4 TX_CURR,
# for 15+, share of 6MMD /TX_CURR
ou_count <- ou_df %>%
  filter(
    indicator == "TX_CURR",
    fiscal_year == 2022,
    standardizeddisaggregate %in% c("Age/Sex/ARVDispense/HIVStatus",
                                    "Age/Sex/HIVStatus"),
    trendscoarse == "15+") %>%
  mutate(
    otherdisaggregate = otherdisaggregate %>%
      str_remove_all("(ARV Dispensing Quantity - | months)") %>%
      str_replace_all(" ", "_"),
    otherdisaggregate = ifelse(is.na(otherdisaggregate), "tx_curr",
      otherdisaggregate)) %>%
  # wt = cumulative should give the same result as wt = targets
  count(fiscal_year, indicator, otherdisaggregate, 
        trendscoarse, wt = cumulative) %>%
  pivot_wider(
    names_from = otherdisaggregate,
    names_glue = "mmd_{tolower(otherdisaggregate)}",
    values_from = n) %>%
  rename(tx_curr = mmd_tx_curr) %>%
  mutate(across(starts_with("mmd"), ~ . / tx_curr,
    .names = "pct_{.col}"))

glue("Of the {comma(df_mmd$tx_curr)} adult patients on treatment in
      {df_mmd$fiscal_year}, {percent(df_mmd$pct_mmd_6_or_more, 1)} are on 6 months of MMD.")
