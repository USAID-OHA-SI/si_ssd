# PROJECT:  sid_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  checking DREAMS age disaggs
# REF ID:   c5edc0f2 
# LICENSE:  MIT
# DATE:     2022-12-13
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(janitor)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "c5edc0f2"

# IMPORT ----------------------------------------------------------------------
  
  path <- "PSNU_IM_DREAMS_FY20-23"
  
  df <- si_path() %>%
    return_latest(path) %>%
    read_msd()

# MUNGE -----------------------------------------------------------------------

  df_filt <- df %>%
    filter(country == "South Sudan", 
           indicator == "AGYW_PREV", 
           standardizeddisaggregate == "Age/Sex/Time/Complete", 
           ageasentered != "Unknown Age") %>%
    clean_indicator()
    reshape_msd() %>%
    select(ageasentered, otherdisaggregate)
  