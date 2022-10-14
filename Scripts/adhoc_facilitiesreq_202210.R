# PROJECT:  ad-hoc requests
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  identify sites not in list
# REF ID:   2973ad33 
# LICENSE:  MIT
# DATE:     2022-10-14
# UPDATED:  2022-10-14

# DEPENDENCIES -----------------------------------------------------------------

  library(tidyverse)
  library(extrafont)
  library(glitr)
  library(glamr)
  library(gophr)
  library(gisr)
  library(grabr)
  library(googlesheets4)
  library(janitor)
  library(stringr)

# GLOBAL VARIABLES -------------------------------------------------------------
  
  ref_id <- "2973ad33"

# IMPORT -----------------------------------------------------------------------

  ou <- "South Sudan"
  level_fac <- grabr::get_ouorglevel(operatingunit = ou, 
                              org_type = "facility")
  
  df_facs <- extract_locations(country = ou, level = level_fac) %>% 
    extract_facilities()
  
  pepfar_facilities_jc <- read_sheet("1R8hHmITd9dAPWLzZd_yKVV-ctjMXCnS1nHWtokUyYh8") %>%
    clean_names()

# MUNGE ------------------------------------------------------------------------

  pepfar_loc <- pepfar_facilities_jc %>%
    mutate(
      dataset_jc = "yes", 
      name = str_to_lower(facility))
  
  combined <- df_facs %>%
    select(name, longitude, latitude) %>%
    mutate(
      name = str_to_lower(name),
      name = str_replace(name, "primary health care centre", "phcc"),
      name = str_replace(name, "primary health care unit", "phcu"),
      name = str_replace(name, "kapoeta state hospital", "kapoeta  hospital"), 
      name = str_replace(name, "maridi county hospital", "maridi  hospital"), 
      name = str_replace(name, "namaiku phcu", "namaiku phcc"), 
      name = str_replace(name, "rumbek state hospital", "rumbek  hospital"), 
      name = str_replace(name, "yirol state hospital", "yirol  hospital"), 
      dataset_datim = "yes") %>%
    full_join(pepfar_loc, 
              by = "name") %>%
    mutate(
      dataset_jc = if_else(is.na(dataset_jc) == TRUE, "no", dataset_jc),
      dataset_datim = if_else(is.na(dataset_datim) == TRUE, "no", dataset_datim)) %>%
    select(name, longitude, latitude, dataset_datim, dataset_jc)

  write_sheet(combined, "1gSC0s6OUO3ENQHVwM-V_V4NsOGhtO0ZvQe3EOr4IeLQ",
              "combined")