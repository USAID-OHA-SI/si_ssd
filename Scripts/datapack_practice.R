# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  investigate datapack using tamedp
# REF ID:   b9174f31 
# LICENSE:  MIT
# DATE:     2023-01-04
# UPDATED: 

# DEPENDENCIES ----------------------------------------------------------------
  
  library(tidyverse)
  library(extrafont)
  library(gagglr)
  library(tameDP)
  library(readxl)

# GLOBAL VARIABLES ------------------------------------------------------------
  
  ref_id <- "b9174f31"

# IMPORT ----------------------------------------------------------------------
  
  path <- "Data/Mozambique_datapack_Finalized_20220425_083344_vmmc_Corrected.xlsx"
  
  df <- tame_dp(path)
  
  df_plhiv <- tame_dp(path, type = "PLHIV")

# MUNGE -----------------------------------------------------------------------