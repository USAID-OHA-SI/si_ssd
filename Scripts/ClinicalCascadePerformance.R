## PROJECT:  SI Support for SSD
## AUTHOR:   J. Hoehner | USAID
## LICENSE:  MIT
## PURPOSE:  SSD - Clinical Cascade Performance
## REF ID:   "f5d11198"
## Date:     2022-11-03
## Update:   2022-11-03

# LIBRARIES --------------------------------------------------------------------

    library(tidyverse)
    library(cascade)
    library(gagglr)
    library(extrafont)
    library(glue)
    library(here)

# Global Variables -------------------------------------------------------------

    # reference id for this script
    # it is used in the filename of any output visuals so that we can find the
    # script used to produce it in our GitHub repository in the future
    ref_id <- "f5d11198"

    # path to all MER data files
    data <- here("Data/MER/")

    # path to current OUxIM data file
    file_path <- return_latest(folderpath = data,
                               pattern = "Genie_OU_IM_South_Sudan_Frozen_Q4")

# Data -------------------------------------------------------------------------

    # get the data
    df_ssd <- read_msd(file_path) 
    
    # %>%
    #   filter(indicator %in% c("HTS_TST", "HTS_TST_POS", 
    #                           "TX_NEW", "TX_NET_NEW, 
    #                           "))

    # get information about the data for the final graph
    get_file_metadata(file_path)

# Choose a Cascade -------------------------------------------------------------

    plot_name

    # [1] "Standard"              # [2] "Standard Female"
    # [3] "Standard Male"         # [4] "Pediatric" # [5] "Pediatric Female"
    # [6] "Pediatric Male"        # [7] "AYP (15-24 years old)"
    # [8] "AYP Female"            # [9] "AYP Male"
    # [10] "Adults"               # [11] "Adults Female"
    # [12] "Adults Male"          # [13] "KP"

    # Return a cascade data frame (number corresponds to position in list)
    # 13 = KP cascade
    kp_cascade_data <- return_cascade(df_ssd, 13)
    

# VIZ --------------------------------------------------------------------------

    # Plot the cascade
    # You will be prompted to enter a cascade number
    return_cascade_plot(df_ssd, export = F)

    # Un-comment and enter the name of the plot you selected in between the ""
    # ex: plot_file_name = "KP_Cascade_FY22Q3"
    plot_file_name = "KP_Cascade_FY22Q4"

    # save the plot to the Images folder
    si_save(here(glue("Images/{plot_file_name}_{ref_id}.png")),
            height = 9,
            width = 13)
    