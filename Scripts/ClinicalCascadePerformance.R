## PROJECT:  SI Support for SSD
## AUTHOR:   J. Hoehner | USAID
## LICENSE:  MIT
## PURPOSE:  SSD - Clinical Cascade Performance
## REF ID:   "f5d11198"
## Date:     2022-11-03
## Update:   2022-11-09

# LIBRARIES --------------------------------------------------------------------

    library(tidyverse)
    library(cascade)
    library(gagglr)
    library(extrafont)
    library(glue)

# Global Variables -------------------------------------------------------------

    # reference id for this script
    # it is used in the filename of any output visuals so that we can find the
    # script used to produce it in our GitHub repository in the future
    ref_id <- "f5d11198"

    # path to current OUxIM data file
    file_path <- si_path() %>% 
      return_latest("Genie_OU_IM_South_Sudan_Daily")
    
# Data -------------------------------------------------------------------------

    # get the data
    df_ssd <- read_msd(file_path)
    
    # get information about the data for the final graph
    get_metadata(file_path)

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

    # Un-comment and enter the name of the plot you selected in between " and the first _
    # ex: If you select plot 13, plot_file_name = glue("KP_Cascade_{metadata$curr_pd})
    plot_file_name = glue("SSD_Std_Cascade_{metadata$curr_pd}")
    # 13 = glue("KP_Cascade_{metadata$curr_pd}")
    # 1 =  glue("Standard_Cascade_{metadata$curr_pd}")

    # save the plot to the Images folder
    si_save(glue("Images/{plot_file_name}_{ref_id}.png"),
            height = 9,
            width = 13)
    