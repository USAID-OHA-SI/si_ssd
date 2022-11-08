# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Practicing quarterly review analysis
# REF ID:   9071c744
# LICENSE:  MIT
# DATE:     2022-11-08
# UPDATED:  2022-11-08

# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(gagglr)
library(glue)
library(scales)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(ggrepel)

# GLOBAL VARIABLES -------------------------------------------------------------

ref_id <- "9071c744"

path_genie <- "Data/Genie_SITE_IM_South_Sudan_Daily_2022-11-08.zip"

peds <- c("<01", "01-04", "05-09", "10-14", "<15")

# IMPORT -----------------------------------------------------------------------

df <- read_msd(path_genie)

# PERIODS ----------------------------------------------------------------------

get_metadata(path_genie)

full_pds <- (min(df$fiscal_year) - 1) %>%
  paste0("-10-01") %>%
  as_date() %>%
  seq.Date(convert_qtr_to_date(metadata$curr_pd), by = "3 months") %>%
  convert_date_to_qtr()

pd_brks <- str_replace(full_pds, "FY.*(1|3)$", "")

pd_prior <- (convert_qtr_to_date(metadata$curr_pd) - months(3)) %>%
  convert_date_to_qtr()

# TREATMENT --------------------------------------------------------------------

df_tx <- df %>%
  filter(
    indicator == "TX_CURR",
    (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% peds) |
      (standardizeddisaggregate == "Total Numerator")) %>%
  mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
    "Total", "Peds")) %>%
  group_by(fiscal_year, snu1, indicator, type) %>%
  summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
    .groups = "drop") %>%
  reshape_msd("quarters") %>%
  select(-results_cumulative) %>%
  arrange(type, snu1, period)

df_tx <- df_tx %>%
  mutate(
    growth_rate_req =
      case_when(period == metadata$curr_pd ~
        ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
  group_by(type, snu1) %>%
  fill(growth_rate_req, .direction = "updown") %>%
  mutate(
    growth_rate = (results / lag(results, order_by = period)) - 1,
    growth_rate = na_if(growth_rate, Inf)) %>%
  ungroup() %>%
  mutate(
    geo_gr_lab = case_when(
      is.infinite(growth_rate_req) ~ glue("{toupper(snu1)}"), # metadata$curr_qtr == 4 | is.infinite(growth_rate_req)
      growth_rate_req < 0 ~ glue("{toupper(snu1)}\nTarget achieved"),
      growth_rate_req < .1 ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}"),
      TRUE ~ glue("{toupper(snu1)}\n{percent(growth_rate_req, 1)}")),
    gr_lab = case_when(fiscal_year == metadata$curr_fy ~ percent(growth_rate, 1)),
    gr_label_position = 0,
    disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets))

df_achv <- df_tx %>%
  filter(period == metadata$curr_pd) %>%
  count(type, results >= targets) %>%
  filter(`results >= targets` == TRUE)

df_tx %>%
  filter(type == "Total") %>%
  ggplot(aes(period, results, fill = as.character(fiscal_year))) +
  geom_col(aes(y = disp_targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
  geom_col(na.rm = TRUE) +
  geom_errorbar(aes(ymin = targets, ymax = targets), linetype = "dashed", width = .95, na.rm = TRUE) +
  geom_text(aes(label = gr_lab, y = gr_label_position),
    family = "Source Sans Pro", color = "white", size = 9 / .pt,
    vjust = -.5, na.rm = TRUE) +
  facet_wrap(~ fct_reorder2(geo_gr_lab, period, targets), scales = "free_y") +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  scale_x_discrete(breaks = unique(df_tx$period)[grep("Q(4)", unique(df_tx$period))]) +
  scale_fill_manual(values = c(scooter_light, scooter)) +
  labs(
    x = NULL, y = NULL,
    title = glue("ONLY {df_achv[df_achv$type == 'Total',]$n} of USAID's regions reached their {metadata$curr_fy_lab} treatment targets") %>% toupper(),
    subtitle = "Current on treatment by region and quarterly growth rate",
    # subtitle = "Current on treatment by region and growth rate needed in Q4 to reach target",
    caption = glue( # "Note: quarterly growth rate needed calculated as a compound annual growth rate
      "{metadata$caption} | US Agency for International Development")) +
  si_style_ygrid() +
  theme(
    legend.position = "none",
    panel.spacing = unit(.5, "picas"),
    axis.text.x = element_text(size = 8))


si_save(paste0(metadata$curr_pd, "_SSD-_tx-curr-growth_regional.png"),
  path = "Images",
  scale = 1.5)

# IIT --------------------------------------------------------------------------

# KP ---------------------------------------------------------------------------
# ACHIEVEMENT ------------------------------------------------------------------
