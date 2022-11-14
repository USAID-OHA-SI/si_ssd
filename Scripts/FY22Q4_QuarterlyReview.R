# PROJECT:  si_ssd
# AUTHOR:   J.Hoehner | USAID
# PURPOSE:  Practicing quarterly review analysis
# REF ID:   9071c744
# LICENSE:  MIT
# DATE:     2022-11-08
# UPDATED:  2022-11-09

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
adults <- c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
            "50-54", "55-59", "60-64", "65+", "15+")

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
   (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults) |
      (standardizeddisaggregate == "Total Numerator")) %>%
  mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
    "Total", "Adults")) %>%
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

df_iit <- df %>% 
  filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW")) %>%
  pluck_totals() %>%
  group_by(fiscal_year, snu1, psnu, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "indicator",
              names_glue = "{tolower(indicator)}")

df_iit <- df_iit %>%
  # group_by(psnu) %>% 
  # mutate(tx_curr_lag1 = lag(tx_curr, n = 1, order_by = period)) %>% 
  # ungroup() %>% 
  mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
  rowwise() %>% 
  mutate(iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  ungroup()

df_snu_lab <- df_iit %>% 
  filter(period == metadata$curr_pd) %>% 
  count(snu1, wt = tx_curr, sort = TRUE) %>% 
  # mutate(snu1_lab = glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)}")) %>% 
  mutate(snu1_lab = ifelse(n == max(n), glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)} [TX_CURR {pd_prior}]"),
                           glue("{snu1} - {label_number(1, scale_cut = cut_short_scale())(n)}"))) %>%
  select(-n)

snu_tx_order <- df_iit %>% 
  filter(period == metadata$curr_pd) %>% 
  count(snu1, wt = tx_curr, sort = TRUE) %>% 
  pull(snu1)

vct_itt_cntry <- df %>% 
  filter(indicator %in% c("TX_ML", "TX_CURR", "TX_NEW", "TX_NET_NEW"),) %>%
  pluck_totals() %>%
  group_by(fiscal_year, country, indicator) %>% 
  summarise(across(starts_with("qtr"), sum, na.rm = TRUE), .groups = "drop") %>% 
  reshape_msd(include_type = FALSE) %>% 
  pivot_wider(names_from = "indicator",
              names_glue = "{tolower(indicator)}") %>% 
  filter(period == metadata$curr_pd) %>% 
  mutate(tx_curr_lag1 = tx_curr - tx_net_new) %>% 
  rowwise() %>% 
  mutate(
    iit = tx_ml / sum(tx_curr_lag1, tx_new, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pull() %>% 
  percent()

df_iit %>%
  left_join(df_snu_lab, by = "snu1") %>% 
  mutate(snu1_lab = factor(snu1_lab, df_snu_lab$snu1_lab),
         fiscal_year = str_sub(period, end = 4)) %>% 
  filter(tx_curr_lag1 != 0) %>%
  ggplot(aes(period, iit, size = tx_curr_lag1)) +
  geom_point(position = position_jitter(width = .2, seed = 42),
             na.rm = TRUE, color = scooter,
             alpha = .2) +
  geom_smooth(aes(weight = tx_curr_lag1, group = snu1_lab),
              method = "loess",
              formula = "y ~ x", se = FALSE, na.rm = TRUE,
              linewidth = 1.5, color = golden_sand) +
  facet_wrap(~snu1_lab) +
  scale_size(label = comma, guide = NULL) +
  scale_x_discrete(labels = pd_brks) +
  scale_y_continuous(limits = c(0,.25),
                     label = percent_format(1),
                     oob = oob_squish) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL,
       size = "Site TX_CURR (1 period prior)",
       title = glue("USAID ENDED THE YEAR WITH IIT of {vct_itt_cntry} with 1 region 
                    trending downward and the other trending upwards") %>% toupper,
       subtitle = "County IIT rates within each region",
       caption = glue("Note: IIT = TX_ML / TX_CURR - TX_NET_NEW + TX_NEW; ITT capped to 25%
                        {metadata$caption} | US Agency for International Development")) +
  si_style_ygrid() +
  theme(panel.spacing = unit(.5, "line"),
        axis.text = element_text(size = 8),
        plot.subtitle = element_markdown())

si_save(glue("Images/{metadata$curr_pd}_SSD_region_iit.png"),
        scale = 1.5)  

# KP TX_CURR -------------------------------------------------------------------

df_tx_kp <- df %>%
  filter(
    indicator == "TX_CURR",
    (standardizeddisaggregate == "KeyPop/HIVStatus") |
      (standardizeddisaggregate == "Total Numerator")) %>%
  mutate(type = ifelse(standardizeddisaggregate == "Total Numerator",
                       "Total", "FSW")) %>%
  group_by(fiscal_year, snu1, indicator, type) %>%
  summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE),
            .groups = "drop") %>%
  reshape_msd("quarters") %>%
  select(-results_cumulative) %>%
  arrange(type, snu1, period)

df_tx_kp <- df_tx_kp %>%
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

df_achv_kp <- df_tx_kp %>%
  filter(period == metadata$curr_pd) %>%
  count(type, results >= targets) %>%
  filter(`results >= targets` == TRUE)

df_tx_kp %>%
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
    title = glue("ONLY {df_achv[df_achv$type == 'Total',]$n} of USAID's regions reached their {metadata$curr_fy_lab} treatment targets among FSWs") %>% toupper(),
    subtitle = "Current FSWs on treatment by region and quarterly growth rate",
    caption = glue("{metadata$caption} | US Agency for International Development")) +
  si_style_ygrid() +
  theme(
    legend.position = "none",
    panel.spacing = unit(.5, "picas"),
    axis.text.x = element_text(size = 8))

# ACHIEVEMENT ------------------------------------------------------------------

df_achv <- df %>% 
  filter(indicator == "TX_CURR", 
         (standardizeddisaggregate == "Age/Sex/HIVStatus" & ageasentered %in% adults) |
           (standardizeddisaggregate == "Total Numerator"),
         fiscal_year == metadata$curr_fy) %>%
  mutate(type = ifelse(standardizeddisaggregate == "Total Numerator", "Total", "Adults")) %>% 
  group_by(fiscal_year, snu1, psnu, indicator, type) %>% 
  summarise(across(c(targets, cumulative), sum, na.rm = TRUE), .groups = "drop") %>%
  adorn_achievement(qtr = identifypd(df, pd_type = "quarter"))

vct_colors <-  df_achv %>% 
  filter(!is.na(achievement)) %>% 
  distinct(achv_label, achv_color) %>% 
  deframe()

df_underachv <- df_achv %>% 
  filter(!is.na(achievement)) %>%
  count(type, reached_target = achievement >= .9) %>% 
  group_by(type) %>% 
  mutate(share = n/sum(n)) %>% 
  ungroup() %>% 
  filter(reached_target == FALSE) %>% 
  select(-share) %>% 
  pivot_wider(names_from = type, values_from = n)

df_achv %>% 
  filter(type == "Total",
         !is.na(achievement)) %>% 
  mutate(label_psnu = case_when(achievement < .9 ~ psnu)) %>% 
  ggplot(aes(targets, achievement, fill = achv_label, size = targets)) +
  geom_point(shape = 21, alpha = .6) +
  geom_text_repel(aes(label = label_psnu), size = 8/.pt, na.rm = TRUE,
                  color = matterhorn, family = "Source Sans Pro") +
  facet_wrap(~fct_reorder(snu1, targets, sum, na.rm = TRUE, .desc = TRUE)) +
  scale_size(label = label_number(scale_cut = cut_short_scale())) +
  scale_fill_manual(values = vct_colors) +
  scale_x_log10(label = comma) +
  scale_y_continuous(limits = c(0,1.1),
                     label = percent_format(1),
                     oob = oob_squish,
                     breaks = seq(0, 1, .25)) +
  coord_cartesian(clip = "off") +
  labs(x = glue("{metadata$curr_fy} Targets (log scale)"), y = "Target Achievement",
       fill = "Target Achievement", size = glue("{metadata$curr_fy_lab} Targets"),
       title = glue("{df_underachv$Total} USAID counties failed to reach the 90% of their {metadata$curr_fy_lab} treatment targets") %>% toupper(),
       subtitle = "Counties under the achivement threshold are labeled",
       caption = glue("Note: Achievement capped at 110% 
                        {metadata$caption} | US Agency for International Development")) +
  si_style() +
  theme(panel.spacing = unit(1, "picas"),
        legend.position = "none") 


si_save(paste0(metadata$curr_pd,"_SSD-_tx-curr-psnu-achv.png"),
        path = "Images",
        scale = 1.5)

