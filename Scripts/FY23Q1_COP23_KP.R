# PROJECT: si_ssd
# PURPOSE: key indicators for KP by snu
# AUTHOR: Jessica Hoehner
# LICENSE: MIT
# DATE: 2023-02-13 
# NOTE: adapted from 20211005_HAFCO_USAID_KP-ind-trends.R

# LOCALS & SETUP ===============================================================

# Libraries
library(gagglr)
library(tidyverse)
library(scales)
library(sf)
library(extrafont)
library(tidytext)
library(patchwork)
library(ggtext)
library(glue)

# Global variables -------------------------------------------------------------

file_path <- "OU_IM_South_Sudan"

ind_sel_kp <- c("HTS_TST", "HTS_TST_POS", 
                "TX_CURR", "TX_NEW", 
                "TX_PVLS_D", "TX_PVLS")

# LOAD DATA ==================================================================== 

msd <- si_path() %>%
  return_latest(file_path) %>%
  read_psd()

# Grab metadata

get_metadata()

# MUNGE ========================================================================

df_kp <- msd %>% 
  filter(fiscal_year == metadata$curr_fy,
         indicator %in% ind_sel_kp,
         !(indicator == "TX_PVLS" & standardizeddisaggregate == "KeyPop/HIVStatus"),
         str_detect(standardizeddisaggregate, "KeyPop")) %>%
  clean_indicator() %>% 
  group_by(indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>% 
  reshape_msd() %>% 
  mutate(indicator = factor(indicator, ind_sel_kp),
         value = na_if(value, 0),
         ind_lab = case_when(indicator == "HTS_TST" ~ "Receiving HIV testing services",
                             indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                             indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                             indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy",
                             indicator == "TX_PVLS_D" ~ "ART patients with Viral Load result (last 12mo)",
                             indicator == "TX_PVLS" ~ "ART patients with supressed Viral Load result (last 12mo)")) %>%
  arrange(indicator) %>% 
  mutate(ind_lab = fct_inorder(ind_lab)) %>% 
  mutate(max = max(value, na.rm = TRUE)) %>% 
  ungroup()

df_agyw <- msd %>% 
  filter(fiscal_year == metadata$curr_fy,
         indicator %in% ind_sel_kp,
         !(indicator == "TX_PVLS" & standardizeddisaggregate == "KeyPop/HIVStatus"), 
         ageasentered %in% c("10-14", "15-19", "20-24", "25-29"), 
         sex == "Female") %>%
  clean_indicator() %>% 
  group_by(indicator, fiscal_year) %>%
  summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>% 
  reshape_msd() %>% 
  mutate(indicator = factor(indicator, ind_sel_kp),
         value = na_if(value, 0),
         ind_lab = case_when(indicator == "HTS_TST" ~ "Receiving HIV testing services",
                             indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                             indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                             indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy",
                             indicator == "TX_PVLS_D" ~ "ART patients with Viral Load result (last 12mo)",
                             indicator == "TX_PVLS" ~ "ART patients with supressed Viral Load result (last 12mo)")) %>%
  arrange(indicator) %>% 
  mutate(ind_lab = fct_inorder(ind_lab)) %>% 
  mutate(max = max(value, na.rm = TRUE)) %>% 
  ungroup()

df_peds_ads <- msd %>% 
  filter(fiscal_year == metadata$curr_fy,
         indicator %in% ind_sel_kp,
         !(indicator == "TX_PVLS" & standardizeddisaggregate == "KeyPop/HIVStatus"),
         # 0-4, 5-9, and 10-19
         ageasentered %in% c("<01", "01-04", "05-09", "10-14", "15-19")) %>%
  clean_indicator() %>% 
  mutate(
    age_group = case_when(
      ageasentered %in% c("<01", "01-04") ~ "0-4",
      ageasentered %in% c("05-09") ~ "5-9",
      ageasentered %in% c("10-14", "15-19") ~ "10-19"), 
    age_group2 = "<20") %>%
  group_by(indicator, fiscal_year, age_group2) %>%
  summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE))) %>% 
  reshape_msd()%>% 
  mutate(indicator = factor(indicator, ind_sel_kp),
         value = na_if(value, 0),
         ind_lab = case_when(indicator == "HTS_TST" ~ "Receiving HIV testing services",
                             indicator == "HTS_TST_POS" ~ "Receiving HIV testing services and positive results",
                             indicator == "TX_NEW" ~ "Newly enrolled on antiretroviral therapy",
                             indicator == "TX_CURR"~ "Currently receiving antiretroviral therapy",
                             indicator == "TX_PVLS_D" ~ "ART patients with Viral Load result (last 12mo)",
                             indicator == "TX_PVLS" ~ "ART patients with supressed Viral Load result (last 12mo)")) %>%
  arrange(indicator) %>% 
  mutate(ind_lab = fct_inorder(ind_lab),
         max = max(value, na.rm = TRUE)) %>% 
  ungroup()

# Viz --------------------------------------------------------------------------
  
df_kp %>% 
  ggplot(aes(period, value, group = indicator, fill = indicator, 
             color = indicator)) +
  geom_blank(aes(y = max)) +
  geom_area(alpha = .2, size = 1, na.rm = TRUE) +
  geom_point(shape = 21, fill = "white", stroke = 1.5, na.rm = TRUE) +
  facet_wrap(~ind_lab, nrow = 3, ncol = 2, scales = "free_y") +
  scale_y_continuous(label = scales::label_number_si()) +
  scale_fill_manual(values = c("HTS_TST" = moody_blue,
                    "HTS_TST_POS" = moody_blue_light,
                    "TX_NEW" = golden_sand_light,
                    "TX_CURR"= golden_sand,
                    "TX_PVLS_D" = scooter,
                    "TX_PVLS" = scooter_light), aesthetics = c("color", "fill")) +
  labs(x = NULL, y = NULL, 
       title = "Key Populations", 
       subtitle = "Female Sex Workers",
       caption = glue("Source: {metadata$source}")) +
  si_style_ygrid(facet_space = .2) +
  theme(legend.position = "none")

si_save("Images/FY22Q4_SSD_kp-trends.png", scale = 1.2)

df_agyw %>% 
  ggplot(aes(period, value, group = indicator, fill = indicator, 
             color = indicator)) +
  geom_blank(aes(y = max)) +
  geom_area(alpha = .2, size = 1, na.rm = TRUE) +
  geom_point(shape = 21, fill = "white", stroke = 1.5, na.rm = TRUE) +
  facet_wrap(~ind_lab, nrow = 3, ncol = 2, scales = "free_y") +
  scale_y_continuous(label = scales::label_number_si()) +
  scale_fill_manual(values = c("HTS_TST" = moody_blue,
                               "HTS_TST_POS" = moody_blue_light,
                               "TX_NEW" = golden_sand_light,
                               "TX_CURR"= golden_sand,
                               "TX_PVLS_D" = scooter,
                               "TX_PVLS" = scooter_light), aesthetics = c("color", "fill")) +
  labs(x = NULL, y = NULL, 
       title = "Adolescent Girls and Young Women",
       subtitle = "Females, ages 10-29",
       caption = glue("Source: {metadata$source}")) +
  si_style_ygrid(facet_space = .2) +
  theme(legend.position = "none")

df_peds_ads %>% 
  ggplot(aes(period, value, group = indicator, fill = indicator, 
             color = indicator)) +
  geom_blank(aes(y = max)) +
  geom_area(alpha = .2, size = 1, na.rm = TRUE) +
  geom_point(shape = 21, fill = "white", stroke = 1.5, na.rm = TRUE) +
  facet_wrap(~ind_lab, nrow = 3, ncol = 2, scales = "free_y") +
  scale_y_continuous(label = scales::label_number_si()) +
  scale_fill_manual(values = c("HTS_TST" = moody_blue,
                               "HTS_TST_POS" = moody_blue_light,
                               "TX_NEW" = golden_sand_light,
                               "TX_CURR"= golden_sand,
                               "TX_PVLS_D" = scooter,
                               "TX_PVLS" = scooter_light), aesthetics = c("color", "fill")) +
  labs(x = NULL, y = NULL, 
       title = "Children and Adolescents",
       subtitle = "ages 19 and younger",
       caption = glue("Source: {metadata$source}")) +
  si_style_ygrid(facet_space = .2) +
  theme(legend.position = "none")

# ● FSW and KP Clients HIV Testing quarterly Trend

df_kp <- ou_df %>% 
  filter(operatingunit == "South Sudan",
         indicator == "HTS_TST", 
         (standardizeddisaggregate == "KeyPop/Result"),
         fiscal_year %in% c(2021, 2022)) %>%
  group_by(country, fiscal_year, indicator) %>%
  summarise(across(c(targets, starts_with("qtr")), sum, na.rm = TRUE), .groups = "drop") %>%
  reshape_msd("quarters") %>% 
  arrange(period)

df_kp <- df_kp %>% 
  mutate(
    growth_rate_req =
      case_when(period == metadata$curr_pd ~
                  ((targets / results)^(1 / (4 - metadata$curr_qtr))) - 1)) %>%
  fill(growth_rate_req, .direction = "updown") %>%
  mutate(
    growth_rate = case_when(period %in% c("FY21Q2", "FY21Q3", "FY21Q4", 
                                          "FY22Q2", "FY22Q3", "FY22Q4")
                            ~ (results_cumulative / lag(results_cumulative, order_by = period)) - 1),
    growth_rate = na_if(growth_rate, Inf)) %>%
  ungroup() %>%
  mutate(
    geo_gr_lab = case_when(
      is.infinite(growth_rate_req) ~ glue("{toupper(country)}"),
      growth_rate_req < 0 ~ glue("{toupper(country)}\nTarget achieved"),
      growth_rate_req < .1 ~ glue("{toupper(country)}\n{percent(growth_rate_req, 1)}"),
      TRUE ~ glue("{toupper(country)}\n{percent(growth_rate_req, 1)}")),
    gr_lab = glue("{percent(growth_rate, 1)}"),
    disp_targets = case_when(fiscal_year == metadata$curr_fy ~ targets), 
    amount_diff = targets - results, 
    pct_change = round_half_up((results - targets)/abs(targets) * 100),0)

df_achv_kp <- df_kp %>% 
  filter(period == metadata$curr_pd) %>%
  count(results >= targets) %>%
  filter(`results >= targets` == TRUE)

df_kp %>%
  ggplot(aes(period, results_cumulative, fill = as.character(period))) +
  geom_blank(aes(y = results_cumulative + 1000)) +
  geom_col(aes(y = targets), na.rm = TRUE, fill = suva_grey, alpha = .2) +
  geom_col(na.rm = TRUE) +
  geom_errorbar(aes(ymin = targets, ymax = targets), 
                linetype = "dashed", width = .95, na.rm = TRUE) +
  geom_text(aes(label = case_when(!gr_lab == "NA" ~ gr_lab), y = results_cumulative-1000),
            family = "Source Sans Pro", color = "white", size = 9 / .pt,
            vjust = -.5, na.rm = TRUE) +
  scale_y_continuous(label = label_number(scale_cut = cut_short_scale())) +
  scale_x_discrete(breaks = unique(df_kp$period)[grep("Q(4)", unique(df_kp$period))]) +
  scale_fill_manual(values = c(moody_blue_light, moody_blue_light, moody_blue_light, moody_blue_light,
                               moody_blue_light, moody_blue_light, moody_blue_light, moody_blue)) +
  labs(
    x = NULL, y = NULL,
    title = glue("PEPFAR Performance in Testing Targets (HTS_TST) Among Key Populations (FSW) 
                    Has Maintained Improvements in Achievement Made since FY21") %>% toupper(),
    caption = glue("{metadata$caption} | US Agency for International Development")) +
  si_style_ygrid() +
  theme(
    legend.position = "none",
    panel.spacing = unit(.5, "picas"),
    axis.text.x = element_text(size = 8))

si_save(paste0(metadata$curr_pd, "_SSD_KP_HTS_TST_TRENDS.png"),
        path = "Images",
        scale = 0.8)
