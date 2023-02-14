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
