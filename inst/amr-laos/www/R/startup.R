# Load packages
library(bslib)
library(DT)
library(glue)
library(highcharter)
library(lubridate)
library(markdown)
library(prompter)
library(readxl)
library(shiny)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(tidyverse)
# library(viridisLite)

# Languages
lang <- data.frame(
  val = c("en", "la"),
  flg = c(
    "<span><i class='fa fa-language' role='presentation' aria-label='language icon'></i> English</span>",
    "<span><i class='fa fa-language' role='presentation' aria-label='language icon'></i> Lao</span>"
  )
)

# Colors order: S, I, R, Not Tested
cols_sir <- c("#2166ac", "#fddbc7", "#b2182b", "#969696")

# Colors order: Negative, Positive, Unknown
cols_esbl <- c("#2166ac", "#b2182b", "#969696")

amr_theme <- bslib::bs_theme(version = 4, bootswatch = "flatly")

source("www/R/function/highchart_sir.R", local = TRUE)$value
source("www/R/function/highchart_sir_evolution.R", local = TRUE)$value

bugantibio <- read_excel("www/data/bug-antibio_display.xlsx") %>% 
  pivot_longer(-Antibiotics, names_to = "bug", values_to = "display") %>%
  rename(antibio = Antibiotics)

load("www/data/lims_processed.RData")

# Recoding
lims$amr <- lims$amr %>% 
  replace_na(list(gender = "Unknown", 
                  province = "Unknown", 
                  location = "Unknown", 
                  org_name = "Unknown", 
                  esbl = "Negative",  # Following Tamalaee instructions. Negative test results should be made explicit in future updates.
                  spec_method = "Unknown"))


lims$amr$location[lims$amr$location == "Mahosot hospital"] <- "Mahosot"
lims$amr$location[lims$amr$location == "Mahosot hospital (MH)"] <- "Mahosot"
lims$amr$location[lims$amr$location == "OPD"] <- "Mahosot"
lims$amr$location[lims$amr$location == "Sethathirat Hospital (ST)"] <- "Setthathirat Hospital (ST)"
lims$amr$location[lims$amr$location == "Police Hospital (PH)"] <- "Police Hospital Vientiane"
lims$amr$location[lims$amr$location == "Military Hospital (MIL)"] <- "Military Hospital Vientiane (MIL)"
lims$amr$location[lims$amr$location == "Xieng Kwang Hospital (XK)"] <- "Xieng Khuang Hospital (XK)"
lims$amr$location[lims$amr$location == "Luxembourg hospital, Vientiane Province (VT)"] <- "Vientiane Provincial Hospital (VTP)"
lims$amr$location[lims$amr$location == "Luxembourg hospital, Vientiane Province (VTP)"] <- "Vientiane Provincial Hospital (VTP)"

lims$amr <- lims$amr %>% 
  mutate(
    province_recode = case_when(
      province == "Vieniane Capital" ~ "Vientiane Capital",
      province == "Oudomxai" ~ "Oudomxay",
      province == "Baukeo" ~ "Bokeo",
      province == "Huaphan" ~ "Huaphanh",
      province == "Xaiyabuly" ~ "Xayabury",
      province == "Baulikhamxai" ~ "Borikhamxay",
      province == "Khammuan" ~ "Khammuane",
      province == "Saravan" ~ "Salavan",
      province == "Champasak" ~ "Champasack",
      province == "Savannakhet" ~ "Savanakhet",
      province == "Xaysomboon" ~ "Xaysomboun",
      province == "Xiengkhuang" ~ "Xieng Khuang",
      province == "Luangnamtha" ~ "Luang Namtha",
      province == "Luangprabang" ~ "Luang Prabang",
      TRUE ~ province)) %>%
  select(-province) %>%
  rename(province = province_recode)


lims$amr$spec_method[lims$amr$spec_method  == "Haemoculture"] <- "Blood culture"

# Create an age category for filters.
lims$amr <- lims$amr |> 
  replace_na(list(age_years = 0, age_months = 0, age_days = 0)) |> 
  mutate(age_total_days = 365.25 * age_years + 30.4167 * age_months + age_days) |> 
  mutate(age_category = case_when(
    age_total_days >= 15 * 365.25 ~ "Adult",
    age_total_days > 28 & (age_total_days <= 15 * 365.25) ~ "Child",
    age_total_days <= 28 & age_total_days >= 1 ~ "Neonate",
    TRUE ~ "Unknown"
  ))

if (sum(lims$amr$age_category == "Unknown") > 0) warning("Some ages are missing")

# Check coherence:
# table(lims$amr$age_category, useNA = "always")
# lims$amr |> filter(age_category == "Unknown") |> select(starts_with("age"))

all_provinces <- sort(unique(lims$amr$province))
all_locations <- sort(unique(lims$amr$location))
all_org_name <- sort(unique(lims$amr$org_name))
all_spec_method <- sort(unique(lims$amr$spec_method))
all_spec_year <- unique(lims$amr$spec_year)
oldest_patient <- max(lims$amr$age_years, na.rm = TRUE)
min_collection_date <- min(lims$amr$spec_date)
max_collection_date <- max(lims$amr$spec_date)