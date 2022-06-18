# These  R packages are needed to run this script.
library(readxl)
library(tidyverse)
library(writexl)

# Change these paths to reflect where the lims_file and dic_file are located on your laptop and where to save the processed files.
# The rest of the script should run silently and, at the end, generate two files "lims_processed.xlsx" and, in the same folder "lims_processed.RData"

lims_file <- "/Users/olivier/Documents/Projets/LOMWRU/AMR in Laos/amr-laos/misc/data_provided_gitignored/LIMS_2021_long.xlsx"
dic_file <- "/Users/olivier/Documents/Projets/LOMWRU/AMR in Laos/amr-laos/misc/data_provided_gitignored/LIMS_AMR_codes.xlsx"
lims_processed_file <- "/Users/olivier/Documents/Projets/LOMWRU/AMR in Laos/amr-laos/misc/data_processed/lims_processed.xlsx"

# Read files
lims <- readxl::read_excel(lims_file, guess_max = 1000000) |> janitor::clean_names()
antibio_codes <- readxl::read_excel(dic_file, sheet = 1) |> janitor::clean_names()
spec_codes    <- readxl::read_excel(dic_file, sheet = 2) |> janitor::clean_names()
org_include   <- readxl::read_excel(dic_file, sheet = 3) |> janitor::clean_names()


clin_significant_organisms <- org_include |> 
  filter(include == "Yes") |> 
  pull(organisms)

comments_to_exclude <- c("Mixed environemental gram negative organism and Staphylococcus coagulase negative", 
                         "Normal oral flora", 
                         "Normal skin flora")

stopifnot(
  !is.null(lims),
  nrow(lims) > 0,
  ncol(lims) > 0,
  !is.null(antibio_codes),
  !is.null(spec_codes),
  !is.null(org_include)
)

# Join with data dictionnary and select required data
amr <- left_join(lims |> mutate(antibiotic_code = str_to_upper(antibiotic_etest_name)), 
                 antibio_codes, 
                 by = "antibiotic_code")

amr <- left_join(amr |> rename(spec_type = spectype), 
                 spec_codes, 
                 by = "spec_type")


amr <- amr |> 
  transmute(
    isolate_id = 1:n(),
    spec_id = specnum,
    patient_id = patient_idnum,
    gender,
    province,
    spec_method,
    spec_type,
    spec_date = specdate,
    spec_year = lubridate::year(specdate),
    age_years, 
    age_months, 
    age_days,
    location,
    org_name,
    esbl,
    culturecomment, 
    culturecomment2, 
    antibiotic_code,
    antibiotic_name,
    resistance) |> 
  replace_na(list(culturecomment = "", culturecomment2 = ""))


# Add columns to tag elements to keep/exclude:
exclude_columns <- tribble(
  ~column, ~description,
  "exclude_blo", "all elements for which the method is 'Clotted Blood' or 'EDAT Blood'.",
  "exclude_oni", "organism not in the list of organisms to include.",
  "exclude_sta", "organism labelled 'Staphylococcus species - not Staph. aureus'",
  "exclude_com", "elements with the following comments: 'Mixed environemental gram negative organism and Staphylococcus coagulase negative', 'Normal oral flora', 'Normal skin flora'.",
  "exclude_nso", "organisms that are not signifiant",
  "exclude_aggregate", "any of the exclusion criteria"
)

amr <- amr |> 
  mutate(
    exclude_blo = case_when(
      spec_method %in% c("Clotted blood", "EDTA blood") ~ "exclude",
      TRUE ~ "keep"),
    exclude_oni = case_when(
      ! org_name %in% clin_significant_organisms ~ "exclude",
      TRUE ~ "keep"
    ),
    exclude_sta = case_when(
      org_name == "Staphylococcus species - not Staph. aureus" ~ "exclude",
      TRUE ~ "keep"
    ),
    exclude_com = case_when(
      culturecomment  %in% comments_to_exclude ~ "exclude", 
      culturecomment2 %in% comments_to_exclude ~ "exclude",
      TRUE ~ "keep"
    ),
    exclude_nso = case_when(
      org_name != "No growth" & (str_detect(culturecomment, "Probable contaminant") | 
                                   str_detect(culturecomment, "Uncertain clinical significance") | 
                                   str_detect(culturecomment2, "Probable contaminant") | 
                                   str_detect(culturecomment2, "Uncertain clinical significance")) ~ "exclude",
      TRUE ~ "keep"
    )
  ) |> 
  mutate(exclude_aggregate = case_when(
    exclude_blo == "exclude" ~ "exclude",
    exclude_oni == "exclude" ~ "exclude",
    exclude_sta == "exclude" ~ "exclude",
    exclude_com == "exclude" ~ "exclude",
    exclude_nso == "exclude" ~ "exclude",
    TRUE ~ "keep"
  ))

lims <- list(
  meta = tibble(generate = Sys.Date()),
  amr = amr,
  exclude = exclude_columns,
  dic_antibio_codes = antibio_codes,
  dic_spec_codes = spec_codes,
  dic_org_include = org_include
)

writexl::write_xlsx(lims, path = lims_processed_file)
save(lims, file = str_replace(lims_processed_file, ".xlsx", ".RData"))
