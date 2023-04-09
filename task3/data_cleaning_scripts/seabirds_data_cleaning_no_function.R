library(tidyverse)

# import dirty data
bird_dirty_data <- readxl::read_xls(here::here("raw_data/seabirds.xls"),
                            sheet = "Bird data by record ID",
                            col_types = "text")

ship_dirty_data <- readxl::read_xls(here::here("raw_data/seabirds.xls"),
                            sheet = "Ship data by record ID",
                            col_types = "text")


# join data sets and clean column names 
seabirds_joined_data <- bird_dirty_data %>% 
  left_join(ship_dirty_data, by = "RECORD ID") %>% 
  janitor::clean_names() %>% 
  rename(com_name = species_common_name_taxon_age_sex_plumage_phase,
         sci_name = species_scientific_name_taxon_age_sex_plumage_phase,
         abr_name = species_abbreviation)


# clean species names to remove age, wanplum, plphase and sex
seabirds_clean_data <- seabirds_joined_data %>%
  mutate(
    common_name = case_when(
      !is.na(age) ~ str_remove(com_name, " [A-Z]{2}[// [:alnum:]]*"),
      !is.na(wanplum) ~ str_remove(com_name, " PL[// [:alnum:]]*"),
      !is.na(plphase) ~ str_remove(com_name, " [A-Z]{2}[// [:alnum:]]*"),
      !is.na(sex) ~ str_remove(com_name, " [MF]$"),
      TRUE ~ com_name
      ),
    .after = com_name
    ) %>%
  mutate(
    scientific_name = case_when(
      !is.na(age) ~ str_remove(sci_name, " [A-Z]{2}[// [:alnum:]]*"),
      !is.na(wanplum) ~ str_remove(sci_name, " PL[// [:alnum:]]*"),
      !is.na(plphase) ~ str_remove(sci_name, " [A-Z]{2}[// [:alnum:]]*"),
      !is.na(sex) ~ str_remove(sci_name, " [MF]$"),
      TRUE ~ sci_name
    ),
    .after = com_name
  ) %>%
  mutate(
    abbreviation = case_when(
      !is.na(age) ~ str_remove(abr_name, " [A-Z]{2}[// [:alnum:]]*"),
      !is.na(wanplum) ~ str_remove(abr_name, " PL[// [:alnum:]]*"),
      !is.na(plphase) ~ str_remove(abr_name, " [A-Z]{2}[// [:alnum:]]*"),
      !is.na(sex) ~ str_remove(abr_name, " [MF]$"),
      TRUE ~ abr_name
    ),
    .after = abr_name
  )


# select required variables and export clean data
seabirds_clean_data %>%
  select(
    record = record_x,
    record_id,
    common_name = com_name_1,
    scientific_name = sci_name_1,
    abbreviation = abr_name_1,
    count,
    acc_num = nacc,
    acc_occ = ocacc,
    latitude = lat,
    longitude = long
    ) %>%
  write_csv(here::here("clean_data/seabirds_clean.csv"))
