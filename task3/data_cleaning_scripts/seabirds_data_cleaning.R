library(tidyverse)


# SECTION 1: FUNCTION DEFINITIONS -----

# function to clean species names to remove age, wanplum, plphase and sex
species_split <- function(df, col_name) {
  case_when(
    !is.na(df$age) ~ str_remove(col_name, " [A-Z]{2}[\\ [:alnum:]]*"),
    !is.na(df$wanplum) ~ str_remove(col_name, " PL[\\ [:alnum:]]*"),
    !is.na(df$plphase) ~ str_remove(col_name, " [A-Z]{2}[\\ [:alnum:]]*"),
    !is.na(df$sex) ~ str_remove(col_name, " [MF]$"),
    TRUE ~ col_name
  )
}


# SECTION 2: IMPORT AND CLEAN DATA SET, THEN EXPORT -----

# import dirty data
bird_dirty_data <- readxl::read_xls(here::here("raw_data/seabirds.xls"),
                            sheet = "Bird data by record ID",
                            col_types = "text")

ship_dirty_data <- readxl::read_xls(here::here("raw_data/seabirds.xls"),
                            sheet = "Ship data by record ID",
                            col_types = "text")


# join data sets, clean column names and remove empty observations/records
seabirds_joined_data <- bird_dirty_data %>% 
  left_join(ship_dirty_data, by = "RECORD ID") %>% 
  janitor::clean_names() %>% 
  rename(com_name = species_common_name_taxon_age_sex_plumage_phase,
         sci_name = species_scientific_name_taxon_age_sex_plumage_phase,
         abr_name = species_abbreviation) %>% 
  filter(com_name != "[NO BIRDS RECORDED]")


# clean species names to remove age, wanplum, plphase and sex
seabirds_clean_data <- seabirds_joined_data %>% 
  mutate(across(.cols = c(com_name, sci_name, abr_name),
                .fns = ~ species_split(seabirds_joined_data, .x),
                .names = "{.col}_1"
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
    latitude = lat,
    longitude = long
    ) %>%
  write_csv(here::here("clean_data/seabirds_clean.csv"))
