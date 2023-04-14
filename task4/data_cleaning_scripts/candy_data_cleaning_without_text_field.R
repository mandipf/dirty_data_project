library(tidyverse)



#####
# SECTION 1: FUNCTION DEFINITIONS

# function to import dirty data and perform general clean of column names
import_xlsx_and_clean_names <- function(file_name) {
  df <- readxl::read_xlsx(here::here(str_c("raw_data/", file_name)),
                          col_types = "text")
  
  df_clean <- df %>% 
    mutate(year = str_extract(file_name, "20[:digit:]{2}"),
           tibble::rowid_to_column(df, "rater_id"),
           rater_id = str_c(year, "_", rater_id),
           .before = everything()
    ) %>% 
    janitor::clean_names()
}



##### 
# SECTION 2: IMPORT AND CLEAN EACH DATA SET, THEN COMBINE THEM

# import and extract relevant data from 2015 data file (file specific)
candy_2015_import <- import_xlsx_and_clean_names("boing-boing-candy-2015.xlsx")

candy_2015_col_clean <- candy_2015_import %>%
  rename(age = how_old_are_you,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         joy = please_list_any_items_not_included_above_that_give_you_joy,
         despair = please_list_any_items_not_included_above_that_give_you_despair
  ) %>% 
  select(year:despair, necco_wafers)

candy_2015_main_list <- candy_2015_col_clean %>%
  select(-joy, -despair) %>% 
  pivot_longer(cols = butterfinger:last_col(),
               names_to = "candy",
               values_to = "rating"
  ) %>% 
  mutate(candy = str_replace_all(candy, "_", " ")) %>% 
  filter(!is.na(rating))


# import and extract relevant data from 2016 data file (file specific)
candy_2016_import <- import_xlsx_and_clean_names("boing-boing-candy-2016.xlsx")

candy_2016_col_clean <- candy_2016_import %>%
  rename(age = how_old_are_you,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         country = which_country_do_you_live_in,
         joy = please_list_any_items_not_included_above_that_give_you_joy,
         despair = please_list_any_items_not_included_above_that_give_you_despair
  ) %>% 
  select(year:despair)

candy_2016_main_list <- candy_2016_col_clean %>%
  select(-joy, -despair) %>%
  pivot_longer(cols = x100_grand_bar:last_col(),
               names_to = "candy",
               values_to = "rating"
  ) %>% 
  mutate(candy = str_replace_all(candy, "_", " ")) %>% 
  filter(!is.na(rating))


# import and extract relevant data from 2017 data file (file specific)
candy_2017_import <- import_xlsx_and_clean_names("boing-boing-candy-2017.xlsx")

candy_2017_col_clean <- candy_2017_import %>%
  rename_with(~str_remove(., "^q[1-6]_")) %>% 
  rename(going_trick_or_treating = going_out,
         joy = q7_joy_other,
         despair = q8_despair_other) %>% 
  select(year:despair)

candy_2017_main_list <- candy_2017_col_clean %>%
  select(-joy, -despair) %>% 
  pivot_longer(cols = `100_grand_bar`:last_col(),
               names_to = "candy",
               values_to = "rating"
  ) %>% 
  mutate(candy = str_replace_all(candy, "_", " ")) %>% 
  filter(!is.na(rating))


# combine all 3 data sets and remove redundant columns
all_candy_data_combined <- bind_rows(candy_2015_main_list,
                                     candy_2016_main_list,
                                     candy_2017_main_list)


all_candy_data_combined <- all_candy_data_combined %>% 
  select(year, rater_id, age, gender, country, 
         going_trick_or_treating, candy, rating)



#####
# SECTION 3: DEEP CLEAN OF FULL DATA SET

# combine similar sounding candy names and only keep joy/meh/despair entries
source(here::here("data_cleaning_scripts/lists/alt_entry_names_for_candy.R"))

alternative_candy_names <- enframe(candy_list) %>%
  unnest_longer(col = value, values_to = "replace_this")

alt_candy_combined <- all_candy_data_combined %>%
  mutate(rating = str_to_lower(str_trim(rating)),
         candy = str_to_lower(str_trim(candy))
  ) %>%
  filter(rating %in% c("joy", "despair", "meh")) %>%
  left_join(alternative_candy_names, by = join_by(candy == replace_this)) %>%
  mutate(candy = if_else(!is.na(name), name, candy)) %>% 
  select(-name)


# check candy names exists and remove those with less than 10 entries 
source(here::here("data_cleaning_scripts/lists/candy_names.R"))

fixed_candy_data <- alt_candy_combined %>% 
  mutate(is_candy = (candy %in% candy_names)) %>%
  filter(is_candy) %>%
  group_by(candy) %>%
  filter(n() > 9) %>%
  ungroup() %>% 
  select(-is_candy)


# filter countries into Canada, UK, USA, others and NA
source(here::here("data_cleaning_scripts/lists/alt_entry_names_for_countries.R"))

alternative_country_names <- enframe(country_list) %>%
  unnest_longer(col = value, values_to = "replace_this")

candy_and_country_data <- fixed_candy_data %>% 
  left_join(alternative_country_names, by = join_by(country == replace_this)) %>% 
  mutate(name_2 = case_when(str_detect(country, "^(?i)canada") ~ "Canada",
                            str_detect(country, "^(?i)unite[sd] st") ~ "United States of America",
                            str_detect(country, "^(?i)U[\\ \\.]*S") ~ "United States of America",
                            str_detect(country, "mer") ~ "United States of America",
                            str_detect(country, "^(?i)U[\\ ]*K$") ~ "United Kingdom",
                            str_detect(country, "^United Ki[:alpha:]*om$") ~ "United Kingdom",
                            TRUE ~ NA),
         .after = name
  ) %>%
  mutate(country = case_when(is.na(country) ~ NA,
                             is.numeric(country) ~ NA,
                             !is.na(name_2) ~ name_2,
                             !is.na(name) ~ name,
                             TRUE ~ "Other")
  ) %>% 
  select(-name, -name_2)


# update age list to be 1-100 years old, rest are NA
age_candy_country_data <- candy_and_country_data %>% 
  mutate(age_floor = floor(as.numeric(age)),
         age = if_else(age_floor > 0 & age_floor <= 100,
                       age_floor,
                       NA)) %>% 
  select(-age_floor)



#####
# SECTION 4: EXPORT CLEAN DATA AS CSV
age_candy_country_data %>%
  select(year, rater_id, age, gender, country, 
         going_trick_or_treating, candy, rating) %>% 
  write_csv(here::here("clean_data/candy_clean_without_text_field.csv"))
