# import libraries
library(tidyverse)

# source R scripts
source(here::here("data_cleaning_scripts/R scripts/functions.R"))
source(here::here("data_cleaning_scripts/R scripts/lookup_candy_alt_names.R"))
source(here::here("data_cleaning_scripts/R scripts/lookup_candy_approved_names.R"))
source(here::here("data_cleaning_scripts/R scripts/lookup_countries_alt_names.R"))



#####
# SECTION 1: IMPORT FILES
# Rename personal details to short/tidy names
# Remove unwanted columns within the personal details columns

candy_2015 <- import_xlsx_and_clean_names("boing-boing-candy-2015.xlsx") %>% 
  rename(age = `How old are you?`,
         going_trick_or_treating = `Are you going actually going trick or treating yourself?`) %>% 
  select(-Timestamp)
  
candy_2016 <- import_xlsx_and_clean_names("boing-boing-candy-2016.xlsx") %>% 
  rename(age = `How old are you?`,
         going_trick_or_treating = `Are you going actually going trick or treating yourself?`,
         gender = `Your gender:`,
         country = `Which country do you live in?`) %>% 
  select(-Timestamp)

candy_2017 <- import_xlsx_and_clean_names("boing-boing-candy-2017.xlsx") %>% 
  rename(age = `Q3: AGE`,
         going_trick_or_treating = `Q1: GOING OUT?`,
         gender = `Q2: GENDER`,
         country = `Q4: COUNTRY`) %>% 
  select(-`Internal ID`)



#####
# SECTION 2: MAIN QUESTIONNAIRE WITH CANDY NAMED COLUMNS STANDARDISED
# select required columns and convert candy data to long format

candy_2015_main <- candy_2015 %>%
  select(year:going_trick_or_treating,
         `[Butterfinger]`:`[York Peppermint Patties]`,
         `[Necco Wafers]`) %>% 
  pivot_longer(cols = `[Butterfinger]`:last_col(),
               names_to = "candy",
               values_to = "rating",
               values_drop_na = TRUE)

candy_2016_main <- candy_2016 %>%
  select(year:country,
         `[100 Grand Bar]`:`[York Peppermint Patties]`) %>% 
  pivot_longer(cols = `[100 Grand Bar]`:last_col(),
               names_to = "candy",
               values_to = "rating",
               values_drop_na = TRUE)

candy_2017_main <- candy_2017 %>%
  select(year:country,
         `Q6 | 100 Grand Bar`:`Q6 | York Peppermint Patties`) %>% 
  pivot_longer(cols = `Q6 | 100 Grand Bar`:last_col(),
               names_to = "candy",
               values_to = "rating",
               values_drop_na = TRUE)  

# combine all three data sets
candy_combined <- bind_rows(candy_2015_main, candy_2016_main,
                            candy_2017_main) %>% 
  # remove wrapper characters from candy column and format rating column
  mutate(rating = str_to_lower(rating),
         candy = str_remove(candy, "^\\[|Q6 \\| "),
         candy = str_remove(candy, "\\]$"),
         candy = str_to_lower(candy))



##### 
# SECTION 3: MAIN QUESTIONNAIRE WITH ANY OTHER JOY/DESPAIR STANDARDISED
# Section 3 can be commented out to remove these additional joy/despair columns

# select required columns
candy_2015_extra <- candy_2015 %>%
  select(year:going_trick_or_treating,
         joy = `Please list any items not included above that give you JOY.`,
         despair = `Please list any items not included above that give you DESPAIR.`)

candy_2016_extra <- candy_2016 %>%
  select(year:country,
         joy = `Please list any items not included above that give you JOY.`,
         despair = `Please list any items not included above that give you DESPAIR.`)

candy_2017_extra <- candy_2017 %>%
  select(year:country,
         joy = `Q7: JOY OTHER`,
         despair = `Q8: DESPAIR OTHER`)

# combine all three data sets
candy_extra_combined <- bind_rows(candy_2015_extra, candy_2016_extra,
                                  candy_2017_extra) %>% 
  # convert to long format
  pivot_longer(cols = joy:despair,
               names_to = "rating",
               values_to = "candy") %>%
  # split rows with multiple entries, seperated by commas or semi-colons
  separate_longer_delim(cols = candy,
                        delim = regex("[\\,\\;]")) %>%
  filter(!is.na(candy) & candy != "") %>%
  # remove leading and lagging spaces and convert to lower case
  mutate(candy = trimws(str_to_lower(candy))) %>%
  # remove any candies with less than 10 entries
  group_by(candy) %>%
  filter(n() > 9)

# Join main questionnaire responses to other joy/despair responses
candy_combined <- bind_rows(candy_combined,
                            candy_extra_combined)



#####
# SECTION 4: COMBINE SIMILAR NAMED CANDY AND CHECK VALIDITY

# create list to combine similar named candy
candy_alt_names <- enframe(candy_list) %>%
  unnest_longer(col = value, values_to = "replace_this")

candy_renamed <- candy_combined %>% 
  # combine similar sounding candy names
  left_join(candy_alt_names, by = join_by(candy == replace_this)) %>%
  mutate(candy = if_else(!is.na(name), name, candy)) %>% 
  select(-name) %>% 
  # check whether candy name is a real candy
  filter(candy %in% candy_names)



#####
# SECTION 5: SPLIT COUNTRIES INTO CANADA, UK, USA, ROW and NA

country_candy_data <- candy_renamed %>%   
  mutate(country = case_when(is.na(country) ~ NA,
                             str_detect(country, "^[\\.[:digit:]]+$") ~ NA,
                             # Rename as Canada
                             str_detect(country, "^(?i)canada") ~ "Canada",
                             # Rename as UK
                             str_detect(country, "^(?i)U[\\ ]*K$") ~ "UK",
                             str_detect(country, "^United Ki[:alpha:]*om$") ~ "UK",
                             str_detect(country, "^(?i)[es][:alpha:]{2,3}land$") ~ "UK",
                             # Rename as USA
                             str_detect(country, "^(?i)U[\\ \\.]*S") ~ "USA",
                             str_detect(country, "(?i)unit[esd]+ s") ~ "USA",
                             str_detect(country, "((?i)m)er") ~ "USA",
                             country %in% country_list$`United States of America` ~ "USA",
                             # Rename as ROW
                             TRUE ~ "ROW"))



#####
# SECTION 6: UPDATE AGE LIST TO BE 4-100 years old; set rest to NA

age_country_candy_data <- country_candy_data %>% 
  mutate(age_floor = floor(as.numeric(age)),
         age = if_else(age_floor > 3 & age_floor <= 100,
                       age_floor,
                       NA)) %>% 
  select(-age_floor)



#####
# SECTION 7: EXPORT CLEAN DATA AS CSV
age_country_candy_data %>%
  select(year, rater_id, age, gender, country, 
         going_trick_or_treating, candy, rating) %>% 
  write_csv(here::here("clean_data/candy_clean.csv"))
