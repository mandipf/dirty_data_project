library(tidyverse)


# import dirty data
dog_owners_survey_data <- read_csv(here::here("raw_data/dog_survey.csv"),
                                   show_col_types = FALSE)


# Clean personal details and remove duplicate entries
dos_personal_info_clean <- dog_owners_survey_data %>% 
  janitor::clean_names() %>% 
  # remove blank columns
  select(-x10, -x11) %>% 
  # set email addresses without @ to NA
  mutate(email_2 = str_replace(email, ".com[m]+", ".com"),
         email_2 = case_when(str_detect(email_2, "@") ~ email_2, TRUE ~ NA),
         .after = email) %>% 
  # remove duplicates
  unique()


# Clean COST column
dos_cost_clean <- dos_personal_info_clean %>% 
  # set cost to NA if multiple dogs in row
  mutate(cost = case_when(str_detect(dog_age, ",") ~ NA,
                          str_detect(dog_age, "and") ~ NA,
                          TRUE ~ amount_spent_on_dog_food),
         .after = amount_spent_on_dog_food) %>% 
  # extract cost without units and set other rows to NA
  mutate(cost = case_when(str_detect(cost, " and ") ~ NA,
                          str_detect(cost, "-") ~ NA,
                          str_detect(cost, "/") ~ NA,
                          TRUE ~ str_extract(cost, "[//.[:digit:]]+")),
         # check cost is between 10 and 100 or set NA for typos
         cost = if_else(as.numeric(cost) < 10 | as.numeric(cost) > 100,
                        NA, cost))


# Clean DOG-RELATED columns
dog_owners_final <- dos_cost_clean %>% 
  # split multiple entries within same row 
  mutate(across(.cols = dog_size:dog_age,
                .fns = ~ str_replace(.x, " and ", ",")),
         across(.cols = dog_size:dog_age,
                .fns = ~ str_split(.x, ","))) %>% 
  unnest_longer(col = c(dog_size, dog_gender, dog_age)) %>%
  # convert words for size (e.g. large) to abbreviation (e.g. S, M, L)
  mutate(dog_size_2 = case_when(str_detect(dog_size, "^(?i)s") ~ "S",
                                str_detect(dog_size, "^(?i)m") ~ "M",
                                str_detect(dog_size, "^(?i)l") ~ "L",
                                TRUE ~ dog_size),
         # update size to be XS, S, M, L, XL or NA
         dog_size_2 = if_else(str_detect(dog_size_2, "[X]*[SML]"), 
                              dog_size_2,
                              NA),
         .after = dog_size) %>%
  # convert words for gender (e.g. female) to abbreviation (e.g. M, F)
  mutate(dog_gender_2 = str_remove_all(dog_gender, "[ [:digit:]]*"),
         dog_gender_2 = case_when(str_detect(dog_gender_2, "^(?i)m") ~ "M",
                                  str_detect(dog_gender_2, "^(?i)f") ~ "F",
                                  TRUE ~ NA),
         .after = dog_gender) %>%
  # set anything other than a number only to NA
  mutate(dog_age_2 = if_else(str_detect(dog_age, "^[:digit:]+$"), 
                             dog_age,
                             NA),
         .after = dog_age)


# export clean data
dog_owners_final %>% 
  select(id, title, first_name, last_name,
         email = email_2,
         amount_spent_on_dog_food = cost,
         dog_size = dog_size_2,
         dog_gender = dog_gender_2,
         dog_age = dog_age_2) %>% 
  write_csv(here::here("clean_data/dog_owners_survey_clean.csv"))
