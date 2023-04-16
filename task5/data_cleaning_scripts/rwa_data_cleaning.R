library(tidyverse)


# Import data and add person identifier
rwa_import <- read_csv(here::here("raw_data/rwa.csv"),
                       show_col_types = FALSE)

rwa_data <- rwa_import %>% 
  mutate(rowid_to_column(rwa_import, "person_id"))


# calculate RWA score (based on Q3:Q22)  
rwa_score <- rwa_data %>% 
  select(person_id, Q3:Q22) %>%
  # Set 0 (no response) to NA
  # Reverse scores for 4, 6, 8, 9, 11, 13, 15, 18, 20, 21.
  mutate(across(.cols = c(Q4, Q6, Q8, Q9, Q11, Q13, Q15, Q18, Q20, Q21),
                .fns = ~ if_else(.x > 0,
                                 10 - .x,
                                 NA))) %>%
  # calculate mean of Q3:Q22 for each person_id
  pivot_longer(Q3:Q22) %>% 
  summarise(rwa = mean(value, na.rm = TRUE),
            .by = person_id)


# add columns required for data analysis and calculated RWA score
rwa_all_columns <- rwa_data  %>% 
  # gender, hand, family size, childhood, test time, education level, and age
  select(person_id, test_time = testelapse, education,
         childhood = urban, gender, age, hand, familysize) %>% 
  inner_join(rwa_score, by = "person_id")


# recode personal details from numbers to definitions in rwa_codebook
rwa_recode <- rwa_all_columns %>% 
  mutate(education = case_match(education,
                                1 ~ "Less than high school",
                                2 ~ "High school",
                                3 ~ "University degree",
                                4 ~ "Graduate degree",
                                .default = NA),
         
         childhood = case_match(childhood,
                            1 ~ "Rural (country side)",
                            2 ~ "Suburban",
                            3 ~ "Urban (town, city)",
                            .default = NA),
         
         gender = case_match(gender,
                             1 ~ "Male",
                             2 ~ "Female",
                             3 ~ "Other",
                             .default = NA),
         
         hand =  case_match(hand,
                            1 ~ "Right",
                            2 ~ "Left",
                            3 ~ "Both",
                            .default = NA),
         
         # test_time filtered to allow up to 10 minutes, rest are NA
         test_time = if_else(test_time <= 600,
                             test_time,
                             NA),
         
         # age filtered to allow 13 (minimum survey age) to 100, rest are NA
         age = if_else(age >= 13 & age <= 100,
                       age,
                       NA),
         
         # familysize filtered to allow 1 (person surveyed) to 13, rest are NA
         familysize = if_else(familysize > 0 & familysize <= 13,
                              familysize,
                              NA))


# export to CSV
rwa_recode %>% 
  write_csv(here::here("clean_data/rwa_clean.csv"))
