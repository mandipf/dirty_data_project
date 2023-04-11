library(readr)
library(tidyverse)

# import dirty data
decathlon_dirty_data <- read_rds(here::here("raw_data/decathlon.rds"))

# convert to long format data and create clean column names
decathlon_clean_data <- decathlon_dirty_data %>% 
  tibble::rownames_to_column("competitor") %>% 
  janitor::clean_names() %>% 
  pivot_longer(cols = x100m:x1500m,
               names_to = "event",
               values_to = "score"
               ) %>% 
  mutate(competitor = str_to_title(competitor),
         event = str_replace_all(event, "_", " "),
         event = str_remove(event, "^x")
         )

# export clean data
decathlon_clean_data %>% 
  write_csv(here::here("clean_data/decathlon_clean.csv"))
