library(tidyverse)


# import dirty data
cake_ingredients_dirty <- read_csv(here::here("raw_data/cake-ingredients-1961.csv"),
                                   show_col_types = FALSE)

cake_ingredients_code <- read_csv(here::here("raw_data/cake_ingredient_code.csv"),
                                   show_col_types = FALSE)

# convert to long format data and convert ingredient abbreviations
cake_ingredients_clean <- cake_ingredients_dirty %>% 
  pivot_longer(cols = -Cake,
               names_to = "code",
               values_to = "quantity"
               ) %>%
  janitor::clean_names() %>% 
  inner_join(cake_ingredients_code, by = "code") %>% 
  select(cake, ingredient, quantity, measure)

# export clean data
cake_ingredients_clean %>% 
  write_csv(here::here("clean_data/cake_ingredients_clean.csv"))
