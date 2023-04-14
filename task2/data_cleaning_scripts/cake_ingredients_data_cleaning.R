library(tidyverse)


# import dirty data
cake_ingredients_dirty <- read_csv(here::here("raw_data/cake-ingredients-1961.csv"),
                                   show_col_types = FALSE)

cake_ingredients_code <- read_csv(here::here("raw_data/cake_ingredient_code.csv"),
                                   show_col_types = FALSE)


# find units of measure (exclude NA) in code CSV and create REGEX pattern
units_measure_pattern <- cake_ingredients_code %>% 
  distinct(measure) %>% 
  drop_na() %>% 
  pull() %>% 
  str_c(collapse="|")

  
# split any concatenated ingredient-measure strings in code CSV
cake_ingredients_code_fixed <- cake_ingredients_code %>% 
  mutate(ingredient_2 =  if_else(str_detect(ingredient, units_measure_pattern),
                                 str_remove(ingredient, str_c(" ", units_measure_pattern)),
                                 ingredient),
         measure_2 = if_else(str_detect(ingredient, units_measure_pattern),
                             str_extract(ingredient, units_measure_pattern),
                             measure)) %>% 
  select(code,
         ingredient = ingredient_2,
         measure = measure_2)
            

# convert to long format data and convert ingredient abbreviations
cake_ingredients_clean <- cake_ingredients_dirty %>% 
  pivot_longer(cols = -Cake,
               names_to = "code",
               values_to = "quantity",
               values_drop_na = TRUE,
               ) %>%
  janitor::clean_names() %>% 
  inner_join(cake_ingredients_code_fixed, by = "code") %>% 
  select(cake, ingredient, quantity, measure)


# export clean data
cake_ingredients_clean %>% 
  write_csv(here::here("clean_data/cake_ingredients_clean.csv"))
