library(tidyverse)


# import dirty data and perform general clean of column names (function)

import_xlsx_and_clean_names <- function(file_name) {
  df <- readxl::read_xlsx(here::here(str_c("raw_data/", file_name)),
                col_types = "text")
  
  df_clean <- df %>% 
    janitor::clean_names() %>% 
    mutate(year = str_extract(file_name, "20[:digit:]{2}"),
           .before = everything()
           )
}

candy_2015_data <- import_xlsx_and_clean_names("boing-boing-candy-2015.xlsx")
candy_2016_data <- import_xlsx_and_clean_names("boing-boing-candy-2016.xlsx")
candy_2017_data <- import_xlsx_and_clean_names("boing-boing-candy-2017.xlsx")


# secondary clean of candy_2017_data column names

candy_2017_data_2 <- candy_2017_data %>%
  rename_with(~str_remove(., "^q[1-6]_"))


#combine all 3 data sets

all_candy_data <- bind_rows(candy_2015_data,
                            candy_2016_data,
                            candy_2017_data_2)


all_candy_final <- data.frame(names(all_candy_data))

# export clean data
all_candy_data %>%
  write_csv(here::here("clean_data/candy_clean_data.csv"))

all_candy_final %>%
  write_csv(here::here("clean_data/candy_clean.csv"))

