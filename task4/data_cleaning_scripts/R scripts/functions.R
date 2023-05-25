# function to import dirty data and add year and rater_id columns
import_xlsx_and_clean_names <- function(file_name) {
  df <- readxl::read_xlsx(here::here(str_c("raw_data/", file_name)),
                          col_types = "text")
  
  df_clean <- df %>% 
    mutate(year = str_extract(file_name, "20[:digit:]{2}"),
           rowid_to_column(df, "rater_id"),
           rater_id = str_c(year, "_", rater_id),
           .before = everything()
    )
}
 