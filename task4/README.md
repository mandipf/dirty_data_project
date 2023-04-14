---
editor_options: 
  markdown: 
    wrap: sentence
---

# **Task 4 - Halloween Candy Data**
By Mandip Farmahan (2023-04-12)

------------------------------------------------------------------------

## Project Description

This project cleans some Halloween Candy data and performs some analysis on that cleaned data. The data itself, which is in the form of three Excel-based questionnaires (2015, 2016 and 2017), was provided as part of a CodeClan Dirty Data Project. 

Assumptions made about the data:

-   There are no duplicates (raters) across the three year data set;
-   Age range of people surveyed were 4 to 100. Anything outside this range was marked N/A;
-   Candy names within the free text columns were separated using full stops, commas or semi-colons;
- Any candy which had less than 10 entries is removed from the data set to remove any pattern errors when searching free text columns.

------------------------------------------------------------------------

## Data cleaning

The data cleaning scripts are located in the `data_cleaning_scripts` folder. There are two scripts within this folder:

-   The first uses the distinct column headers only for candy name (`candy_data_cleaning_without_text_field.R`);
-   The other also extracts candy data from the free text field for other candies described as joy, meh or despair (`candy_data_cleaning.R`).

There are also three scripts within the sub folder `lists`. These contain lists of:

-   Recognised candy names (`candy_names.R`);
-   Alternative entries for certain candy used within the questionnaires (`alt_entry_names_for_candy.R`);
-   Alternative entries for certain countries used within the questionnaires (`alt_entry_names_for_countries.R`).

The libraries required to run the data cleaning script are:

```         
library(here)
library(janitor)
library(readxl)
library(tidyverse)
```

<br>

The following data cleaning tasks are carried out in four stages:

**1. Function definitions**

- Function 1
  - Import the xlsx file;
  - Add columns for `year` and `rater_id`, which is made up of the row number and questionnaire year;
  - Clean the column names.
  
```
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
```

- Function 2 (`candy_data_cleaning.R` only)
  - Convert the data to long format (headers `candy` and `rating`);
  - Search through the free text columns for additional candy ratings;
  - Remove any empty (N/A) ratings.

```
# function to convert other candy bar column to usable candy and rating data

others_to_candy <- function(df, start_column, end_column) {
  df %>% 
    pivot_longer(cols = all_of(start_column):all_of(end_column),
                 names_to = "rating",
                 values_to = "candy"
    ) %>%
    separate_longer_delim(cols = candy,
                          delim = regex("[//.//,//;]")
    ) %>% 
    filter(!is.na(candy) & candy != "")
}
```

<br>

**2. Import and clean each data set to standardise them, then combine them**

- For each data set:
  - Import file using Function 1;
  - Rename columns relating the rater (age, country etc) to be consistent across all three files;
  - Convert the data from the candy named columns into long format (`candy` and `rating`) and remove any empty (N/A) ratings;
  - Use Function 2 to extract ratings for free text candy  (`candy_data_cleaning.R` only);
  - Combine the data from the named columns and the extracted columns for a complete data set for that year (`candy_data_cleaning.R` only).

- Combine all three files for further processing.

```
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

candy_2015_other_items <- candy_2015_col_clean %>% 
  select(year, rater_id, age,
         going_trick_or_treating,
         joy, despair
  ) %>%
  others_to_candy("joy", "despair")

candy_2015_data <- bind_rows(candy_2015_main_list,
                             candy_2015_other_items)
```
```
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

candy_2016_other_items <- candy_2016_col_clean %>% 
  select(year, rater_id, age, gender,
         going_trick_or_treating,
         country,
         joy, despair) %>% 
  others_to_candy("joy", "despair")

candy_2016_data <- bind_rows(candy_2016_main_list,
                             candy_2016_other_items)
```
```
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

candy_2017_other_items <- candy_2017_col_clean %>% 
  select(year, rater_id, age, gender,
         going_trick_or_treating,
         country,
         joy, despair) %>%
  others_to_candy("joy", "despair")

candy_2017_data <- bind_rows(candy_2017_main_list,
                             candy_2017_other_items)
```
```
# combine all 3 data sets and remove redundant columns

all_candy_data_combined <- bind_rows(candy_2015_data,
                                     candy_2016_data,
                                     candy_2017_data)


all_candy_data_combined <- all_candy_data_combined %>% 
  select(year, rater_id, age, gender, country, 
         going_trick_or_treating, candy, rating)
```

<br>

**3. Deep clean of full data set**

  - Create a table of accepted alternative candy entries (spelling errors etc) using `alt_entry_names_for_candy.R`;
  - Join this table to the combined data set;
  - Create a table of recognised candy names using `candy_names.R`;
  - Filter the combined data set against this candy name table;
  - Remove any candies with less than 10 entries for pattern detection errors;
  - Decode country entries using REGEX patterns;
  - Create a table of accepted alternative country entries (spelling errors etc) using `alt_entry_names_for_countries.R`;
  - Join this table to the combined data set;
  - Convert age column to numeric and set all entries below 1 or above 100 to N/A.

```
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
```
```
# check candy names exists and remove those with less than 10 entries

source(here::here("data_cleaning_scripts/lists/candy_names.R"))

fixed_candy_data <- alt_candy_combined %>% 
  mutate(is_candy = (candy %in% candy_names)) %>%
  filter(is_candy) %>%
  group_by(candy) %>%
  filter(n() > 9) %>%
  ungroup() %>% 
  select(-is_candy)
```
```
# filter countries into Canada, UK, USA and others_unknown

source(here::here("data_cleaning_scripts/lists/alt_entry_names_for_countries.R"))

alternative_country_names <- enframe(country_list) %>%
  unnest_longer(col = value, values_to = "replace_this")

candy_and_country_data <- fixed_candy_data %>% 
  left_join(alternative_country_names, by = join_by(country == replace_this)) %>% 
  mutate(name_2 = case_when(
    str_detect(country, "^(?i)canada") ~ "Canada",
    str_detect(country, "^(?i)unite[sd] st") ~ "United States of America",
    str_detect(country, "^(?i)U[// //.]*S") ~ "United States of America",
    str_detect(country, "mer") ~ "United States of America",
    str_detect(country, "^(?i)U[// ]*K$") ~ "United Kingdom",
    str_detect(country, "^United Ki[:alpha:]*om$") ~ "United Kingdom",
    TRUE ~ NA),
    .after = name
  ) %>%
  mutate(country = case_when(
    !is.na(name_2) ~ name_2, !is.na(name) ~ name,
    TRUE ~ "Other or Unknown")
  ) %>% 
  select(-name, -name_2)
```
```
# update age list to be 1-100 years old, rest are NA

age_candy_country_data <- candy_and_country_data %>% 
  mutate(age_floor = floor(as.numeric(age)),
         age = if_else(age_floor > 0 & age_floor <= 100,
                       age_floor, NA)) %>% 
  select(-age_floor)
```

<br>

**4. Select required columns for data analysis and export to CSV**
```
age_candy_country_data %>%
  select(year, rater_id, age, gender, country, 
         going_trick_or_treating, candy, rating) %>% 
  write_csv(here::here("clean_data/candy_clean.csv"))
```

------------------------------------------------------------------------

## Data analysis

The data analysis scripts are located in the `analysis_and_documentation` folder. There are two scripts within this folder, which correspond to `candy_data_cleaning.R`and `candy_data_cleaning_without_text_field.R`. The only difference between these data analysis scripts is the location of the CSV file produced by the respective cleaning script.

The libraries required to run the data analysis script are:

```         
library(assertr)
library(here)
library(tidyverse)
```

The presence of all required variables within the imported file are verified before any analysis is conducted. This also includes a class verification for numeric fields.

<br>

### Analysis questions

With the exception of Q1, the answers provided by both data analysis scripts matched although the numbers were slightly higher in the complete data set. The example outputs are provided only for the complete data set, i.e. the data set also containing the free text fields.


<br>

#### Q1

What is the total number of candy ratings given across the three years?

```         
candy_data  %>%
  summarise(number_of_ratings = n())

##   number_of_ratings
##               <int>
## 1            623513
```

This code chunk showed that there were 623513 candy ratings within the complete data set. 

- Split by year, this was 384024 in 2015, 97340 in 2016, and 142149 in 2017.

When using the reduced data set (no free text field), this was reduced to 620373 candy ratings.

- Split by year, this was 381871 in 2015, 96959 in 2016, and 141543 in 2017.


<br>

#### Q2

What was the average age of people who are going out trick or treating?

```         
candy_data_unique_rater  %>%
  filter(going_trick_or_treating == "Yes") %>% 
  summarise(average_age = mean(age, na.rm = TRUE))
  
##   average_age
##         <dbl>
## 1        35.1
```

The average age for trick or treaters was 35.1 years old.


<br>

#### Q3

What was the average age of people who are not going trick or treating?

```         
candy_data_unique_rater  %>%
  filter(going_trick_or_treating == "No") %>% 
  summarise(average_age = mean(age, na.rm = TRUE))
  
##   average_age
##         <dbl>
## 1        39.0
```

The average age for people not trick or treating was 39.0 years old.


<br>

#### Q4

For each of joy, despair and meh, which candy bar received the most of these ratings?

```         
candy_data %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(rating, candy) %>% 
  summarise(number_of_ratings = n()) %>%
  slice_max(number_of_ratings, n = 1)
  
##   rating  candy              number_of_ratings
##   <chr>   <chr>                          <int>
## 1 despair peanut butter bars              6006
## 2 joy     toblerone bar                   6176
## 3 meh     100 grand bar                   1307
```

For candy bars:

-   the highest rating for joy was Toblerone bar
-   the highest rating for despair was Peanut Butter bars
-   the highest rating for meh was 100 Grand bar

For all candy:

-   the highest rating for joy and meh was m&ms
-   the highest rating for despair was Mary Jane


<br>

#### Q5

How many people rated Starburst as despair?

```         
candy_data %>% 
  filter(candy == "starburst" & rating == "despair") %>% 
  summarise(number_of_people_rated_starburst_as_despair = n())
  
##   number_of_people_rated_starburst_as_despair
##                                         <int>
## 1                                        1990
```

1990 people rated Starburst as despair.


<br>

#### Q6

What was the most popular candy bar for each gender in the dataset? Count despair as -1, joy as +1, and meh as 0.

```         
candy_data_with_rating_numbers %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(gender, candy) %>%
  summarise(rating_value = sum(rating_number, na.rm = TRUE)) %>% 
  slice_max(rating_value)
  
##   gender             candy         rating_value
##   <chr>              <chr>                <dbl>
## 1 Female             toblerone bar          716
## 2 I'd rather not say dove bars               53
## 3 Male               toblerone bar         1238
## 4 Other              toblerone bar           23
## 5 <NA>               toblerone bar         2807
```

For candy bars:

-   the highest rating for female was Toblerone bar
-   the highest rating for male was Toblerone bar
-   the highest rating for other was Toblerone bar
-   the highest rating for "I'd rather not say" was Dove bar
-   the highest rating for unanswered (N/A) was Toblerone bar

For all candy, the highest rated by everyone was m&ms.


<br>

#### Q7

What was the most popular candy bar in each year? Count despair as -1, joy as +1, and meh as 0.

```         
candy_data_with_rating_numbers %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(year, candy) %>%
  summarise(rating_value = sum(rating_number, na.rm = TRUE)) %>% 
  slice_max(rating_value)
  
##    year candy         rating_value
##   <dbl> <chr>                <dbl>
## 1  2015 toblerone bar         2797
## 2  2016 toblerone bar          802
## 3  2017 toblerone bar         1236
```

The highest rated candy bar every year was Toblerone bar.

For all candy types, it was m&ms every year.


<br>

#### Q8

What was the most popular candy bar by this rating for people in US, Canada, UK, and all other countries? Count despair as -1, joy as +1, and meh as 0.

```         
candy_data_with_rating_numbers %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(country, candy) %>%
  summarise(rating_value = sum(rating_number, na.rm = TRUE)) %>% 
  slice_max(rating_value)
  
##   country                  candy         rating_value
##   <chr>                    <chr>                <dbl>
## 1 Canada                   toblerone bar          220
## 2 Other or Unknown         toblerone bar         2894
## 3 United Kingdom           toblerone bar           33
## 4 United States of America toblerone bar         1688
```

The highest rated candy bar every region was Toblerone bar.

For all candy types, it was m&ms for every region.
