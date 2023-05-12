
# **Task 4 - Halloween Candy Data v2**
By Mandip Farmahan (2023-04-15)

------------------------------------------------------------------------

## Project Description

This project cleans some Halloween Candy data and performs some analysis on that cleaned data. The data itself, which is in the form of three Excel-based questionnaires (2015, 2016 and 2017), was provided as part of a CodeClan Dirty Data Project. 

Assumptions made about the data:

-   There are no duplicates (raters) across the three year data set;
-   Age range of people surveyed were 4 to 100. Anything outside this range was marked N/A;
-   Candy names within the others rated as joy/despair columns were separated using commas or semi-colons;
- Any candy within the others rated as joy/despair columns which had less than 10 entries is removed from the data set to remove any pattern errors when searching free text columns.
- "any full sized candy bar" removed from data set as too generic

As part of Task 4 v2, the candy rating analysis was completely re-written. There were minor updates to the country and age cleaning sections.

------------------------------------------------------------------------

## Data cleaning

The data cleaning script is located in the `data_cleaning_scripts` folder. 

There are also four other scripts within sub-folder `R scripts`:

- An import function (`functions.R`);
- Recognised candy names (`lookup_candy_approved_names.R`);
- Alternative entries for certain candy used within the questionnaires (`lookup_candy_alt_names.R`);
- Alternative entries for certain countries used within the questionnaires (`lookup_countries_alt_names.R`).

The libraries required to run the data cleaning script are:

```         
library(here)
library(janitor)
library(readxl)
library(tidyverse)
```

Data was collected using dedicated questions for certain candy types. There were also free text fields to collect names of other candy which brought joy/despair. If the data cleaning script is fully executed, all data will be exported for analysis. To remove the additional data from the free text fields, Section 3 should be removed or commented out. No other changes are required. 

<br>

The data cleaning tasks are carried out in seven stages:

**1. Import files**

  - Import each file using Import function;
  - Rename columns relating the rater (`age`, `country` etc) to be consistent across all three files;
  - Remove any surplus columns in rater details section (`Timestamp` and `Internal ID`).

```
# import function definition
# add columns for `year` and `rater_id` (made up of the row number and questionnaire year)

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
```
```
# file import

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
```
 
 <br> 

**2. Standardise main questionnaire with named columns**  
  
  - Select the required columns from each imported data set;
  - Convert the data from the candy named columns into long format (`candy` and `rating`) and remove any empty (N/A) ratings;
  - Combine the data from the named columns;
  - Remove wrapper characters from the rating column to leave candy names.
  
```
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
```
```
# combine all three data sets

candy_combined <- bind_rows(candy_2015_main, candy_2016_main,
                            candy_2017_main) %>% 
  # remove wrapper characters from candy column and format rating column
  mutate(rating = str_to_lower(rating),
         candy = str_remove(candy, "^\\[|Q6 \\| "),
         candy = str_remove(candy, "\\]$"),
         candy = str_to_lower(candy))
```

<br>

**3. Standardise main questionnaire with any other joy/despair data** 

  - Select the required columns from each imported data set;
  - Combine the three data sets and convert the data from the candy named columns into long format (`candy` and `rating`);
  - remove any empty string or NA columns;
  - remove any candy names with less than 10 entries as stated in assumptions above;
  - combine this data with the data from section 2.
  
```
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
```
```
# combine all three extra data sets

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
```
```
# Join main questionnaire responses to other joy/despair responses

candy_combined <- bind_rows(candy_combined,
                            candy_extra_combined)
```

<br>

**4. Combine similar named candy and check whether entry is an official candy**

  - Create a table of accepted alternative candy entries (spelling errors etc) using `lookup_candy_alt_names.R`;
  - Join this table to the combined data set;
  - Filter the combined data set against the list of official candy names in `lookup_candy_approved_names.R`.
  
```
# create list to combine similar named candy

candy_alt_names <- enframe(candy_list) %>%
  unnest_longer(col = value, values_to = "replace_this")
```
```
candy_renamed <- candy_combined %>% 
  # combine similar sounding candy names
  left_join(candy_alt_names, by = join_by(candy == replace_this)) %>%
  mutate(candy = if_else(!is.na(name), name, candy)) %>% 
  select(-name) %>% 
  # check whether candy name is a real candy
  filter(candy %in% candy_names)
```
 
<br>

**5. Split countries into Canada, UK, USA, ROW and NA** 

  - Decode country entries using REGEX patterns;
  - Compare the remaining entires against a list of accepted alternative country entries (spelling errors etc) using `lookup_countries_alt_names.R`;
  - Rename any country other than Canada, UK or USA as ROW.

```
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
```

<br>

**6. Update age list to be 4-100 years old and set others as NA**   

  - Convert age column to numeric and set all entries below 4 or above 100 to N/A.

```
age_country_candy_data <- country_candy_data %>% 
  mutate(age_floor = floor(as.numeric(age)),
         age = if_else(age_floor > 3 & age_floor <= 100,
                       age_floor,
                       NA)) %>% 
  select(-age_floor)
```

<br>

**7. Select required columns for data analysis and export to CSV**
```
age_country_candy_data %>%
  select(year, rater_id, age, gender, country, 
         going_trick_or_treating, candy, rating) %>% 
  write_csv(here::here("clean_data/candy_clean.csv"))
```

------------------------------------------------------------------------

## Data analysis

The data analysis script is located in the `analysis_and_documentation` folder. The libraries required to run the data analysis script are:

```         
library(assertr)
library(here)
library(tidyverse)
```

The presence of all required variables within the imported file are verified before any analysis is conducted. This also includes a class verification for numeric fields.

<br>

### Analysis questions

With the exception of Q1, the answers provided with and without Section 3 of the data cleaning script match, however some of the counts are higher with Section 3. The outputs below are provided only for the complete data set, i.e. including Section 3 of the data cleaning script.

<br>

#### Q1

What is the total number of candy ratings given across the three years?

```         
candy_data  %>%
  summarise(number_of_ratings = n())

##   number_of_ratings
##               <int>
## 1            631057
```

This code chunk showed that there were 631057 candy ratings within the complete data set. 

- Split by year, this was 388828 in 2015, 98474 in 2016, and 143755 in 2017.

When using the reduced data set (no free text field), this was reduced to 628478 candy ratings.

- Split by year, this was 387023 in 2015, 98164 in 2016, and 143291 in 2017.


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
## 1 despair reggie jackson bar              4506
## 2 joy     kit kat bar                     7252
## 3 meh     100 grand bar                   1307
```

For candy bars:

-   the highest rating for joy was Kit Kat bar
-   the highest rating for despair was Reggie Jackson bar
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
##   <chr>              <chr>              <dbl>
## 1 Female             kit kat bar          766
## 2 I'd rather not say kit kat bar           62
## 3 Male               kit kat bar         1439
## 4 Other              twix bar              32
## 5 <NA>               kit kat bar         4358
```

For candy bars:

-   the highest rating for female was Kit Kat bar
-   the highest rating for male was Kit Kat bar
-   the highest rating for other was Twix bar
-   the highest rating for "I'd rather not say" was Kit Kat bar
-   the highest rating for unanswered (N/A) was Kit Kat bar

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
##   <dbl> <chr>              <dbl>
## 1  2015 kit kat bar         4340
## 2  2016 kit kat bar          920
## 3  2017 kit kat bar         1395
```

The highest rated candy bar every year was Kit Kat bar.

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
  
##   country candy         rating_value
##   <chr>   <chr>                <dbl>
## 1 Canada  kit kat bar            229
## 2 ROW     kit kat bar             71
## 3 UK      toblerone bar           33
## 4 USA     kit kat bar           1951
## 5 <NA>    kit kat bar           4377
```

The highest rated candy bar every region except Uk was Kit Kat bar. In the UK, it was Toblerone bar.

For all candy types, it was m&ms for every region.
