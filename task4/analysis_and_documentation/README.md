---
editor_options: 
  markdown: 
    wrap: sentence
---

# **Task 4 - Halloween Candy Data**

------------------------------------------------------------------------

## Project Description

This project cleans some Halloween Candy Data and performs some analysis on that cleaned data. The data itself, which is in the form of three Excel-based questionnaires (2015, 2016 and 2017), was provided as part of a CodeClan Dirty Data Project. 

Assumptions made about the data:

-   There are no duplicates (raters) across the three year data set;
-   Age range of people surveyed were 4 to 100. Anything outside this range was marked N/A;
-   Candy names within the free text columns were separated using full stops, commas or semi-colons;
- Any candy which had less than 10 entries was removed from the data set to remove any pattern errors when searching free text columns.

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

### Section 1: Function definitions
This section defines the two functions used later in the data cleaning script.

**Function 1**

- Imports the xlsx file;
- Adds columns for `year` and `rater_id`, which is made up of the row number and questionnaire year;
- Cleans the column names.


**Function 2** (only required for free text field search)

- Converts the data to long format (headers `candy` and `rating`);
- Searches through the free text columns for additional candy ratings;
- Remove any empty (N/A) ratings.


### Section 2: Import and clean each data set, then combine them
This section is used to import each data file and standardise it to combine all three data sets into one data set. For each data set (2015, 2016, 2017), the following is carried out:

- Import file using Function 1;
- Rename columns relating the rater (age, country etc) to be consistent across all three files;
- Convert the data from the candy named columns into long format (`candy` and `rating`) and remove any empty (N/A) ratings;
- Use Function 2 to extract ratings for free text candy;
- Combine the data from the named columns and the extracted columns for a complete data set for that year.

Each of the three data sets were then combined for further processing.

Note: Function 2 is not used in `candy_data_cleaning_without_text_field.R`. Therefore only the data from the named columns is used in this reduced analysis.


### Section 3: Deep clean of full data set
This section is used to deep clean the combined data set as follows:

- Create a table of accepted alternative candy entries (spelling errors etc) using `alt_entry_names_for_candy.R`;
- Join this table to the combined data set;
- Create a table of recognised candy names using `candy_names.R`;
- Filter the combined data set against this candy name table;
- Remove any candies with less than 10 entries for pattern detection errors;
- Decode country entries using REGEX patterns;
- Create a table of accepted alternative country entries (spelling errors etc) using `alt_entry_names_for_countries.R`;
- Join this table to the combined data set;
- Convert age column to numeric and set all entries below 1 or above 100 to N/A.


### Section 4:Export clean data as CSV
This section selects the required variables (columns) for data analysis and exports the data to a CSV file.


------------------------------------------------------------------------

## Data analysis

The data analysis scripts are located in the `analysis_and_documentation` folder. There are two scripts within this folder:

-   The first analyses data cleaned with `candy_data_cleaning_without_text_field.R`.
-   The other analyses data cleaned with `candy_data_cleaning.R`.

The libraries required to run the data analysis script are:

```         
library(assertr)
library(here)
library(tidyverse)
```

The presence of all required fields within the imported file are verified before any analysis is conducted. This also includes a class verification for numeric fields.

### Analysis questions

With the exception of Q1, the answers provided by both data analysis scripts matched although the numbers were slightly higher in the complete data set.

#### Q1

What is the total number of candy ratings given across the three years?

```         
candy_data  %>%
  summarise(number_of_ratings = n())
```

This code chunk showed that there were 623513 candy ratings within the complete data set. Split by year, this was 384024 in 2015, 97340 in 2016, and 142149 in 2017.

When using the reduced data set (no free text field), this was reduced to 620373 candy ratings.Split by year, this was 381871 in 2015, 96959 in 2016, and 141543 in 2017.


#### Q2

What was the average age of people who are going out trick or treating?

```         
candy_data_unique_rater  %>%
  filter(going_trick_or_treating == "Yes") %>% 
  summarise(average_age = mean(age, na.rm = TRUE))
```

The average age for trick or treaters was 35.1 years old.


#### Q3

What was the average age of people who are not going trick or treating?

```         
candy_data_unique_rater  %>%
  filter(going_trick_or_treating == "No") %>% 
  summarise(average_age = mean(age, na.rm = TRUE))
```

The average age for people not trick or treating was 39.0 years old.


#### Q4

For each of joy, despair and meh, which candy bar received the most of these ratings?

```         
candy_data %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(rating, candy) %>% 
  summarise(number_of_ratings = n()) %>%
  slice_max(number_of_ratings, n = 1)
```

For candy bars:

-   the highest rating for joy was Toblerone bar
-   the highest rating for despair was Peanut Butter bars
-   the highest rating for meh was 100 Grand bar

For all candy:

-   the highest rating for joy and meh was m&ms
-   the highest rating for despair was Mary Jane


#### Q5

How many people rated Starburst as despair?

```         
candy_data %>% 
  filter(candy == "starburst" & rating == "despair") %>% 
  summarise(number_of_people_rated_starburst_as_despair = n())
```

1990 people rated Starburst as despair.


#### Q6

What was the most popular candy bar for each gender in the dataset? Count despair as -1, joy as +1, and meh as 0.

```         
candy_data_with_rating_numbers %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(gender, candy) %>%
  summarise(rating_value = sum(rating_number, na.rm = TRUE)) %>% 
  slice_max(rating_value)
```

For candy bars:

-   the highest rating for female was Toblerone bar
-   the highest rating for male was Toblerone bar
-   the highest rating for other was Toblerone bar
-   the highest rating for "I'd rather not say" was Dove bar
-   the highest rating for unanswered (N/A) was Toblerone bar

For all candy, the highest rated by everyone was m&ms.


#### Q7

What was the most popular candy bar in each year? Count despair as -1, joy as +1, and meh as 0.

```         
candy_data_with_rating_numbers %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(year, candy) %>%
  summarise(rating_value = sum(rating_number, na.rm = TRUE)) %>% 
  slice_max(rating_value)
```

The highest rated candy bar every year was Toblerone bar.

For all candy types, it was m&ms every year.


#### Q8

What was the most popular candy bar by this rating for people in US, Canada, UK, and all other countries? Count despair as -1, joy as +1, and meh as 0.

```         
candy_data_with_rating_numbers %>%
  filter(str_detect(candy, "bar")) %>% 
  group_by(country, candy) %>%
  summarise(rating_value = sum(rating_number, na.rm = TRUE)) %>% 
  slice_max(rating_value)
```

The highest rated candy bar every region was Toblerone bar.

For all candy types, it was m&ms for every region.


------------------------------------------------------------------------

## License

Distributed under the MIT License.
See `LICENSE.txt` for more information.
