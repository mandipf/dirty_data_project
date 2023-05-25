
# **Task 4 - Halloween Candy Data**
By Mandip Farmahan (2023-04-15)

------------------------------------------------------------------------

## Project Description

This project cleans some Halloween Candy data and performs some analysis on that cleaned data. The data itself, which is in the form of three Excel-based questionnaires (2015, 2016 and 2017), was provided as part of a CodeClan Dirty Data Project. 

Assumptions made about the data:

- There are no duplicates (raters) across the three year data set;
- Age range of people surveyed were 4 to 100. Anything outside this range was marked N/A;
- Candy names within the others rated as joy/despair columns were separated using commas or semi-colons;
- Any candy within the others rated as joy/despair columns which had less than 10 entries is removed from the data set to remove any pattern errors when searching free text columns.
- "any full sized candy bar" removed from data set as too generic

Task 4 was updated and the candy rating analysis was completely re-written. There were minor updates to the country and age cleaning sections.

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

Data was collected using dedicated questions for certain candy types. There were also free text fields to collect names of other candy which brought joy/despair. If the data cleaning script is fully executed, all data will be exported for analysis. 

To remove the additional data from the free text fields, Section 3 should be removed or commented out. No other changes are required. 

<br>

The data cleaning tasks are carried out in seven stages:

**1. Import files**

  - Import each file using Import function;
  - Rename columns relating the rater (`age`, `country` etc) to be consistent across all three files;
  - Remove any surplus columns in rater details section (`Timestamp` and `Internal ID`).

**2. Standardise main questionnaire with named columns**  
  
  - Select the required columns from each imported data set;
  - Convert the data from the candy named columns into long format (`candy` and `rating`) and remove any empty (N/A) ratings;
  - Combine the data from the named columns;
  - Remove wrapper characters from the rating column to leave candy names.
  
**3. Standardise main questionnaire with any other joy/despair data** 

  - Select the required columns from each imported data set;
  - Combine the three data sets and convert the data from the candy named columns into long format (`candy` and `rating`);
  - remove any empty string or NA columns;
  - remove any candy names with less than 10 entries as stated in assumptions above;
  - combine this data with the data from section 2.
  
**4. Combine similar named candy and check whether entry is an official candy**

  - Create a table of accepted alternative candy entries (spelling errors etc) using `lookup_candy_alt_names.R`;
  - Join this table to the combined data set;
  - Filter the combined data set against the list of official candy names in `lookup_candy_approved_names.R`.
  
**5. Split countries into Canada, UK, USA, ROW and NA** 

  - Decode country entries using REGEX patterns;
  - Compare the remaining entires against a list of accepted alternative country entries (spelling errors etc) using `lookup_countries_alt_names.R`;
  - Rename any country other than Canada, UK or USA as ROW.

**6. Update age list to be 4-100 years old and set others as NA**   

  - Convert age column to numeric and set all entries below 4 or above 100 to N/A.

**7. Select required columns for data analysis and export to CSV**

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
