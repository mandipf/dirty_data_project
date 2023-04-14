
# **Task 1 - Decathlon Data**

By Mandip Farmahan (2023-04-12)

------------------------------------------------------------------------

## Project Description

This project cleans some Decathlon Data and performs some analysis on that cleaned data.
The data itself, which is in the form of a RDS file, was provided as part of a CodeClan Dirty Data Project.

It is assumed that there are no duplicates within the data set.

------------------------------------------------------------------------

## Data cleaning

The data cleaning script is located in the `data_cleaning_scripts` folder.
The libraries required to run the data cleaning script are:

```         
library(here)
library(janitor)
library(readr)
library(tidyverse)
```

<br>

The following data cleaning tasks are carried out:

-   Import file using the `readr` library;
-   Create a new column at the start with the names of the competitors from the row names;
-   Clean the column names;
-   Convert the data from wide format to long format (headings: `event` and `score`);
-   Convert the competitor names from UPPERCASE to Title case;
-   Export data to CSV file.

<br>

```
# import dirty data

decathlon_dirty_data <- read_rds(here::here("raw_data/decathlon.rds"))
```
```
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
```
```
# export clean data

decathlon_clean_data %>% 
  write_csv(here::here("clean_data/decathlon_clean.csv"))
```

------------------------------------------------------------------------

## Data analysis

The data analysis script is located in the `analysis_and_documentation` folder.
The libraries required to run the data analysis script are:

```         
library(assertr)
library(here)
library(tidyverse)
```

The presence of all required variables within the imported file are verified before any analysis is conducted.
This also includes a class verification for numeric fields.

<br>

### Analysis questions

#### Q1

Who had the longest long jump seen in the data?

```         
decathlon_data %>% 
  filter(event == "long jump") %>% 
  slice_max(score)
  
##   competitor  rank points competition event     score
##   <chr>      <dbl>  <dbl> <chr>       <chr>     <dbl>
## 1 Clay           2   8820 OlympicG    long jump  7.96
```

Clay had the longest jump.

<br>

#### Q2

What was the average 100m time in each competition?

```         
decathlon_data %>% 
  filter(event == "100m") %>% 
  summarise("average 100m time (s)" = mean(score),
            .by =competition)
            
##   competition `average 100m time (s)`
##   <chr>                         <dbl>
## 1 Decastar                       11.2
## 2 OlympicG                       10.9
```

The average 100m time was 11.2 seconds in Decastar and 10.9 seconds in OlympicG.

<br>

#### Q3

Who had the highest total points across both competitions?

```         
decathlon_data %>% 
  summarise(total_points = sum(points),
            .by = competitor) %>% 
  slice_max(total_points)
  
##   competitor total_points
##   <chr>             <dbl>
## 1 Sebrle           171100  
```

Serble had the highest points total across both competitions.

<br>

#### Q4

What was the shot-put scores for the top three competitors in each competition?

```         
decathlon_data %>% 
  filter(event == "shot put") %>% 
  group_by(competition) %>%
  slice_max(score, n = 3)
  
##   competitor  rank points competition event    score
##   <chr>      <dbl>  <dbl> <chr>       <chr>    <dbl>
## 1 Yurkov         5   8036 Decastar    shot put  15.2
## 2 Sebrle         1   8217 Decastar    shot put  14.8
## 3 Karpov         3   8099 Decastar    shot put  14.8
## 4 Sebrle         1   8893 OlympicG    shot put  16.4
## 5 Karpov         3   8725 OlympicG    shot put  15.9
## 6 Macey          4   8414 OlympicG    shot put  15.7
```

Decastar

-   The top three shot-put scores were 15.2m (Yurkov), 14.8m (Sebrle) and 14.8m (Karpov)

OlympicG

-   The top three shot-put scores were 16.4m (Sebrle), 15.9m (Karpov) and 15.7m (Macey)

<br>

#### Q5

What was the average points for competitors who ran the 400m in less than 50 seconds vs. those than ran 400m in more than 50 seconds?

```         
decathlon_data %>% 
  filter(event == "400m") %>% 
  mutate(less_than_50s = score < 50) %>%
  summarise(average_points = mean(points),
            .by = less_than_50s)
            
##   less_than_50s average_points
##   <lgl>                  <dbl>
## 1 TRUE                   8120.
## 2 FALSE                  7727.
```

The average points for competitors across both competitions who ran the 400m in less than 50 seconds was 8120, compared to 7727 for those who ran the 400m in more than 50 seconds.

If this was split by competition, the results were:

|               | Decastar | OlympicG |
|:-------------:|:--------:|:--------:|
| \< 50 seconds |   7988   |   8180   |
| \> 50 seconds |   7721   |   7730   |
