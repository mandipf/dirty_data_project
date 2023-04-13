---
editor_options: 
  markdown: 
    wrap: sentence
---

# **Task 3 - Seabird Observation Data**
By Mandip Farmahan (2023-04-12)

------------------------------------------------------------------------

## Project Description

This project cleans some Seabird observation data and performs some analysis on that cleaned data.
The data itself, which is in the form of an XLS file with two sheets, was provided as part of a CodeClan Dirty Data Project.

The first sheet `Bird data by record ID` contains the names and appearance of the seabirds observed.
The second sheet `Ship data by record ID` contains the times and location of each sighting.

It is assumed that there are no duplicates within the data set.

------------------------------------------------------------------------

## Data cleaning

The data cleaning script is located in the `data_cleaning_scripts` folder.
The libraries required to run the data cleaning script are:

```         
library(here)
library(janitor)
library(readxl)
library(tidyverse)
```

<br>

The following data cleaning tasks are carried out in two stages:

**1. Function definitions**

  - The function removes age, sex and plummage data from the species names and abbreviations using REGEX patterns in a specific order based on the order they appear in the species names.

  - The REGEX patterns originally consisted of the data within the `age`, `wanplum` and `plphase` columns. Records 25238 and 28499 of "Bird data by record ID" sheet list `age` as *JUV*, however *AD* is appended to the species names and abbreviations. Record 25733 of "Bird data by record ID" sheet lists `wanplum` as *5*, however *6* is appended to the species names and abbreviations. A value of 6 for `wanplum` is not valid based on the definitions the "Bird date codes" sheet of the XLS file. As a result, more generic patterns are used for `age`, `wanplum` and `plphase`.

**2. Import and clean data set, then export as CSV**

  -   Import both sheets from the XLS file;
  -   Join the two data sets using `RECORD ID` as the key;
  -   Clean the column names;
  -   Filter out observations marked as [NO BIRDS RECORDED];
  - Use the above function to remove age, sex and plummage data from the species names and abbreviations as these are thought to be entry errors and duplication of the dedicated `age`, `wanplum`, `plphase` and `sex` columns;
  - Remove variables not required for data analysis;
  -   Export data to CSV file.

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

### Analysis questions

#### Q1

Which bird had the most individual sightings?

```         
seabirds_data %>% 
  select(common_name, scientific_name, abbreviation) %>% 
  mutate(number_of_sightings = n(),
         .by = common_name) %>%
  unique() %>% 
  slice_max(number_of_sightings)

##   common_name                    scientific_name                 abbre…¹ numbe…²
##   <chr>                          <chr>                           <chr>     <int>
## 1 Wandering albatross sensu lato Diomedea antipodensis / exulans DIOANT…   11293
## # … with abbreviated variable names ¹​abbreviation, ²​number_of_sightings
```

The bird with the most sightings was:

- common name: Wandering albatross sensu lato	
- scientific name: Diomedea antipodensis / exulans
- species abbreviation: DIOANTEXU

<br>


#### Q2

Which bird had the highest total count?

```         
seabirds_data %>% 
  mutate(total_count = sum(count, na.rm = TRUE),
         .by = common_name) %>%
  select(common_name, scientific_name, abbreviation, total_count) %>%
  unique() %>% 
  slice_max(total_count)
  
##   common_name             scientific_name       abbreviation total_count
##   <chr>                   <chr>                 <chr>              <dbl>
## 1 Short-tailed shearwater Puffinus tenuirostris PUFTEN            982553
```

The bird with the highest total count was:

- common name: Short-tailed shearwater	
- scientific name: Puffinus tenuirostris
- species abbreviation: PUFTEN

<br>


#### Q3

Which bird had the highest total count above a latitude of -30?

```         
seabirds_data %>% 
  filter(latitude > -30) %>% 
  mutate(total_count = sum(count, na.rm = TRUE),
         .by = common_name) %>%
  select(common_name, scientific_name, abbreviation, total_count) %>%
  unique() %>% 
  slice_max(total_count)
  
##   common_name             scientific_name    abbreviation total_count
##   <chr>                   <chr>              <chr>              <dbl>
## 1 Wedge-tailed shearwater Puffinus pacificus PUFPAC               855
```

The bird with the highest total count above a latitude of -30 was:

- common name: Wedge-tailed shearwater	
- scientific name: Puffinus pacificus
- species abbreviation: PUFPAC

<br>


#### Q4

How many different types of birds were only ever seen in groups of 1?

```         
seabirds_data %>% 
  filter(count == 1) %>%
  select(common_name, scientific_name, abbreviation) %>% 
  unique()
  
##    common_name                       scientific_name                 abbreviat…¹
##    <chr>                             <chr>                           <chr>      
##  1 Seabird (Unidentified)            <NA>                            SEABUN     
##  2 White-chinned petrel              Procellaria aequinoctialis      PROAEQ     
##  3 Sooty shearwater                  Puffinus griseus                PUFGRI     
##  4 Northern giant petrel             Macronectes halli               MACHAL     
##  5 Wandering albatross sensu lato    Diomedea antipodensis / exulans DIOANTEXU  
##  6 Royal albatross sensu lato        Diomedea epomophora / sanfordi  DIOEPOSAN  
##  7 Australasian gannet               Morus serrator                  MORSER     
##  8 Diving-petrel (unidentified)      Pelecanoides sp.                PELSP      
##  9 Skua (unidentified)               Catharacta / Stercorarius sp.   SKUAUN     
## 10 Black-browed albatross sensu lato Diomedea impavida / melanophrys DIOIMPMEL  
## # … with 138 more rows, and abbreviated variable name ¹​abbreviation
```

There were 148 types of bird that were only ever seen alone.

<br>

#### Q5

How many penguins were seen?

```         
seabirds_data %>% 
  filter(str_detect(common_name, "(?i)penguin")) %>% 
  summarise(number_penguin_sightings = n(),
            number_of_penguins_seen = sum(count, na.rm = TRUE))
            
##   number_penguin_sightings number_of_penguins_seen
##                      <int>                   <dbl>
## 1                       70                     158
```

There were 158 penguins seen during 70 sightings.
