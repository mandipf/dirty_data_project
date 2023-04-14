
# **Task 2 - Cake Ingredients**
By Mandip Farmahan (2023-04-12)

------------------------------------------------------------------------

## Project Description

This project cleans some Cake Ingredients data and performs some analysis on that cleaned data. The data itself, which is in the form of two CSV files, was provided as part of a CodeClan Dirty Data Project.

The first CSV file contains the names of the cakes and the quantity of abbreviated ingredients. The second CSV file contains a key for the abbreviations (`code`) and the unit of measure for that ingredient.

It is assumed that there are no duplicates within the data set.

------------------------------------------------------------------------

## Data cleaning

The data cleaning script is located in the `data_cleaning_scripts` folder.
The libraries required to run the data cleaning script are:

```         
library(here)
library(janitor)
library(tidyverse)
```

<br>

The following data cleaning tasks are carried out:

-   Import both CSV files;
-   Convert the cake data (first CSV file) from wide format to long format (headings: `code` and `quantity`);
-   Clean the column names;
-   Join the two data sets using `code` as the key;
-   Export data to CSV file.

<br>

```
# import dirty data

cake_ingredients_dirty <- read_csv(here::here("raw_data/cake-ingredients-1961.csv"),
                                   show_col_types = FALSE)

cake_ingredients_code <- read_csv(here::here("raw_data/cake_ingredient_code.csv"),
                                   show_col_types = FALSE)
```
```
# convert to long format data and convert ingredient abbreviations

cake_ingredients_clean <- cake_ingredients_dirty %>% 
  pivot_longer(cols = -Cake,
               names_to = "code",
               values_to = "quantity"
               ) %>%
  janitor::clean_names() %>% 
  inner_join(cake_ingredients_code, by = "code") %>% 
  select(cake, ingredient, quantity, measure)
```
```
# export clean data

cake_ingredients_clean %>% 
  write_csv(here::here("clean_data/cake_ingredients_clean.csv"))
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

The presence of all required variables within the imported file are verified before any analysis is conducted. This also includes a class verification for numeric fields.

<br>

### Analysis questions


#### Q1

Which cake has the most cocoa in it?

```         
cake_data %>% 
  filter(ingredient == "Cocoa") %>%
  group_by(measure) %>% 
  slice_max(quantity)
  
##   cake               ingredient quantity measure   
##   <chr>              <chr>         <dbl> <chr>     
## 1 One Bowl Chocolate Cocoa            10 tablespoon
```

One Bowl Chocolate has the most cocoa in it.

<br>


#### Q2

For sponge cake, how many cups of ingredients are used in total?

```         
cake_data %>% 
  filter(cake == "Sponge") %>% 
  summarise(quantity_per_measure = sum(quantity, na.rm = TRUE),
            .by = measure)
            
##   measure    quantity_per_measure
##   <chr>                     <dbl>
## 1 teaspoon                   1.75
## 2 cup                        3.5 
## 3 one                        4   
## 4 tablespoon                 0   
## 5 pound                      0   
## 6 ounce                      0   
## 7 <NA>                       0   
## 8 quart                      0
```

Sponge cake has 3.5 *cups* of ingredients.


<br>

#### Q3

How many ingredients are measured in teaspoons?

```         
cake_data %>% 
  filter(measure == "teaspoon" & !is.na(quantity)) %>% 
  distinct(ingredient) %>% 
  arrange(ingredient)
  
##   ingredient     
##   <chr>          
## 1 Almond essence 
## 2 Baking powder  
## 3 Cream of tartar
## 4 Lemon rind     
## 5 Nutmeg         
## 6 Salt           
## 7 Soda           
## 8 Vanilla extract
```

8 ingredients are measured in teaspoons.


<br>

#### Q4

Which cake has the most unique ingredients?

```         
cake_data %>% 
  filter(!is.na(quantity)) %>%
  summarise(number_of_ingredients = n(),
            .by = cake) %>% 
  slice_max(number_of_ingredients, n = 1)
  
##   cake               number_of_ingredients
##   <chr>                              <int>
## 1 Babas au Rhum                         11
## 2 One Bowl Chocolate                    11
```

Babas au Rhum and One Bowl Chocolate were tied for the most unique ingredients.

<br>

#### Q5

Which ingredients are used only once?

```         
cake_data %>% 
  filter(!is.na(quantity)) %>% 
  count(ingredient) %>% 
  filter(n == 1)

##   ingredient          n
##   <chr>           <int>
## 1 Bananas             1
## 2 Cream of tartar     1
## 3 Crushed Ice         1
## 4 Dried currants      1
## 5 Egg white           1
## 6 Nutmeg              1
## 7 Nuts                1
## 8 Zwiebach            1
```

8 ingredients are used only once.
