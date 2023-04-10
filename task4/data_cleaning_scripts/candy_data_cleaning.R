library(tidyverse)

# FUNCTIONS

# function to import dirty data and perform general clean of column names
import_xlsx_and_clean_names <- function(file_name) {
  df <- readxl::read_xlsx(here::here(str_c("raw_data/", file_name)),
                col_types = "text")
  
  df_clean <- df %>% 
    janitor::clean_names() %>% 
    mutate(year = str_extract(file_name, "20[:digit:]{2}"),
           .before = everything()
           )
}

# function to convert other candy bar column to usable candy and rating data
others_to_candy <- function(df, start_column, end_column) {
  df %>% 
    pivot_longer(cols = all_of(start_column):all_of(end_column),
                 names_to = "rating",
                 values_to = "candy"
                 ) %>%
    separate_longer_delim(cols = candy,
                          delim = regex("[//.//,]")
                          ) %>% 
    separate_longer_delim(cols = candy,
                          delim = regex(" and ")
                          ) %>% 
    filter(!is.na(candy) & candy != "")
}





# IMPORT EACH DATA SET, CLEAN AND COMBINE THEM

# import of 2015 data set and deep clean of individual file
candy_2015_import <- import_xlsx_and_clean_names("boing-boing-candy-2015.xlsx")

candy_2015_col_clean <- candy_2015_import %>%
  rename(age = how_old_are_you,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         joy = please_list_any_items_not_included_above_that_give_you_joy,
         despair = please_list_any_items_not_included_above_that_give_you_despair
         )

candy_2015_main_list <- candy_2015_col_clean %>%
  select(-joy, -despair) %>% 
  pivot_longer(cols = butterfinger:please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_beyonce_knowles,
               names_to = "candy",
               values_to = "rating"
               ) %>% 
  mutate(candy = str_replace_all(candy, "_", " ")) %>% 
  filter(!is.na(rating))

candy_2015_other_items <- candy_2015_col_clean %>% 
  select(year, timestamp, age,
         going_trick_or_treating,
         joy, despair
         ) %>%
  others_to_candy("joy", "despair")

candy_2015_data <- bind_rows(candy_2015_main_list,
                             candy_2015_other_items)


# import of 2016 data set and deep clean of individual file
candy_2016_import <- import_xlsx_and_clean_names("boing-boing-candy-2016.xlsx")

candy_2016_col_clean <- candy_2016_import %>%
  rename(age = how_old_are_you,
         going_trick_or_treating = are_you_going_actually_going_trick_or_treating_yourself,
         gender = your_gender,
         country = which_country_do_you_live_in,
         state_province_county_etc = which_state_province_county_do_you_live_in,
         joy = please_list_any_items_not_included_above_that_give_you_joy,
         despair = please_list_any_items_not_included_above_that_give_you_despair
         )

candy_2016_main_list <- candy_2016_col_clean %>%
  select(-joy, -despair) %>%
  pivot_longer(cols = x100_grand_bar:york_peppermint_patties_ignore,
               names_to = "candy",
               values_to = "rating"
               ) %>% 
  mutate(candy = str_replace_all(candy, "_", " ")) %>% 
  filter(!is.na(rating))

candy_2016_other_items <- candy_2016_col_clean %>% 
  select(year, timestamp, age, gender,
         going_trick_or_treating,
         country, state_province_county_etc,
         joy, despair) %>% 
  others_to_candy("joy", "despair")

candy_2016_data <- bind_rows(candy_2016_main_list,
                             candy_2016_other_items)


# import of 2017 data set and deep clean of individual file
candy_2017_import <- import_xlsx_and_clean_names("boing-boing-candy-2017.xlsx")

candy_2017_col_clean <- candy_2017_import %>%
  rename_with(~str_remove(., "^q[1-6]_")) %>% 
  rename(going_trick_or_treating = going_out,
         joy = q7_joy_other,
         despair = q8_despair_other)

candy_2017_main_list <- candy_2017_col_clean %>%
  select(-joy, -despair) %>% 
  pivot_longer(cols = `100_grand_bar`:click_coordinates_x_y,
               names_to = "candy",
               values_to = "rating"
               ) %>% 
  mutate(candy = str_replace_all(candy, "_", " ")) %>% 
  filter(!is.na(rating))

candy_2017_other_items <- candy_2017_col_clean %>% 
  select(year, internal_id, age, gender,
         going_trick_or_treating,
         country, state_province_county_etc,
         joy, despair) %>%
  others_to_candy("joy", "despair")

candy_2017_data <- bind_rows(candy_2017_main_list,
                             candy_2017_other_items)


# combine all 3 data sets
all_candy_data_combined <- bind_rows(candy_2015_data,
                                      candy_2016_data,
                                      candy_2017_data)





# DEEP CLEAN

# remove "candy" listings (individual spelling) with less than 5 votes based
all_candy_data_ordered <- all_candy_data_unordered %>% 
  select(year, timestamp, age, gender,
         going_trick_or_treating,
         country, state_province_county_etc,
         everything()) %>%
  mutate(rating = str_to_lower(str_trim(rating)),
         candy = str_to_lower(str_trim(candy))
         ) %>% 
  filter(rating %in% c("joy", "despair", "meh")) %>%
  group_by(candy) %>% 
  filter(n() > 4) %>% 
  ungroup()

# filter list further to group candy with different spelling

# hundred_grand_bar <- c("100 grand bar", "x100 grand bar")
# fifth_avenue <- c("5th avenue", "5th avenue bar")
# aero <- c("aero, aero bars")
# almond_joy <- c("almond_joy, almond_joys")
# mary_jane <- c("anonymous brown globs that come in black and orange wrappers",
#                "anonymous brown globs that come in black and orange wrappers a k a mary janes",
#                "mary janes")
# babe_ruth <- c("babe ruth", "baby ruth", "baby ruth bar", "baby ruths")
# bit_o_honey <- c("bit o honey", "bit o' honey", "bit o'honey", "bit-o-honey")
# bonkers <- c("bonkers", "bonkers the candy")
# box_o_raisins <- c("box o raisins", "boxo raisins")
# candy_apple <- c("candy apple", "candy apples")
# caramel_apple <- c("caramel apple", "caramel apple pops", "caramel apples", "carmel apples")
# charleston_chew <- c("charleston chew", "charleston chews")
# clark_bar <- c("clark bar", "clark bars")
# cow_tales <- c("cow tails", "cow tales")
# dum_dums <- c("dum dums", "dum-dums")
# gummi_worms <- c("gummi worms", "gummy worms")
# jawbreakers <- c("jaw breakers", "jawbreakers")
# jelly_belly <- c("jelly bellies", "jelly belly")
# jolly_rancher <- c("jolly rancher bad flavor", "jolly ranchers good flavor")
# licorice <- c("licorice", "licorice not black","licorice yes black")
# mallo_cup <- c("mallo cups", "mallow cups")
# mike_and_ike <- c("mike", "mike & ike", "mike & ikes", "mike and ike")
# mounds_bar <- c("mounds", "mounds bar", "mounds bars")
# mr_goodbar <- c("goodbar", "mr", "mr goodbar")
# payday_bar <- c("pay day", "payday", "payday bar", "payday bars")
# peanut_butter_m&m <- c("peanut butter m&m's","peanut butter m&ms")
# raisinets <- c("raisinets", "raisinettes", "raisins")
# reeses_pieces <- c("reece's pieces", "reese's pieces", "reeses pieces")
# skor_bar <- c("skor", "skor bar", "skor bars")
# sky_bar <- c("sky bar", "skybar")
# sour_patch_kids <- c("sour patch kids", "sourpatch kids",
#                      "sourpatch kids i e abominations of nature")
# take_5_bar <- c("take 5", "take 5 bar", "take 5 bars", "take five", "take5")
# tootsie_pop <- c("tootsie pop", "tootsie pops")
# tootsie_roll <- c("tootsie roll", "tootsie roll pops", "tootsie rolls")
# zagnut_bars <- c("zagnut", "zagnut bars")
# zero_bar <- c("zero", "zero bar", "zero bars")



alternative_candy_names <- data.frame(
  hundred_grand_bar = cbind("100 grand bar", "x100 grand bar"),
  fifth_avenue = cbind("5th avenue", "5th avenue bar"),
  aero = cbind("aero", "aero bars"),
  almond_joy = cbind("almond_joy", "almond_joys"),
  mary_jane = cbind("anonymous brown globs that come in black and orange wrappers",
                 "anonymous brown globs that come in black and orange wrappers a k a mary janes",
                 "mary janes"),
  babe_ruth = cbind("babe ruth", "baby ruth", "baby ruth bar", "baby ruths"),
  bit_o_honey = cbind("bit o honey", "bit o' honey", "bit o'honey", "bit-o-honey"),
  bonkers = cbind("bonkers", "bonkers the candy"),
  box_o_raisins = cbind("box o raisins", "boxo raisins"),
  candy_apple = cbind("candy apple", "candy apples"),
  caramel_apple = cbind("caramel apple", "caramel apple pops", "caramel apples", "carmel apples"),
  charleston_chew = cbind("charleston chew", "charleston chews"),
  clark_bar = cbind("clark bar", "clark bars"),
  cow_tales = cbind("cow tails", "cow tales"),
  dum_dums = cbind("dum dums", "dum-dums"),
  gummi_worms = cbind("gummi worms", "gummy worms"),
  jawbreakers = cbind("jaw breakers", "jawbreakers"),
  jelly_belly = cbind("jelly bellies", "jelly belly"),
  jolly_rancher = cbind("jolly rancher bad flavor", "jolly ranchers good flavor"),
  licorice = cbind("licorice", "licorice not black","licorice yes black"),
  mallo_cup = cbind("mallo cups", "mallow cups"),
  mike_and_ike = cbind("mike", "mike & ike", "mike & ikes", "mike and ike"),
  mounds_bar = cbind("mounds", "mounds bar", "mounds bars"),
  mr_goodbar = cbind("goodbar", "mr", "mr goodbar"),
  payday_bar = cbind("pay day", "payday", "payday bar", "payday bars"),
  peanut_butter_m_m = cbind("peanut butter m&m's","peanut butter m&ms"),
  raisinets = cbind("raisinets", "raisinettes", "raisins"),
  reeses_pieces = cbind("reece's pieces", "reese's pieces", "reeses pieces"),
  skor_bar = cbind("skor", "skor bar", "skor bars"),
  sky_bar = cbind("sky bar", "skybar"),
  sour_patch_kids = cbind("sour patch kids", "sourpatch kids",
                       "sourpatch kids i e abominations of nature"),
  take_5_bar = cbind("take 5", "take 5 bar", "take 5 bars", "take five", "take5"),
  tootsie_pop = cbind("tootsie pop", "tootsie pops"),
  tootsie_roll = cbind("tootsie roll", "tootsie roll pops", "tootsie rolls"),
  zagnut_bar = cbind("zagnut", "zagnut bars"),
  zero_bar = cbind("zero", "zero bar", "zero bars")
)

alternative_candy_names_2 <- alternative_candy_names %>% 
  pivot_longer(cols = everything(),
               names_to = "replacement",
               values_to = "to_replace") %>% 
  mutate(replacement = str_remove(replacement, ".[0-9]$"))









all_candy_data_ordered %>% 
  group_by(candy) %>% 
  count() %>% 
  arrange() %>% 
  write_csv(here::here("clean_data/all_candy_clean.csv"))


all_candy_grouped <- all_candy_data_ordered %>% 
  rename(going_trick_or_treating = going_out,
         joy = q7_joy_other,
         despair = q8_despair_other)
 
# remove non-candy bars from the list

candy_only_data <- all_candy_grouped %>% 
  filter(candy %in% candy_bar_list)

# filter countries into Canada, UK, USA and others_unknown

canada <- c()
u_k <- c()
u_s_a <- c()

candy_only_and_country_data <- candy_only_data %>% 
  rename(going_trick_or_treating = going_out,
         joy = q7_joy_other,
         despair = q8_despair_other)


## check age limits, gender and trick_treating status


# export clean data
all_candy_final %>%
  write_csv(here::here("clean_data/candy_clean.csv"))
