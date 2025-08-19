# Clearing environment
rm(list = ls())

# Attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) # help us work with dates

# Data wrangling refresher
# 1. only include penguins at Briscoe and Dream Islands
# 2. remove the year and sex variables
# 3. add a new column called body_mass_kg with penguin mass converted from grams to kg
# 4. rename the island variable to location
penguins %>%
  filter(island %in% c("Briscoe", "Dream")) %>%
  select(-year, -sex) %>%
  mutate("body_mass_kg" = body_mass_g / 1000) %>%
  rename(location = island)
  
# 1. Limit to only Adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, standard deviation, and sample size (n()) of flipper lengths for male and females 

penguins %>%
  filter(species == "Adelie") %>%
  filter(!is.na(flipper_length_mm),
         !is.na(sex)) %>% # remove rows that are not NA
  group_by(sex) %>%
  summarise(mean = mean(flipper_length_mm),
            standard_dev = sd(flipper_length_mm),
            sample_size = n())
# Practice with joins

animals <- data.frame(
  stringsAsFactors = FALSE,
           location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
  ) 


sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

# practice with full_join
# keeps all rows and adds all columns
full_join(animals, sites)

# left_join()
left_join(animals, sites)

# right_join()
right_join(animals, sites)

# inner_join()
inner_join(animals, sites)

# Filtering joins
semi_join(animals, sites)

animals %>%
  filter(location %in% sites$location)

anti_join(animals, sites)

animals %>%
  filter(!location %in% sites$location)

anti_join(sites, animals)

# Practice with lubridate
my_date <- "03-13-1998"
lubridate::mdy(my_date) # fixed date to ISO 8601

# new format for date
my_date <- "08/Jun/1974"
lubridate::dmy(my_date)

# another example of different format
my_date <- "19160518"
lubridate::ymd(my_date)

# what happens if we give lubridate a date that doesn't make sense?
lubridate::mdy("1942-08-30")

lubridate::dmy("09/12/84")

# working with date-times

time <- "2020-08-12 11:18"
time <- ymd_hm(time, tz = "America/Los_Angeles")

# convert to PDT
with_tz(time, "America/Los_Angeles")

# extract info from dates
week(time)
year(time)
day(time)

start_time <- Sys.time()

end_time <- Sys.time()

end_time - start_time
