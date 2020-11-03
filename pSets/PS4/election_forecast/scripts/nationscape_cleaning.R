#### Preamble ####
# Purpose: Prepare and clean survey data downloaded from Democracy Fund
# Author: Isaac Ehrlich
# Data: 02 November 2020
# Contact: isaac.ehrlich@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the Democracy Fund data and saved it to the data folder


#### Workspace Setup ####
library(haven)
library(tidyverse)
library(dplyr)
# Read in raw data
df_raw <- read_dta("data/ns20200625.dta")

# Add Labels
df_raw <- labelled::to_factor(df_raw)

# Just keep some variables
df_data <- 
  df_raw %>% 
  select(right_track,
         economy_better,
         interest,
         registration,
         pres_approval,
         vote_2016,
         consider_trump,
         not_trump,
         trump_biden,
         primary_party,
         vote_intention,
         vote_2020,
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         age)

rm(df_raw)


#### Data Cleaning ####

# Change planned vote to show 0 for Donald Trump and 1 for Joe Biden
df_data <- df_data %>% mutate(vote_2020 = case_when(vote_2020 == "Donald Trump" ~ 0,
                                              vote_2020 == "Joe Biden" ~ 1))
df_data <- df_data[!is.na(df_data$vote_2020),]


# Make age groups, based on generational breaks provided by Gallup
df_data <- df_data %>% mutate(age = case_when(age >= 18  & age < 35 ~ '18-34',
                                             age >= 35  & age < 52 ~ '35-51',
                                             age >= 52  & age < 68 ~ '52-67',
                                             age>=68 ~ '68+'))

# Combine race to match IPUMS Data
df_data <- df_data %>% mutate(race_ethnicity = case_when(race_ethnicity == "White" ~ 'White',
                                                     race_ethnicity == "American Indian or Alaska Native" ~ 'Other',  
                                                     race_ethnicity == "Black, or African American" ~ 'Black',
                                                     race_ethnicity == "Asian (Asian Indian)" ~ 'Asian',
                                                     race_ethnicity == "Asian (Chinese)" ~ 'Asian',
                                                     race_ethnicity == "Asian (Filipino)" ~ 'Asian',
                                                     race_ethnicity == "Asian (Japanese)" ~ 'Asian',
                                                     race_ethnicity == "Asian (Korean)" ~ 'Asian',
                                                     race_ethnicity == "Asian (Vietnamese)" ~ 'Asian',
                                                     race_ethnicity == "Asian (Other)" ~ 'Asian',
                                                     race_ethnicity == "Pacific Islander (Native Hawaiian)" ~ 'Asian',
                                                     race_ethnicity == "Pacific Islander, (Guamanian)" ~ 'Asian',
                                                     race_ethnicity == "Pacific Islander (Samoan)" ~ 'Asian',
                                                     race_ethnicity == "Pacific Islander (Other)" ~ 'Asian',
                                                     race_ethnicity == "Some other race" ~ 'Other'))

# Combine income classes
df_data <- df_data %>% mutate(household_income = case_when(household_income == "Less than $14,999" ~ 'Less than $50,000',
                                                         household_income == "$15,000 to $19,999" ~ 'Less than $50,000',
                                                         household_income == "$20,000 to $24,999" ~ 'Less than $50,000',
                                                         household_income == "$25,000 to $29,999" ~ 'Less than $50,000',
                                                         household_income == "$30,000 to $34,999" ~ 'Less than $50,000',
                                                         household_income == "$35,000 to $39,999" ~ 'Less than $50,000',
                                                         household_income == "$40,000 to $44,999" ~ 'Less than $50,000',
                                                         household_income == "$45,000 to $49,999" ~ 'Less than $50,000',
                                                         household_income == "$50,000 to $54,999" ~ '$50,000 to $99,999',
                                                         household_income == "$55,000 to $59,999" ~ '$50,000 to $99,999',
                                                         household_income == "$60,000 to $64,999" ~ '$50,000 to $99,999',
                                                         household_income == "$65,000 to $69,999" ~ '$50,000 to $99,999',
                                                         household_income == "$70,000 to $74,999" ~ '$50,000 to $99,999',
                                                         household_income == "$75,000 to $79,999" ~ '$50,000 to $99,999',
                                                         household_income == "$80,000 to $84,999" ~ '$50,000 to $99,999',
                                                         household_income == "$85,000 to $89,999" ~ '$50,000 to $99,999',
                                                         household_income == "$90,000 to $94,999" ~ '$50,000 to $99,999',
                                                         household_income == "$95,000 to $99,999" ~ '$50,000 to $99,999',
                                                         household_income == "$100,000 to $124,999" ~ '$100,000 to $249,999',
                                                         household_income == "$125,000 to $149,999" ~ '$100,000 to $249,999',
                                                         household_income == "$150,000 to $174,999" ~ '$100,000 to $249,999',
                                                         household_income == "$175,000 to $199,999" ~ '$100,000 to $249,999',
                                                         household_income == "$200,000 to $224,999" ~ '$100,000 to $249,999',
                                                         household_income == "$225,000 to $249,999" ~ '$100,000 to $249,999',
                                                         household_income == "$250,000 and above" ~ 'More than $250,000'))

df_data <- df_data[!is.na(df_data$race_ethnicity),]

# Export data
write.csv(df_data, "data/df_data_clean.csv")
