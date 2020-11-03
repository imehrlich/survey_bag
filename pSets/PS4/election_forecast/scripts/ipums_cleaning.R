#### Preamble ####
# Purpose: Prepare and clean survey data downloaded from IPUMS
# Author: Isaac Ehrlich
# Data: 02 November 2020
# Contact: isaac.ehrlich@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the IPUMS data and saved it to the data folder


#### Workspace Setup ####
library(haven)
library(tidyverse)
library(labelled)
# Read in raw data
ipums_raw <- read_dta("data/usa_00001.dta")

# Add Labels
ipums_raw <- labelled::to_factor(ipums_raw)

# Keep Variables of Interest 
names(ipums_raw)

ipums_data <- 
  ipums_raw %>% 
  select(region,
         stateicp,
         density, 
         metro,
         sex,
         age,
         marst,
         race, 
         hispan,
         bpl,
         citizen,
         educd,
         labforce,
         inctot)
rm(ipums_raw)
         

#### Data Cleaning ####
# Make age groups, based on generational breaks provided by Gallup
ipums_data$age <- as.numeric(ipums_data$age)
ipums_data <- filter(ipums_data, age >= 18)
ipums_data <- ipums_data %>% mutate(age = case_when(age >= 18  & age < 35 ~ '18-34',
                                              age >= 35  & age < 52 ~ '35-51',
                                              age >= 52  & age < 68 ~ '52-67',
                                              age>=68 ~ '68+'))

# Make birthplace binary (in US/not in US)
ipums_data <- ipums_data %>% mutate(bpl = case_when(bpl %in% tolower(state.name) ~ 'The United States',
                                                    !(bpl %in% tolower(state.name)) ~ 'Another country'))

# Combine race to match Democracy Fund Data
ipums_data <- ipums_data %>% mutate(race = case_when(race == "white" ~ 'White',
                                                     race == "black/african american/negro" ~ 'Black',
                                                     race == "american indian or alaska native" ~ 'Other',
                                                     race == "chinese" ~ 'Asian',
                                                     race == "japanese" ~ 'Asian',
                                                     race == "other asian or pacific islander" ~ 'Asian',
                                                     race == "other race, nec" ~ 'Other',
                                                     race == "two major races" ~ 'Other',
                                                     race == "three or more major races" ~ 'Other'))

# Match gender to Democracy Fund Data
ipums_data <- ipums_data %>% mutate(sex = case_when(sex == "female" ~ 'Female',
                                                    sex == "male" ~ 'Male'))

# Match income to Democracy Fund Data
ipums_data <- ipums_data %>% mutate(inctot = case_when(inctot < 50000 ~ 'Less than $50,000',
                                                       inctot >= 50000  & inctot < 100000 ~ '$50,000 to $99,999',
                                                       inctot >= 100000  & inctot < 250000 ~ '$100,000 to $249,999',
                                                       inctot >= 250000 ~ "More than $250,000"))
         
# Export Data
write.csv(ipums_data, "data/ipums_data_clean.csv")
