### Data Cleaning for eLife Survey on Mental Health Support in Academia ###
# Author: Isaac Ehrlich
# Date: 22 December 2020
# Contact: isaac.ehrlich@mail.utoronto.ca
# Prerequisites: Need to have downloaded the eLife data and saved it to the data folder
# Original data and labels can be found at https://www.repository.cam.ac.uk/handle/1810/312796


## Set-Up and Read Data
library(tidyverse)
data <- read.csv("data/data.csv")

## Select Data
data <- data %>% select(Provided_Support,
                        Not_ProvidedSupport,
                        Supporter_CareerStage,
                        Supporter_Officially,
                        Supporter_MH,
                        Receiver_CareerStage,
                        Receiver_MH_Answered_Sum,
                        Confident_withMH,
                        Comfortable_withMH)

# Reclassify to binary Yes/No responses
data <- filter(data, Provided_Support < 5, Not_ProvidedSupport < 5) %>%
  mutate(Provided_Support = case_when(Provided_Support == 1  ~ 'No',
                         Provided_Support != 1 ~ 'Yes'),
         Not_ProvidedSupport = case_when(Not_ProvidedSupport == 1  ~ '0',
                                         Not_ProvidedSupport != 1 ~ '1'),
         Supporter_Officially = case_when(Supporter_Officially == 1 ~ '0',
                                          Supporter_Officially == 2 ~ '1'),
         Supporter_MH = case_when(Supporter_MH== 1 ~ '0',
                                          Supporter_MH == 2 ~ '1'))

# Reclassify to student/faculty responses
# 0 represents Students and 1 represents Staff
data <- data %>% mutate(Supporter_CareerStage_Binary = case_when(Supporter_CareerStage %in% 1:3 ~ 0,
                                                          Supporter_CareerStage > 3 ~ 1),
                        Supporter_CareerStage_Split = case_when(Supporter_CareerStage %in% 1:3 ~ "Student",
                                                                 Supporter_CareerStage > 3 ~ "Staff"),
                        Supporter_CareerStage_Simple = case_when(Supporter_CareerStage == 0 ~ "Other",
                                                                 Supporter_CareerStage == 1 ~ "Undergraduate",
                                                                 Supporter_CareerStage == 2 ~ "Masters Student",
                                                                 Supporter_CareerStage == 3 ~ "PhD Student",
                                                                 Supporter_CareerStage %in% c(4:8,12,13) ~ "Academic Staff",
                                                                 Supporter_CareerStage %in% c(9,10) ~ "Non-Academic Staff"),
                        Receiver_CareerStage = case_when(Receiver_CareerStage == 0 ~ "Other",
                                                         Receiver_CareerStage %in% 1:3 ~ "Student",
                                                         Receiver_CareerStage > 3 ~ "Staff"))

data <- select(data, -Supporter_CareerStage)

# Export Clean Data
write.csv(data, "data/data_clean.csv")
