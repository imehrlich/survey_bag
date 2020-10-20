## The following code is meant to clean the GSS Data collected in 1989


# Uncomment if packages are not installed
#install.packages("janitor")
#install.packages("tidyverse")

# Load packages
library(janitor)
library(tidyverse)

# Load data
data_raw <- read.csv("data/AA80KdAF.csv")
dict_raw <- read_lines("data/AAwKymfk.txt", skip=68)[2:285]
labels_raw <- read_file("data/AAo4MRCo.txt")

# Format variable descriptions
variable_descriptions <- as_tibble(dict_raw) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(data_raw)[-1]))

# Format variable information
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# Function for fomatting data responses
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# Format data responses
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)

cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

# Select relevant columns from the data set
gss <- data_raw %>% 
  select(a4,
         a5aa7a,
         a5ba7b,
         a5ca7c,
         a5da7d,
         a6a8,
         a10,
         a11,
         dveda15,
         c2,
         c3,
         c12,
         dvsicc9,
         dvsicd11,
         dvsick4,
         dvc10soc,
         dvd12soc,
         dvk5soc,
         e8,
         e9,
         e10a,
         e10b,
         e10c,
         e10d,
         e10e,
         e10f,
         e10g,
         e11,
         f1a,
         f1b,
         f1c,
         f1d,
         f4a,
         dvf5a,
         dvf5b,
         dvf5c,
         dvf5d,
         f6a,
         f6b,
         f6c,
         f6d,
         f6e,
         f6f,
         f6g,
         dvdifjb,
         dvh11,
         dvh29a,
         dvh29b,
         dvh29c,
         dvh29d,
         dvh29e,
         dvh29f,
         h30a,
         h30b,
         h30c,
         h33,
         h35,
         h36,
         h37,
         h38,
         h42,
         h43,
         k6,
         dvk2ret,
         k8a,
         k8b,
         k8c,
         k8d,
         k9,
         k11,
         agertre,
         manrtre,
         dvmage,
         l6a,
         l6b,
         dvn3a,
         dvn3b,
         dvn3c,
         p22,
         weigt,
         prov,
         dvms,
         dvsex,
         dvagegr,
         dvlvgar1,
         dvhhldsz,
         dvchild,
         dvh34,
         dvsalp27) %>% 
  mutate_at(.vars = vars(a4:dvchild),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))

# Format and rename relevant columns
gss <- gss %>% 
  clean_names() %>% 
  rename(hs_complete = a4,##
         hs_math = a5aa7a,
         hs_chem = a5ba7b,
         hs_geo = a5ca7c,
         hs_physics = a5da7d,
         secondary_educ = a6a8, ##
         secondary_sci = a10,
         highest_educ = a11, ##
         degree_field = dveda15,
         activity = c2,
         ever_employed = c3,
         work_relationship = c12,
         work_industry_after = dvsicc9,
         work_industry_before = dvsicd11,
         industry_when_retired = dvsick4,
         work_occupation_after = dvc10soc,
         work_occupation_before = dvd12soc,
         occupation_when_retired = dvk5soc,
         taken_comp_classes = e8,
         comp_ability = e9,
         computer_games = e10a,
         computer_processing = e10b,
         computer_entry = e10c,
         computer_records = e10d,
         computer_analysis = e10e,
         computer_programs = e10f,
         computer_use = e10g,
         personal_computer = e11,
         interest_affairs = f1a,
         interest_econ = f1b,
         interest_tech = f1c,
         interest_sci = f1d,
         read_tech = f4a,
         science_better = dvf5a,
         science_interesting = dvf5b,
         computers_add_jobs = dvf5c,
         science_fast = dvf5d,
         govt_health = f6a,
         govt_elderly = f6b,
         govt_educ = f6c,
         govt_unemployed = f6d,
         govt_research = f6e,
         govt_poor = f6f,
         govt_pollution = f6g,
         num_jobs = dvdifjb,
         job_satisfaction = dvh11,
         work_pleasant = dvh29a,
         work_freedom = dvh29b,
         work_repetitive = dvh29c,
         work_skill = dvh29d,
         work_pay = dvh29e,
         work_promotion = dvh29f,
         work_pension = h30a,
         work_medical = h30b,
         work_dental = h30c,
         work_computer = h33,
         computer_impact = h35,
         computer_skills = h36,
         computer_security = h37,
         computer_interest = h38,
         job_loss = h42,
         computer_unemployment = h43,
         retired_education = k6,
         retired_age = dvk2ret,
         retired_incentive = k8a,
         retired_tech = k8b,
         retired_health = k8c,
         retired_other = k8d,
         retired_pension = k9,
         retired_enjoy = k11,
         plan_retire = agertre,
         mand_retire = manrtre,
         mand_retire_desire = dvmage,
         jobless_unavailable = l6a,
         jobless_skills = l6b,
         satisfaction_health = dvn3a,
         satisfaction_education = dvn3b,
         satisfaction_job = dvn3c,
         spouse_educ = p22,
         income = dvsalp27,
         province = prov,
         married = dvms,
         sex = dvsex,
         age_group = dvagegr,
         living = dvlvgar1,
         household_size = dvhhldsz,
         num_children = dvchild,
         computer_hours = dvh34) 


## The following section cleans the data specifically for a project on modeling computer-based fear

# Remove respondents aged 15-19 since these are mostly students
gss <- gss[gss$age_group != "15-19 years",]

# Select another set of more relevant columns to be used
gss_select <- as.tibble(cbind(gss$hs_complete, gss$taken_comp_classes,
                              gss$work_computer, gss$computer_hours, gss$income,
                              gss$satisfaction_job, gss$computer_unemployment,
                              gss$computers_add_jobs, gss$science_better))
colnames(gss_select) <- c("hs_complete", "taken_comp_classes", "work_computer",
                         "computer_hours", "income", "job_satisfaction",
                         "computer_unemployment", "computers_add_jobs",
                         "science_better") 

# Clean answers for the question of computers at work
gss_select$work_computer[is.na(gss_select$work_computer)] <- "No"
gss_select$work_computer[gss_select$work_computer != "Yes"] <- "No"

# Clean answers for the question of completed computer classes
gss_select$taken_comp_classes[gss_select$taken_comp_classes != "Yes"] <- "No"

# Clean answers for the question of computer hours at work
gss_select$computer_hours[gss_select$computer_hours == 97 | 
                            gss_select$computer_hours == 99] <- 0
gss_select$computer_hours <- as.numeric(gss_select$computer_hours)

# Clean answers for the question of income
gss_select$income[gss_select$income > 99997] <- NA
gss_select$income <- as.numeric(gss_select$income)

# Clean answers for the question of science and computer effects
gss_select$science_better[gss_select$science_better == "Agree/degree not stated" | 
                            gss_select$science_better == "Not stated"] <- NA
gss_select$computers_add_jobs[gss_select$computers_add_jobs == "Disagree/degree not stated" | 
                                gss_select$computers_add_jobs == "Not stated"] <- NA
gss_select$job_satisfaction[gss_select$job_satisfaction == "Satisfied/not stated" | 
                              gss_select$job_satisfaction == "Not stated"] <- NA

# Create satisfaction binomial variable
gss_select$satisfaction_binom <- rep(NA, nrow(gss_select))
gss_select$satisfaction_binom[gss_select$job_satisfaction == "Somewhat satisfied" | 
                                gss_select$job_satisfaction == "Strongly satisfied"] <- 1
gss_select$satisfaction_binom[gss_select$job_satisfaction == "Somewhat dissatisfied" | 
                                gss_select$job_satisfaction == "Strongly dissatisfied"] <- 0

# Final clean of GSS data
gss_select[gss_select=="Not applicable"] <- NA
gss_select <- drop_na(gss_select, c(hs_complete, income))

# Export into csv
write.csv(gss_model, "data/gss1989_clean.csv")

