#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA 2018 5-years ACS. 
# Author: Hanrui Dou, Hanjing Huang, Hairuo Wang, Xuan Zhong - Group 161
# Data: 2 Nomverber 2020
# Contact: hairuo.wang@mail.utoronto.ca, dhr1142638924@gmail.com, hanjing.huang@mail.utoronto.ca, xuan.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("~/Desktop/STA304")
raw_data <- read_dta("usa_00005.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Keep some variables that we are going to use to build model and predict. 
# Arrange detailed region to general region classification. 
reduced_data <- 
  raw_data %>% 
  select(region,
         sex, 
         age, 
         labforce, 
         empstat) %>% 
  mutate(region_new = case_when(`region` == 'new england division' ~ 'northeast', 
                                `region` == 'middle atlantic division' ~ 'northeast', 
                                `region` == 'east north central div' ~ 'midwest', 
                                `region` == 'west north central div' ~ 'midwest', 
                                `region` == 'south atlantic division' ~ 'south', 
                                `region` == 'east south central div' ~ 'south', 
                                `region` == 'west south central div' ~ 'south', 
                                `region` == 'mountain division' ~ 'west', 
                                `region` == 'pacific division' ~ 'west'))
         

#### What's next? ####

# We are going to splitting cells by age, sex, region and employment status 

reduced_data <- 
  reduced_data %>%
  count(age, sex, region_new, empstat) %>%
  group_by(age, sex, region_new, empstat) 

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>% 
  filter(empstat != 'n/a')

reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         