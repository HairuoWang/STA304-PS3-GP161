#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from Democracy Fund + UCLA Nationscape
# Author: Hanrui Dou, Hanjing Huang, Hairuo Wang, Xuan Zhong - Group 161
# Data: 2 November 2020
# Contact: hairuo.wang@mail.utoronto.ca, dhr1142638924@gmail.com, hanjing.huang@mail.utoronto.ca, xuan.zhong@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from Democracy Fund + UCLA Nationscape and save the folder that we're 
# interested in to inputs/data 


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("~/Desktop/STA304")
# Read in the raw data. 
raw_data <- read_dta("ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(interest,
         registration,
         vote_2016,
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
         congress_district,
         age)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?

# Make vote trump as binary variable. 
# Make detailed employment status to general employment status, which have 3 classification. 
# employed, unemployed and not in labor force

reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0)) %>% 
  filter(vote_trump != 'NA') %>% 
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0)) %>%
  filter(vote_biden != 'NA') %>% 
  mutate(empstat = 
           case_when(`employment` == 'Full-time employed' ~ 'employed', 
                     `employment` == 'Part-time employed' ~ 'employed',
                     `employment` == 'Self-employed' ~ 'employed',
                     `employment` == 'Unemployed or temporarily on layoff' ~ 'unemployed', 
                     `employment` == 'Retired' ~ 'not in labor force', 
                     `employment` == 'Student' ~ 'not in labor force', 
                     `employment` == 'Homemaker' ~ 'not in labor force', 
                     `employment` == 'Permanently disabled' ~ 'not in labor force'))
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data.csv")

