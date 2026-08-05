#==============================================================================
# Day 2: RI Workshop
#==============================================================================

#================================
# Cleaning Data
#================================

# Run this code
library(tidyverse)

real_congress<- tibble("Full Name" = c("John Smith", "Jimmy Dean", "Robert Williams",
                       "Emily Davis", "Michael Brown"),
       "Political Affiliation" = c("Democratic", "Republican", NA, "Democratic",
                                   "Libertarian"),
       "Represented State" = c("California", "Texas", "New York", NA, "Florida"),
       "Politician Age" = c(45, 65, 60, 41, 20),
       "Years Served" = c(6, NA, 2, 4, 12),
       "Votes Received" = c(24000, NA, 15000, 20000, 32000),
       "Legislation Passed" = c(12, 10, NA, 6, 15))

real_congress

# Practice (write your code below)

real_congress2<-real_congress %>%
  janitor::clean_names() %>%
  rename('party' = political_affiliation,
         'state' = represented_state,
         'age' = politician_age) %>%
  filter(!is.na(party)) %>%
  mutate(age_cat = case_when(age < 30 ~ "<30",
                             (age >= 30 & age < 60) ~ "30-60",
                             age >= 60 ~ "60+")) %>%
  select(full_name,party,state,age_cat)

real_congress2

#================================
# Aggregating Data
#================================
real_congress<- real_congress %>%
  janitor::clean_names() %>%
  rename('party' = political_affiliation,
         'state' = represented_state,
         'age' = politician_age) %>%
  filter(!is.na(party))

real_congress

# Number of members in each party
real_congress |>
  group_by(party) |>
  summarize(sample = n())

# Average bills by party
real_congress |>
  group_by(party) |>
  summarise(avg_passed=mean(legislation_passed))

# Creating age category and number in each category
real_congress %>%
  mutate(age_cat = case_when(age < 30 ~ "<30",
                             (age >= 30 & age < 60) ~ "30-60",
                             age >= 60 ~ "60+")) %>%
  group_by(age_cat) %>%
  summarize(n())

# Legislation passed by age category
real_congress %>%
  mutate(age_cat = case_when(age < 30 ~ "<30",
                             (age >= 30 & age < 60) ~ "30-60",
                             age >= 60 ~ "60+")) %>%
  group_by(age_cat) %>%
  summarize(leg_pro = mean(legislation_passed))

# Reorder the age category with factor
real_congress %>%
  mutate(age_cat = case_when(age < 30 ~ "<30",
                             (age >= 30 & age < 60) ~ "30-60",
                             age >= 60 ~ "60+"),
         age_cat = factor(age_cat,
                          levels = c('<30', '30-60', '60+'))) %>%
  group_by(age_cat) %>%
  summarize(leg_pro = mean(legislation_passed))

# Practice Aggregating data

## Run this code and then complete the practice problems
real_congress2<- tibble("Full Name" = c("John Smith", "Jimmy Dean", "Robert Williams",
                                       "Emily Davis", "Michael Brown"),
                       "Political Affiliation" = c("Democratic", "Republican", "Republican", "Democratic",
                                                   "Libertarian"),
                       "Represented State" = c("California", "Texas", "New York", "Florida", "Florida"),
                       "Politician Age" = c(45, 65, 60, 41, 20),
                       "Years Served" = c(6, 8, 2, 4, 12),
                       "Votes Received" = c(24000, 30000, 15000, 20000, 32000),
                       "Legislation Passed" = c(12, 10, 21, 6, 15))

real_congress2<-real_congress2 %>%
  janitor::clean_names() %>%
  rename('party' = political_affiliation,
         'state' = represented_state,
         'age' = politician_age) %>%
  filter(!is.na(party)) %>%
  mutate(age_cat = case_when(age < 30 ~ "<30",
                             (age >= 30 & age < 60) ~ "30-60",
                             age >= 60 ~ "60+"))

real_congress2

## Type your code below


#================================
# Data Structure
#================================

real_congress %>%
  summary()

names(real_congress)

nrow(real_congress)
ncol(real_congress)

summary(real_congress$age)

table(real_congress$party)

length(real_congress$party)

#================================
# Practice with Live Coding Data
#================================

# Type your code below


#================================
# Lists
#================================

nums <- list(c(1,2,3,4,5), c(6,7,8,9,10))
nums

nums[[1]]

data<-bind_cols(nums)

nums[[3]] <- c(11,12,13,14,15)

nums

a_list <- list(tibble(x = c(1,2,3,4), y = c(1,1,1,1)),
               tibble(x = c(100,100,100,2), y = c(2,2,2,2)))

a_list

bind_rows(a_list)

bind_cols(a_list)
