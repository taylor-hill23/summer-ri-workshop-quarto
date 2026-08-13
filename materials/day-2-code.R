#==============================================================================
# Day 2: RI Workshop
#==============================================================================

data<-read_csv("data/live_coding_1.csv")
#================================
# Cleaning Data
#================================

# Run this code
library(tidyverse)
library(janitor)

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

congress_clean<- real_congress |> 
  rename(
    "name"="Full Name",
    "pol_aff"="Political Affiliation",
    "state"="Represented State", 
    "age"="Politician Age",
    "yrs_served"="Years Served",
    "votes"="Votes Received", 
    "leg_passed"="Legislation Passed"
  ) |> 
  mutate(pol_aff=factor(pol_aff, 
                        levels=c("Democratic" ,"Republican", "Libertarian"))) |> 
  filter(!is.na(yrs_served))

congress_clean2<- real_congress |> 
  clean_names()

#================================
# Practice with Live Coding Data
#================================

# Type your code below

live_coding<-read_csv("data/live_coding_1.csv")


live_coding2<-live_coding |>
  rename(
    "name"="Full Name",
    "pid"= "Political Affiliation",
    "state"= "Represented State",
    "age"= "Politician Age",
    "served"="Years Served",
    "votes"= "Votes Received",
    "legislation"= "Legislation Passed",
    "speeches"= "Speeches Given",
    "sponsor"= "Bills Sponsored",
    "committee"= "Committees Joined"
  ) |>
  mutate(age_cat= ifelse(age>=60, "60+", "<60"),
         experience= ifelse(served >= 12, "Experienced", "Amatuer"),
         active= ifelse(sponsor< 9 & committee < 6 & votes <= 30000, 
                        "Inactive", "Active")) |>
  select("name", "pid", "served", "sponsor", 'age_cat', "experience", "active")


live_coding2<-na.omit(live_coding2)


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
real_congress2 |> 
  group_by(age_cat) |> 
  summarise(mean_votes=mean(votes_received),
            mean_served=mean(years_served)) 

real_congress2 |> 
  group_by(party) |> 
  summarise(mean_votes=mean(votes_received))

real_congress2<-real_congress2 |> 
  mutate(region=ifelse(state=="New York"|state=="Florida", "East", "West")) |> 
  group_by(region) |> 
  summarise(count_bills=sum(legislation_passed))

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

# Practice

nrow(live_coding2)

ncol(live_coding2)

summary(live_coding2)

table(live_coding2$party)

live_coding2 |> 
  group_by(age_cat) |> 
  summarise(ave_bills=mean(sponsor))


#================================
# More on Vectors
#================================

# To make your code reproducible, always set seed
## You can use any number you want
set.seed(26)
rnorm(5)

# Running this without setting seed gives you different numbers
rnorm(5)

# Normal distribution
set.seed(26)
normal<-rnorm(100)
normal

# Uniform distribution
uniform<-runif(100)
uniform

# Binomial distribution
binomial<- rbinom(100, 1, prob=0.5)
binomial


# Normal distribution - change mean and standard deviation
set.seed(26)
normal<-rnorm(100, mean=75, sd=10)
normal

# Uniform distribution- change minimum and maximum
uniform<-runif(100)
uniform

# Binomial distribution- change the size (uncommon) and probability
binomial<- rbinom(100, 1, prob=0.25)
binomial

# Practice (Type your code below)
n=10000

set.seed(26)
age<-rnorm(n, mean=40, sd=15)
vote_margin<-runif(n)
gender<-rbinom(n, 1, prob=0.5)

vec_data<-tibble(age, vote_margin, gender)
mean(age)


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
