#==============================================================================
# Day 1: RI Workshop
#==============================================================================

#================================
# R as a Calculator
#================================
1+1

1+3

3*4

10000000000/1000000

123456789*987654

sqrt(121)

# Practice



#================================
# Objects
#================================

b <- 2
b+2

sq<- sqrt(500)
sq500

a<-sqrt(200)
b<-log(2)
c<- 35^3
a*b-c

# Practice


#================================
# Strings
#================================

# Words are represented as character strings.
"Hello World"

# Numbers can also be strings with quotation marks
num_string<-"5"
num_string

# Strings cannot be treated as numbers
"Hello World" + 2
"3" + 2

# Coverting to numbers
as.numeric("3") + 2

install.packages("tidyverse")
library(tidyverse)
parse_number("3") + 2

num_string<-"5"
class(num_string)
num<-5
class(num)

# Difference between parse_number and as.numeric
parse_number("himothy316") + 2
as.numeric("himothy316") + 2
parse_number("ideo2")

#================================
# Vectors
#================================

# Numerical vectors
a <- c(5, 10, 15, 100)
a
a / 5
mean(a)

# Character vectors
p <- c("Republican", "Democrat", "Republican", "Independent")
p

p * 3

cat("Repu\nblican")
cat("Demo\tcrat")

# Factor vectors

p1 <- c("Republican", "Republican", "Democrat", "Other")
class(p1)
table(p1)

pid <- factor(p1,levels = c("Republican", "Independent", "Democrat", "Other"))
pid

Practice


#================================
# Vectors
#================================

x <- c(1, 4, 7, NA, 2)
log(x)

sum(x)
sum(x, na.rm = TRUE)


#================================
# Data Frames and Tibbles
#================================
# Data frame is base R data, tibbles are tidyverse data. They work the same.

# Manually create a data frame
data_name<-data.frame(name = c("John", "Jacob", "Jingleheimer Schmidt"),
           ideo = c(1, 4, 7), sex  = c("Male", "Male", "Male"))

# Manually create a tibble (requires tidyverse)
library(tidyverse)
tibble_name<-tibble(name = c("John", "Jacob", "Jingleheimer Schmidt"),
       ideo = c(1, 4, 7), sex  = c("Male", "Male", "Male"))

# Calling a column of the data
tibble_name$name
tibble_name$ideo / 2

# Calling one value in a vector
tibble_name$name[2]

# Combining vectors into a tibble
name <- c("John", "Jacob", "Jingleheimer Schmidt")
ideo <- c(1, 4, 7)
sex  <- c("Male", "Male", "Male")

tibble(name, ideo, sex)

# Changing data with mutate
tibble_name |>
  mutate(ideo_cat = c("Very Conservative", "Moderate", "Very Liberal"))

# Base R equivalent
data_name$ideo_cat <- c("Very Conservative","Moderate","Very Liberal")
data_name

# mutate and case_when
tibble_name2<- tibble_name |>
  mutate(ideo_cat = case_when(
    ideo == 1 ~ "Very Conservative",
    ideo == 4 ~ "Moderate",
    ideo == 7 ~ "Very Liberal"))

tibble_name2

# filter
tibble_name2 |>
  filter(ideo_cat == "Very Liberal")

tibble_name2 |>
  filter(ideo_cat != "Very Liberal")

#================================
# Renaming Variables in a Dataset
#================================
data<-tibble(X1 = c(123, 124, 125, 126),
       X2 = c(1, 2, 4, 7),
       X3 = c("abortion", "health care", "guns", "police"))

data |>
  rename(id = X1,
         ideo  = X2,
         issue = X3)


data2<- tibble("ID Number" = c(123, 124, 125, 126),
       "Ideology Numeric" = c(1, 2, 4, 7),
       "Most Important Issue" = c("abortion", "health care", "guns", "police"))

install.packages('janitor')
library(janitor)
data2<-data2 |>
  janitor::clean_names()

data2

# select
data2 %>% select(most_important_issue)
data2 %>% select(3)

data3<-tibble(
  sex=c("male", "female", "female"),
  race=c("black", "white", "black"),
  pid= c("Strong Republican", "Strong Democrat", "Lean Democrat"),
  ideo= c("Very conservative", "Very liberal", "Somewhat liberal"),
  feeling_dems= c(5, 90, 65)
)

clean_data3<-data3 |>
  select(c(sex, pid, feeling_dems))
clean_data3

# slice to cut out one variable
country<- tibble(country = c("country", "USA", "China", "Germany"),
       wars    = c("wars", 2, 4, 5),pres    = c("pres", 1, 0, 1),
       par     = c("par", "Congress", "None", "Parliament"))

country<- country |>
  slice(-1)
country |>
  select(-wars)

country<- country |>
  mutate(wars=parse_number(wars),
         pres=parse_number(pres))
country

# Practice

#================================
# Reading in Data
#================================

library(tidyverse)
# Base R
anes <- read.csv("anes.csv")
# tidyverse
anes <- read_csv("anes.csv")

# Setting working directory (Put in the pathname of your project)
setwd("/Users/taylorhill/Documents/Research /Mattt-Political Discussion/data")

# You can always just put in the full path name as well
data<-read_csv('/Users/taylorhill/Documents/Research /Mattt-Political Discussion/data/data_clean.csv')

#================================
# Olympic Data Practice
#================================

data |>
  rowwise() |>
  mutate()

library(tidyverse)
# 1. Read the data.
olympics<- read.csv("olympics.csv")
read_csv("olympics.csv")

# 2. Keep only the variables we want.
olympics<- olympics |>
  select(X0, X1, X6)

# 3. Rename the variables.
olympics <- olympics |>
  rename(country = X0, summer  = X1, winter  = X6)

# 4. Create the total-medals variable.
olympics<- olympics |>
  slice(-1) |>
  rowwise() |>
  mutate(total = sum(parse_number(summer), parse_number(winter)))

# 5. Keep only Germany.
olympics |>
  filter(country == "Germany")

# 6. Remove Chile.
olympics |>
  filter(country != "Chile")

