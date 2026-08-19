#==============================================================================
# Day 4: RI Workshop
#==============================================================================


#==================================
# Practice Manipulating Data
#==================================

# Write your code below

library(tidyverse)





#==================================
# For loops
#==================================

# Structure of for loops (don't run this code it is just an template)
results <- container_for_results

for (variable in vector) {
  results[[variable]] <- function to perform(vector[variable])
}

# Simple loop
vec <- c(1,2,3,4,5,6)

## Create an empty vector to store the results
results <- c()

for(i in vec){
  results[i] <- 1+vec[i]
}

results

# Lists and Loops
results_l <- list()

for(i in vec){
  results_l[i] <- 1+vec[i]
}

results_l

# More on loops
results <- c()

results_n <- c()

for (i in vec) {
  results[i] <- 2+vec[i]
  results_n[i]<- results[i]/length(vec)*17
}

data.frame(results, results_n)

# Nested loops
results <- c()

for (i in vec) {
  for (j in vec) {
    answer <- i * j
    results <- c(results, answer)
  }
}

# Loops and dataframes
results <- data.frame(col1 = c(1,2,3,4),
                      col2 = c(5,6,7,8),
                      col3 = c(9,10,11,12))

for(i in 1:nrow(results)){
  for(j in 1:ncol(results)){
    results[i,j]<-results[i,j]+17/2
  }
}

results

#==================================
# Practice with Loops
#==================================

# Run this code
vec <- c(5,10,15,20,25,30)

# Write your loops below




#==================================
# Functions
#==================================

add_two <- function(x){
  x+2
}

add_two(2)

dat <- data.frame(vec = vec, pl_2 = add_two(vec))

dat

# Function that estimates mean and standard deviation
vec2 <- c(2, 5, 8, 3, 9, 10, 12, 1, 15, 12, 18, 21, 7)

mean_and_sd <- function(x) {
  vector_mean <- mean(x)
  vector_sd <- sd(x)
  results <- c(
    mean = vector_mean,
    standard_deviation = vector_sd
  )
  return(results)
}

mean_and_sd(vec2)


# Practice

# Run this code first
set.seed(2026)
## This command creates a vector of 100 random numbers
mins<-rnorm(100, mean= 120, sd=40)



