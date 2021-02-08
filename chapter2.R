############################
############################
##### Differential privacy
############################
############################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("dplyr", "smoothmest")
ipak(packages)

# Why differential privacy
# Quantifies privacy loss via a privacy budget
# Assumes worst-case scenario, no assumptions about the data intruder

# Epsilon, the privacy budget
# e.g., someone asks if I have cancer, set epsilon small, leak less information
# someone asks if I love dogs, set epsilon large, leak more information

# differential privacy: general concept
# Data with me 
# Data without me
# question: how many people are from Idaho?
# data with me sends 1
# data without me sends 0
# if i didn't want anyone to know i'm from Salmon, Idaho
# i will set epsilon to be very small, such as 1
# this means we will receive less information about how many people are from Salmon, Idaho
# because the privacy budget is small, so we get a noisy answer such as 10 people
# from Salmon Idaho

# we see that it is harder to figure out what the true answer could be since
# we received a very noisy answer
# 

# if i don't care, we can set epsilon = 8
# having a larger privacy budget or epsilon means we will gain more information about
# people being from Salmon
# this time the answer will be close to the truth
# essentially, differential privacy finds the most extreme possible person in a data set
# and sees how much the answer to a query changes if that person is present or not

# This provides guidance for how much noise to add given the value of epsilon
# without making assumptions on how a data intruder will behave
# epsilon cannot be zero!!!

# global sensitivity
# how algorithms implement
# the global sensitivity of a query to decide how much noise should be added in addition
# to the privacy budget

# global sensitivty of couting queries
# how many people are form Salmon, Idaho
# data with me - 1
# data without me - 0
# the most the answer could change for counting query is 1

# global sensitivity of other queries
# n is total number of obs
# a is the lower bound of the data
# b is the upper bound of the data

# Counting: 1
# Proportion: 1/n
# mean: (b-a)/n
# variance: (b-a)^2/n

# Global sensitivity and noise
# small global sensitivity results in less noise (e.g., median income)
# large global sensitivity results in more noise (e.g., maximum income)
# 

# Number of observations
n <- nrow(fertility)

# Global sensitivity of counts
gs.count <- 1

# Global sensitivity of proportions
gs.prop <- 1/n

# 
# Sensitivity of Mean and Variance Queries
# 
# Now, you'll calculate the global sensitivity of the mean and proportion queries for Hours_Sitting in the Male Fertility data set.
# 
# Remember that you've already calculated the number of observations n, and that the Hours_Sitting is normalized from 0 to 1, so the lower and upper bounds of Hours_Sitting are 0 and 1, respectively.
# 
# Global Sensitivity
# 
# Mean:
#     
#     Variance:
#     where a is the lower bound of the data, and b is the upper bound.

# 
# Lower bound of Hours_Sitting
a <- 0

# Upper bound of Hours_Sitting
b <- 1

# Global sensitivity of mean for Hours_Sitting
gs.mean <- (b - a) / n

# Global sensitivity of proportions Hours_Sitting
gs.var <- (b - a)^2 / n

#



##################################
## Laplace mechanism
###################################
## Lap(S(f)/epsilon)
## S(f) global sensitivity
## epsilon: privacy budget

## S(f) smaller OR epsilon larger, 
## we will have large probability of drawing values close to 0 from this Laplace distribution
## this means smaller noise

## 
fertility %>% summarize_at(vars(Child_Disease), sum)

## 
# rdoublex(draws, mean, shaping)
# 
set.seed(42)
rdoublex(1, 87, 1/10)
rdoublex(1, 87, 1/0.1)

# 
# Load dplyr and smoothmest packages
library(dplyr)
library(smoothmest)

# How many participants had a Surgical_Intervention?
n_surgeries <- fertility %>%
    summarize_at(vars(Surgical_Intervention), sum) %>%
    pull()

n_surgeries

# Set the seed
set.seed(42)

# Apply the Laplace mechanism
eps <- 0.1
rdoublex(1, 51, 1 / 0.1)

#
# Proportion of Accident_Trauma
accident_prop <- fertility %>%
    summarize_at(vars(Accident_Trauma), mean) %>%
    pull()

accident_prop

# Set the seed
set.seed(42)

# Apply the Laplace mechanism
eps <- 0.1
rdoublex(1, accident_prop, 0.01 / 0.1)


# Mean and Variance of Hours Sitting
hours_summary <- fertility %>%
    summarize_at(vars(Hours_Sitting), funs(mean, var))
hours_summary

# Setup
set.seed(42)
eps <- 0.1

# Laplace mechanism to mean
rdoublex(1, hours_summary$mean, ((1-0)/100) / 0.1 )

# Laplace mechanism to variance
rdoublex(1, hours_summary$var, (1-0)^2/100 / 0.1 )

# Number of Participants with abnormal diagnosis
n_diagnosis <- fertility %>%
    summarize_at(vars(Diagnosis), sum) %>%
    pull()

# Mean of age
mean_age <- fertility %>%
    summarize_at(vars(Age), mean) %>%
    pull()

# Set seed
set.seed(42)

# Laplace mechanism to the count of abnormal diagnosis
rdoublex(1, n_diagnosis, gs.count / eps)

# Laplace mechanism to the mean of Age
rdoublex(1, mean_age, gs.mean / eps)

