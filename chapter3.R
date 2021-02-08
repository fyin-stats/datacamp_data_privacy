############
############
############
## chapter 3
############
############
############
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("dplyr", "smoothmest")
ipak(packages)


# DP property
# if a person keeps querying a database, he will eventually figure out the true
# answer
# sequential composition
# what's the average income and maximum (two queries)
# the privacy budget must be divided by two
# 
# Mean and Variance of hours sitting
fertility %>% summarize_at(vars(Hours_Sitting), funs(mean, var))

# apply the laplace mechanism
set.seed(42)
rdoublex(1, 0.41, gs.mean / 0.1)
rdoublex(1, 0.19, gs.var / 0.1)

# for hours sitting in the fertility data
# GS mean
# GS variance
# mean
# variance

# take into account the number of queries
eps <- 0.1/2

# GS mean and variance
# 

##########################################
# parallel composition
# if our queries only pertain to certain parts of the data and do not overlap
# if multiple queries are sent to different parts of the data that do not overlap
# in information, you don't need to divide epsilon
# suppose our two queries are, what is the average income for men
# what is the average income for women
# the privacy budget does need to be divided
# the query with the most epsilon is the budget for the data

# set value of epsilon
# 
eps <- 0.1

# GS of mean for hours_sitting
gs.mean <- 1/100

# 
set.seed(42)
rdoublex(1, 0.39, gs.mean/eps)
rdoublex(1, 0.54, gs.mean/eps)
# 


# Set Value of Epsilon
eps <- 0.1	

# Mean of Age per diagnosis level 
fertility %>%
    group_by(Diagnosis) %>%
    summarize_at(vars(Age), mean)

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



####################################################
### post processing
### if a new query can be answered by using other queries that already had a differentially private
### mechanism applied to them, we do not need to use or divide up our privacy budget to protect that information
### we can use the other differentially private queries to indirectly create a noisy answer for our new query
### male fertility data: prepping data


###
fertility %>% count(Smoking)

###
# load the data
load("./data.RData")

### decide privacy budget
eps <- 0.1

### global sensitivity for counts : 1
### male fertility data: apply the Laplace mechanism
### 
gs.count <- 1

set.seed(42)
smoking1 <- rdoublex(1, 56, gs.count / eps / 2) %% round()
smoking2 <- rdoublex(1, 23, gs.count/eps/2) %>% round()

# Post process based on previous queries
smoking3 <- nrow(fertility) - smoking - smoking2

# 



##############################
##############################
##### impossible and inconsistent answers
##############################
##############################
# negative counts: solution: bound the values
# set value for epsilon
eps <- 0.01
# GS of counts
gs.count <- 1
# Number of participants with abnormal diagnosis
fertility %>% summarize_at(vars(Diagnosis), sum)

# apply the laplace mechanism
# 
set.seed(22)
rdoublex(1, 12, gs.count / eps) %>% round()

# apply the laplace mechanism and set.seed(22)
# set.seed(22)
# 
rdoublex(1, 12, gs.count/eps) %>% round() %>% max(0)

# suppose we set a different seed
set.seed(12)
noisy_answer <- rdoublex(1, 12, gs.count / eps) %>% round() %>% max(0)

# can also add ifelse to guarantee the noise is under the total number of observations
# normalizing noise: prepping data
# 
eps <- 0.01
# GS of counts
gs.count <- 1
# 

fertility %>% count(Smoking)

# normalizing noise: apply the laplace mechanism
smoking1 <- rdoublex(1, 56, gs.count/eps/2) %>% max(0)
smoking2 <- rdoublex(1, 23, gs.count/eps/2) %>% max(0)
smoking3 <- rdoublex(1, 21, gs.count/eps/2) %>% max(0) # in case we have negative numbers

# normalize smoking 
normalized <- (smoking/sum(smoking))*(nrow(fertility))

# round the values
round(normalized)

# 




# Normalizing noise
# 
# In this exercise, you'll first be applying the Laplace mechanism and then normalizing the 
# noisy results to ensure the total number of observations is consistent. 
# Recall how normalization works: first, we divide each noisy count by the sum of noisy counts, so that the new values would add up to 1. Then, by multiplying each of these new values by the desired total, we will ensure that they add up to that desired total.
# 
# Again, for the Male Fertility Data (fertility), the query is "What is the number of participants 
# in each level of High_Fevers?" (the true values are 9, 63, and 28). Additionally, 
# eps has already been assigned as 0.01 / 2 and gs.count as 1.

# Set the seed
set.seed(42)

# Apply the Laplace mechanism and avoid negative counts
fever1 <- rdoublex(1, 9, gs.count / eps) %>%
    max(0)
fever2 <- rdoublex(1, 63, gs.count / eps) %>%
    max(0)
fever3 <- rdoublex(1, 28, gs.count / eps) %>%
    max(0)


# 