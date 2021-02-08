################# 
#################
#################
### Data privacy and anonymization
#################
#################
#################
# https://learn.datacamp.com/courses/data-privacy-and-anonymization-in-r
# ensure the privacy of the data released to the public
# 
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}
packages <- c("dplyr")
ipak(packages)

# load the data
load("./data.RData")

# researchers, public should not know who, in the dataset, has cancer
# governments should not disallow the researchers to get the data
# Course outline:
# chapter 1, removing identifiers and generating synthetic data
# chapter 2, differential privacy and Laplace mechanism
# chapter 3, differentially private properties
# chapter 4, differentially private data synthesis

# White house salary and fertility data sets
# 

# library(dplyr)
# whitehouse
# whitehouse

# removing identifiers
whitehouse %>% mutate(Name = 1:469)
# or
whitehouse %>% mutate(Name = sample(1:1000, 469))

# rounding
whitehouse %>% mutate(Salary = round(Salary, digits = -3)) # round it to the nearest thousands

# The White House salary data
# Histogram of salaries
# 

# Histogram of salaries
# 
whitehouse.gen <- whitehouse %>% mutate(Salary = ifelse(Salary < 100000, 0, 1))

whitehouse.gen

# Top coding (code all numbers >= 165000 as 165000)

# 
whitehouse %>% count(Status) # number of people in each category

# 
whitehouse %>% count(Status, Title, sort = TRUE)

# summarize_at()
whitehouse %>% summarise_at(vars(Salary), funs(mean, sd))


# 
# Convert the salaries into three categories
whitehouse.gen <- whitehouse %>%
    mutate(Salary = ifelse(Salary < 50000, 0, 
                           ifelse(Salary >= 50000 & Salary < 100000, 1, 2)))

whitehouse.gen

# 
# Bottom Coding
whitehouse.bottom <- whitehouse %>%
    mutate(Salary = ifelse(Salary <= 45000, 45000, Salary))

# Filter Results	
whitehouse.bottom %>%
    filter(Salary <= 45000)

# Counts of the Groups in Child_Disease	and Accident_Trauma	
fertility %>%
    count(Child_Disease, Accident_Trauma)


###################################################
# Data synthesis
# Probability distributions
###################################################
## one approach to achieving anonymization, because the idea
## is we are creating a fake dataset with fake people that is
## statistically representative of the original data
## create a fake dataset with fake data that is still statistically representative
## of the original data

## Male fertility data


## Generating synthetic data part 1
## sampling from a binomial distribution
## 
fertility %>% summarize_at(vars(Child_Disease), mean)

# 
set.seed(42)
child.disease <- rbinom(100,1,0.87)

# 
sum(child.disease)

# Examining the data
# Hours sitting
# need to make sure the generated data are proper 
# e.g., hard bounding
# or resampling until you obtain proper values

# 

# Square root Transformation of Salary
whitehouse.salary <- whitehouse %>%
    mutate(Salary = sqrt(Salary))

# Calculate the mean and standard deviation
salary_stats <- whitehouse.salary %>%
    summarize_at(vars(Salary), funs(mean, sd))

salary_stats

# Generate Synthetic data
set.seed(42)
salary_transformed <- rnorm(nrow(whitehouse), salary_stats$mean, salary_stats$sd)

# Square the generated data
salary_original <- salary_transformed^2

# Hard bound
salary <- ifelse(salary_original < 0, 0, salary_original)