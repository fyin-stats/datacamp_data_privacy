###############
###############
### chapter 4
###############
###############

#########################################
## differentially private data synthesis
#########################################
# non parametric
# and parametric approaches

# Laplace sanitizer
# T(xi) = f(xi) + ri
# ri iid draws from Lap(S(f)/epsilon)
# 


# previously, we did post processing and set value of epsilon to be 
# eps <- 0.01 / 2
fertility %>% count(High_Fevers)
# GS of counts
gs.count <- 1
eps <- 0.01

# male fertility data: apply the laplace mechanism
# 
set.seed(42)
fever1 <- rdoublex(1, 9, gs.count / eps) %>% max(0)
fever2 <- rdoublex(1, 63, gs.count / eps) %>% max(0)
fever3 <- rdoublex(1, 28, gs.count / eps) %>% max(0)

# 
fever <- c(fever1, fever2, fever3)

# 
normalized <- fever/sum(fever) * nrow(fertility)

# 
round(normalized)

# male fertility data: generating synthetic data
# 

# Set the seed
set.seed(42)	

# Apply the Laplace mechanism to each season
winter <- rdoublex(1, 28, gs.count / eps) %>%
    max(0)
spring <- rdoublex(1, 37, gs.count / eps) %>%
    max(0)
summer <- rdoublex(1, 4, gs.count / eps) %>%
    max(0)
fall <- rdoublex(1, 31, gs.count / eps) %>%
    max(0)

#
# Normalizing Noise and Generating Synthetic Data
# 
# Finally, based on the results from the previous two exercises, 
# you'll be generating synthetic data. However, you first need to make sure the results are consistent; the sum of the counts adding up to the overall number of observations. Then, you'll generate the synthetic data using rep().
# 
# The vectors you created in the earlier exercise are combined and available in seasons.

# Generate synthetic data for winter
winter_synth <- rep(-1, 29)
winter_synth

# Generate synthetic data for spring
spring_synth <- rep(-0.33, 38)
spring_synth

# Generate synthetic data for summer
summer_synth <- rep(0.33, 0)
summer_synth

# Generate synthetic data for fall
fall_synth <- rep(1, 33)
fall_synth

# laplace sanitizer: parametric approach

####################################################################
####################################################################
# DP parametric approaches
####################################################################
####################################################################
# use distributions to generate synthetic data
# male fertility data

# 
fertility %>% summarize_at(vars(Child_Disease), mean)

# 
set.seed(42)
rdoublex(1, 0.87, (1/100)/0.1)

# 
set.seed(42)
child.disease <- rbinom(100, 1, 0.89)

# 
sum(child.disease)

# Generating DP synthetic data part 2
# 
fertility %>% mutate(Hours_Sitting = log(Hours_Sitting)) %>% summarize_at(vars(Hours_Sitting),
                                                                          funs(mean, var))

# 
set.seed(42)
rdoublex(1, -1.01, (1/100)/0.01/2)
rdoublex(1, 0.25, (1/100)^2/0.01/2) # don't forget we need to divide epsilon by two since
# we are querying same part of the data twice

# 
set.seed(42)
hours.sit <- rnorm(100, -0.91, sqrt(0.25))

# 
hours.sit <- exp(hours.sit)

# hard bound the data 
hours.sit[hours.sit < 0] = 0
hours.sit[hours.sit > 1] = 1

# 

# Calculate proportions
fertility %>%
    summarize_at(vars(Accident_Trauma, Surgical_Intervention), mean)

# Number of Observations
n <- nrow(fertility)

# Set Value of Epsilon
eps <- 0.1

# GS of Proportion
gs.prop <- 1 / n

accident_prop <- 0.44
surgical_prop <- 0.51

# Apply the Laplace mechanism
set.seed(42)	

accident_prop_noisy <- rdoublex(1, accident_prop, gs.prop / eps) 
accident_prop_noisy 

surgical_prop_noisy <- rdoublex(1, surgical_prop, gs.prop / eps) 
surgical_prop_noisy

# Generate Synthetic data
accident_data_noisy <- rbinom(n, 1, accident_prop_noisy)
surgical_data_noisy <- rbinom(n, 1, surgical_prop_noisy)



# Set Value of Epsilon
eps <- 0.1/2

# Number of observations
n <- nrow(fertility)

# Upper and lower bounds of age
a <- 0
b <- 1

# GS of mean and variance for age
gs.mean <- (b-a)/n
gs.var <- (b-a)^2/n


# Mean and variance of Age
age_summary <- fertility %>%
    summarize_at(vars(Age), funs(mean, var))
age_summary

# Apply the Laplace mechanism
set.seed(42)

age_mean_noisy <- rdoublex(1, age_summary$mean, gs.mean / eps)
age_var_noisy <- rdoublex(1, age_summary$var, gs.var / eps)

# Generate Synthetic data
age_noisy <- rnorm(n, age_mean_noisy, sqrt(age_var_noisy))

# Hard Bounding the data
age_noisy[age_noisy < 0] <- 0
age_noisy[age_noisy > 1] <- 1

# wrap up


# More on data privacy
# issues: complex solutions for complex data
# biasing inferences

# other topics
# others versions of differential privacy
# local privacy: apple
# probablistic privacy: US census Bureau
# 