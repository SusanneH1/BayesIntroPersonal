# Assignment 3
# Susanne Hantke
# Matriculation number: 03751129

#----------------------------------------------------------------------------------------------------------------------------------------------------------

# packages used:
library(tidyr)
library(ggplot2)
library(dplyr)
library(rethinking)

#----------------------------------------------------------------------------------------------------------------------------------------------------------

# Pre-Processing

# tasknumber 1
data <- read.csv("Aging.csv") # load data
View(data)

# tasknumber 2
data <- data %>% drop_na() # drop rows containing missing values
View(data)

# tasknumber 3

# Inspect the distribution of all variables

summary(data) # e.g. min, max and mean of each variable

# Draw each column as density
data_long <- data %>%   # Convert data from wide to long format
  pivot_longer(colnames(data)) %>%
  as.data.frame()
head(data_long) 
plt_densities <- ggplot(data_long, aes(x = value)) +
  geom_density() + 
  facet_wrap(~ name, scales = "free")
plt_densities

# Add new variable AG (1 = young, 2 = old)
# Use median (46.5 years) as threshold
data <- data %>% mutate(AG = if_else(Age>=46.5, 2, 1))
View(data)

##############################################

# Gaussian Models of Decision Quality and Risk Seeking

# tasknumber 4

### Gaussian model for DecisionQuality: DQ ~ Normal(mu,sigma)

N_decisions <- 1e4

## Priors on mu and sigma

# mu
# decision quality in range 0 to 1
# humans do not always choose the option with higher expectation (are not good in statistical thinking)
range <- seq(-0.2, 1.2, length.out = 100)
d <- dnorm(range, mean = 0.6, sd = 0.15)
mu <- data.frame(range, d)
ggplot(mu, aes(x = range, y = d)) +
  geom_line(size = 1, color = "#552583") +
  scale_x_continuous(limits = c(-0.2,1.2), breaks = seq(-0.2,1.2, 0.2)) + 
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

# sigma
range <- seq(-0.1, 0.3, length.out = 100)
d <- dunif(range, min = 0, max = 0.15)
sigma <- data.frame(range, d)
ggplot(sigma, aes(x = range, y = d)) +
  geom_line(size = 1, color = "#552583") +
  scale_x_continuous(limits = c(-0.1, 0.3), breaks = seq(-0.1, 0.3, 0.1)) + 
  labs(x = expression(sigma), 
       y = "Density") +
  theme_minimal()

# Prior predictive check
mu_samples <- rnorm(N_decisions, 0.6, 0.15)
sigma_samples <- runif(N_decisions, 0, 0.15)

prior_pred <- tibble(dq = rnorm(N_decisions, mu_samples, sigma_samples))
prior_pred %>% ggplot(aes(x = dq)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 20) + 
  labs(x = "DecisionQuality", 
       y = "Frequency") + 
  theme_minimal()
# Prior distribution is ok since 0 < dq < 1


## Simulate fake data

sim_dq_static <- function(N_dec, mu_dec, sd_dec){
  rnorm(N_dec, mu_dec, sd_dec)
}

dq_static <- tibble(dq = sim_dq_static(N_decisions, 0.6, 0.1))
ggplot(dq_static, aes(x = dq)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 30) + 
  labs(x = "Decision Quality", 
       y = "Frequency") + 
  theme_minimal()

## Fit model on simulated data

m_desc <- alist(dq ~ dnorm(mu, sigma),
                mu ~ dnorm(0.6, 0.15),
                sigma ~ dunif(0, 0.15))

m_fit <- quap(m_desc, data=dq_static)

precis(m_fit)

## Use model to analyze data

# Fit model to data
m_decisions <- quap(
  alist(DecisionQuality ~ dnorm(mu, sigma),
        mu ~ dnorm(0.6, 0.15),
        sigma ~ dunif(0, 0.15)),
  data = data)

precis(m_decisions)

data %>% ggplot(aes(x=DecisionQuality)) + 
  geom_histogram(fill = "#552583", alpha = .5, color = "#552583", bins = 30) + 
  theme_minimal()

#####

### Gaussian model for RiskSeeking: RS ~ Normal(mu,sigma)

## Priors on mu and sigma

# mu
# risk seeking in range 0 to 1
# participants assumed to be neither risk seeking nor risk avoiding in general
range <- seq(-0.2, 1.2, length.out = 100)
d <- dnorm(range, mean = 0.4, sd = 0.2)
mu <- data.frame(range, d)
ggplot(mu, aes(x = range, y = d)) +
  geom_line(size = 1, color = "#552583") +
  scale_x_continuous(limits = c(-0.2,1.2), breaks = seq(-0.2,1.2, 0.2)) + 
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

# sigma
range <- seq(-0.1, 0.3, length.out = 100)
d <- dunif(range, min = 0, max = 0.2)
sigma <- data.frame(range, d)
ggplot(sigma, aes(x = range, y = d)) +
  geom_line(size = 1, color = "#552583") +
  scale_x_continuous(limits = c(-0.1, 0.3), breaks = seq(-0.1, 0.3, 0.1)) + 
  labs(x = expression(sigma), 
       y = "Density") +
  theme_minimal()

# Prior predictive check
mu_samples <- rnorm(N_decisions, 0.45, 0.1)
sigma_samples <- runif(N_decisions, 0, 0.2)

prior_pred <- tibble(rs = rnorm(N_decisions, mu_samples, sigma_samples))
prior_pred %>% ggplot(aes(x = rs)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 20) + 
  labs(x = "RiskSeeking", 
       y = "Frequency") + 
  theme_minimal()
# Prior distribution is ok since 0 < dq < 1


## Simulate fake data

sim_rs_static <- function(N_dec, mu_dec, sd_dec){
  rnorm(N_dec, mu_dec, sd_dec)
}

rs_static <- tibble(rs = sim_rs_static(N_decisions, 0.45, 0.2))
ggplot(rs_static, aes(x = rs)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 30) + 
  labs(x = "Decision Quality", 
       y = "Frequency") + 
  theme_minimal()

## Fit model on simulated data

m_desc_rs <- alist(rs ~ dnorm(mu, sigma),
                mu ~ dnorm(0.45, 0.2),
                sigma ~ dunif(0, 0.2))

m_fit_rs <- quap(m_desc_rs, data=rs_static)

precis(m_fit_rs)

## Use model to analyze data

# Fit model to data
m_decisions_rs <- quap(
  alist(RiskSeeking ~ dnorm(mu, sigma),
        mu ~ dnorm(0.6, 0.15),
        sigma ~ dunif(0, 0.15)),
  data = data)

precis(m_decisions_rs)

data %>% ggplot(aes(x=RiskSeeking)) + 
  geom_histogram(fill = "#552583", alpha = .5, color = "#552583", bins = 30) + 
  theme_minimal()



### Check posterior intervals

## DecisionQuality

m_dq_smp <- extract.samples(m_decisions, n = 1e3)

m_dq_smp %>%  ggplot(aes(x = mu)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

# Posterior intervals 

PI(m_dq_smp$mu)
HPDI(m_dq_smp$mu)

PI(m_dq_smp$sigma)
HPDI(m_dq_smp$sigma)


## RiskSeeking

m_rs_smp <- extract.samples(m_decisions_rs, n = 1e3)

m_rs_smp %>%  ggplot(aes(x = mu)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(mu), 
       y = "Density") +
  theme_minimal()

# Posterior intervals 

PI(m_rs_smp$mu)
HPDI(m_rs_smp$mu)

PI(m_rs_smp$sigma)
HPDI(m_rs_smp$sigma)


### Interpretation

## DecisionQuality
# In 89%, the quality of decisions has a mean of 0.63-0.66 which means that participants 
# took on average in 63-66% of their decisions the option with the highest expectation.
# Since this value is greater than 50%, the decision quality is not too bad. However,
# it is only slightly higher and thus confirming the assumption that humans have
# problems to incorporate statistical knowledge into their thinking/decisions.

## RiskSeeking
# In 89%, the risk seeking has a mean of 0.46-0.48 which means that participants took
# on average in 46-48% of their decisions the safer option (option A). Since this 
# value is less than 50%, the participants' risk attitude is rather low, i.e. the
# average of the participants are risk-averse.


# tasknumber 5

## Decision quality of young and old participants

# Create subsets for both age groups
data_young <- data[data$AG == 1, ]
View(data_young)
data_old <- data[data$AG == 2, ]
View(data_old)

# Estimate Gaussian model for each group separately

### Gaussian model for DecisionQuality of young people: DQ_young ~ Normal(mu,sigma)

N_decisions <- 1e4

## Priors on mu and sigma as before

# Prior predictive check
mu_samples <- rnorm(N_decisions, 0.6, 0.15)
sigma_samples <- runif(N_decisions, 0, 0.15)

prior_pred <- tibble(dq_young = rnorm(N_decisions, mu_samples, sigma_samples))
prior_pred %>% ggplot(aes(x = dq_young)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 20) + 
  labs(x = "DecisionQuality_Young", 
       y = "Frequency") + 
  theme_minimal()
# Prior distribution is ok since 0 < dq < 1


## Simulate fake data

dq_static_young <- tibble(dq_young = sim_dq_static(N_decisions, 0.6, 0.1))
ggplot(dq_static_young, aes(x = dq_young)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 30) + 
  labs(x = "Decision Quality Young", 
       y = "Frequency") + 
  theme_minimal()

## Fit model on simulated data

m_desc_young <- alist(dq_young ~ dnorm(mu, sigma),
                  mu ~ dnorm(0.6, 0.15),
                  sigma ~ dunif(0, 0.15))

m_fit_young <- quap(m_desc_young, data=dq_static_young)

precis(m_fit_young)

## Use model to analyze data

# Fit model to data
m_decisions_young <- quap(
    alist(DecisionQuality ~ dnorm(mu, sigma),
          mu ~ dnorm(0.6, 0.15),
          sigma ~ dunif(0, 0.15)),
    data = data_young)

precis(m_decisions_young)

data_young %>% ggplot(aes(x=DecisionQuality)) + 
  geom_histogram(fill = "#552583", alpha = .5, color = "#552583", bins = 30) + 
  theme_minimal()


### Gaussian model for DecisionQuality of old people: DQ_old ~ Normal(mu,sigma)

N_decisions <- 1e4

## Priors on mu and sigma as before

# Prior predictive check
mu_samples <- rnorm(N_decisions, 0.6, 0.15)
sigma_samples <- runif(N_decisions, 0, 0.15)

prior_pred <- tibble(dq_old = rnorm(N_decisions, mu_samples, sigma_samples))
prior_pred %>% ggplot(aes(x = dq_old)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 20) + 
  labs(x = "DecisionQuality_old", 
       y = "Frequency") + 
  theme_minimal()
# Prior distribution is ok since 0 < dq < 1


## Simulate fake data

dq_static_old <- tibble(dq_old = sim_dq_static(N_decisions, 0.6, 0.1))
ggplot(dq_static_old, aes(x = dq_old)) + 
  geom_histogram(fill = "#552583", 
                 alpha = .5, 
                 color = "#552583", 
                 bins = 30) + 
  labs(x = "Decision Quality old", 
       y = "Frequency") + 
  theme_minimal()

## Fit model on simulated data

m_desc_old <- alist(dq_old ~ dnorm(mu, sigma),
                      mu ~ dnorm(0.6, 0.15),
                      sigma ~ dunif(0, 0.15))

m_fit_old <- quap(m_desc_old, data=dq_static_old)

precis(m_fit_old)

## Use model to analyze data

# Fit model to data
m_decisions_old <- quap(
  alist(DecisionQuality ~ dnorm(mu, sigma),
        mu ~ dnorm(0.6, 0.15),
        sigma ~ dunif(0, 0.15)),
  data = data_old)

precis(m_decisions_old)

data_old %>% ggplot(aes(x=DecisionQuality)) + 
  geom_histogram(fill = "#552583", alpha = .5, color = "#552583", bins = 30) + 
  theme_minimal()

### Calculate difference between 10,000 samples from the two posterior distributions

m_smp_young <- extract.samples(m_decisions_young, n = 1e4)
m_smp_old <- extract.samples(m_decisions_old, n = 1e4)
m_smp_difference <- m_smp_young - m_smp_old
View(m_smp_difference)

### Visualize the distribution of difference

m_smp_difference %>%  ggplot(aes(x = mu)) +
    geom_density(color = "#552583", linewidth = 1, alpha = .1) +
    labs(x = expression(mu), 
         y = "Density") +
    theme_minimal()

m_smp_difference %>%  ggplot(aes(x = sigma)) +
  geom_density(color = "#552583", linewidth = 1, alpha = .1) +
  labs(x = expression(sigma), 
       y = "Density") +
  theme_minimal()

# -> mean of difference (m_smp_difference) < 0.02
# -> young and old people do not differ in decision quality on average
# (decision quality of young people only very slightly better on average by 0.02)


##############################################

# Linear Prediction

# tasknumber 6

# standardize columns (except Age and AG)
data_s <- data
data_s[2 : 6] <- as.data.frame(scale(data_s[2 : 6]))

# check that
colMeans(data_s[2:6]) #mean is zero
apply(data_s[2:6],2,sd) #sd is 1


# tasknumber 7
# simple linear predictions models

# DecisionQuality ~ Numeracy
m_DQ_N <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b * Numeracy, 
    a ~ dnorm(0, 1), 
    b ~ dnorm(.7, .2), #good calculation skills might help to take good decisions
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_DQ_N)


# DecisionQuality ~ Speed
m_DQ_S <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b * Speed, 
    a ~ dnorm(0, 1), 
    b ~ dnorm(.2, .2), #high speed might lead to less good decisions
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_DQ_S)


# DecisionQuality ~ NegAffect
m_DQ_NA <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b * NegAffect, 
    a ~ dnorm(0, 1), 
    b ~ dnorm(.3, .2), #negative mood might distract from thinking plausibly
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_DQ_NA)



# RiskSeeking ~ Numeracy
m_RS_N <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b * Numeracy, 
    a ~ dnorm(0, 1), 
    b ~ dnorm(.5, .2), #good calculation skills might not have any influence on risk seeking behaviour
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_RS_N)


# RiskSeeking ~ Speed
m_RS_S <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b * Speed, 
    a ~ dnorm(0, 1), 
    b ~ dnorm(.5, .2), #high speed might might not have any influence on risk seeking behaviour
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_RS_S)


# RiskSeeking ~ NegAffect
m_RS_NA <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b * NegAffect, 
    a ~ dnorm(0, 1), 
    b ~ dnorm(.6, .2), #negative mood might lead to risk seeking behaviour
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_RS_NA)


# advantages of using standardized variables for this task

# 1. coefficients of independent variables can be interpreted using the absolute values (of the column 'mean')
#    i.e. independent variables with a larger absolute value have stronger effect on dependent variable
# 2. results are easier to interpret: a unit increase in each variable is equal to its standard deviation
#    i.e. effect of each variable easier to grasp (no need to check the range of each variable first)


# Which variable has, on average, the strongest total effect on DecisionQuality?

# Numeracy


# Which variable has, on average, the strongest total effect on RiskSeeking?

# Numeracy



# tasknumber 8

# linear prediction model to estimate the associations among Speed, Numeracy and DecisionQuality

# assumptions:

# the higher Numeracy, the higher Speed
# the higher Speed, the lower DecisionQuality
# the higher Numeracy, the higher DecisionQuality

# -> confounding effect: fork + mediation

#  Speed --(-)--> DecisionQuality
#     ^                 ^
##    |                 |
#    (+)               (+)
##    |                 |
#     ----  Numeracy ----


# model
# DecisionQuality ~ Speed + Numeracy

m_DQ_S_N <- quap(
  alist(
    DecisionQuality ~ dnorm(mu, sd),
    mu <- a + b_1 * Speed + b_2 * Numeracy, 
    a ~ dnorm(0, 1), 
    b_1 ~ dnorm(.2, .2),
    b_2 ~ dnorm(.7, .2),
    sd ~ dunif(0, 1) 
  ),
  data = data_s)
precis(m_DQ_S_N)

# assumption "the higher Speed, the lower DecisionQuality": not fulfilled (b_1 > 0)
# assumption "the higher Numeracy, the higher DecisionQuality": fulfilled (b_2 > 0)
# assumption "the higher Numeracy, the higher Speed": not directly visible in the model results


