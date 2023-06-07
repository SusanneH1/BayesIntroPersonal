# Exercise Session 6

# Modeling Association between Height, Weight, Age

library(rethinking)

data("Howell1")
dat <- Howell1
head(dat, 5)

# mean values
mu_h <- mean(dat$height)
mu_w <- mean(dat$weight)
mu_a <- mean(dat$age)

# standard deviations
si_h <- sd(dat$height)
si_w <- sd(dat$weight)
si_a <- sd(dat$age)

# plot to see relationship between parameters
plot(dat$height, dat$weight)
plot(dat$age, dat$height)
plot(dat$age, dat$weight)


# Regression

# Choosing priors:

# dnorm if both positive and negative values are possible and equally likely
# dunif if min and max values are known
# dbeta if min and max values are known and some values in the range are assumed to be more likely


# weight ~ a + b*height
# with mean-centering
m1 <- quap(
  alist(
    weight ~ dnorm(mu, sd), 
    mu <- a + b * (height-mu_h),
    a ~ dnorm(70, 10),
    b ~ dnorm(0, 3),
    sd ~ dunif(0, 10)
  ),
  data = dat)
precis(m1)


# weight ~ a + b*height + b2*age
# with mean-centering
m2 <- quap(
  alist(
    weight ~ dnorm(mu, sd), 
    mu <- a + b * (height-mu_h) + b2 * (age-mu_a),
    a ~ dnorm(70, 10),
    b ~ dnorm(0, 3),
    b2 ~ dnorm(0, 3),
    sd ~ dunif(0, 10)
  ),
  data = dat)
precis(m2)

# weight ~ a + b*age
# with mean-centering
m3 <- quap(
  alist(
    weight ~ dnorm(mu, sd), 
    mu <- a + b * (age-mu_a),
    a ~ dnorm(40, 7),
    b ~ dnorm(0, 3),
    sd ~ dunif(0, 20) #larger standard deviation due to greater deviations of weight when older than when taller
  ),
  data = dat)
precis(m3)