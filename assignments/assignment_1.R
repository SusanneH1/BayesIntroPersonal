# Assignment 1
# Susanne Hantke
# Matriculation number: 03751129

#---------------------------------------------------------------------------
# packages used:
library(tidyverse)


#---------------------------------------------------------------------------
  
# Assume you throw 2 dice.

#tasknumber 1
# 1. Create 2 vectors representing the sample spaces of both dice throws. (1 point)

d1 <- 1:6   # dice 1
d2 <- 1:6   # dice 2


#tasknumber 2
# 2. Use the 2 vectors and the expand_grid() function from the tidyverse package to generate
# a data frame representing all 36 possible outcomes of throwing both dice. Each row should
# represent one of the possible combinations, i.e.: (1 point)

both <- expand_grid(d1,d2)


#tasknumber 3
# 3. Add a column to the data frame that specifies the probability of each possible outcome
# (combination). (1 point)

both$p <- 1/(length(d1)*length(d2))


#tasknumber 4
# 4. Add a column to the data frame that specifies the sum across both dice for each possible
# outcome (combination). (1 point)

both$sum <- both$d1 + both$d2


# Tip for Tasks 5-7: Use the function subset() (or filter() from the tidyverse package) to
# create subsets of the data.

#tasknumber 5
# 5. Compute the probability that the sum is ≥ 7 , given the first dice shows 3. (2 points)


#tasknumber 6
# 6. Compute the probability that the sum lies between 4 and 9. (2 points)


#tasknumber 7
# 7. What is the probability of the most probable sum? (2 points)

#---------------------------------------------------------------------------
  
# Assume you take 10 trains, each of which is either delayed (“L”) or on time (“O”). Assume
# the possible probabilities of delay are p=0, p=0.1, p=0.2, p=0.3, p=0.4, p=0.5, p=0.6, p=0.7,
# p=0.8, p=0.9, p=1.

#tasknumber 8
# 8. For each possible probability of delay, compute the probability distribution over all possible
# numbers of delays using the dbinom() function. (2 points)

# Bonus: Store all probability distributions in one data frame, where columns are the possible
# numbers of delay and rows are the possible probabilities of delay (or vice versa).

#tasknumber 9
# 9. Use the code below to generate train ride data. Use the dbinom() function to compute
# the likelihood of the data (observed number of delays), given p=0, p=0.1, p=0.2, p=0.3,
# p=0.4, p=0.5, p=0.6, p=0.7, p=0.8, p=0.9, p=1. (2 points)

#tasknumber 10
# 10. Assume the below prior probabilities. Use the Bayes’ rule to calculate the posterior
# probability using the prior probabilities and the likelihoods that you computed. (2 points)

