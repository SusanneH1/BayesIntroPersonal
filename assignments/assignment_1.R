# Assignment 1
# Susanne Hantke
# Matriculation number: 03751129

#----------------------------------------------------------------------------------------------------------------------------------------------------------

# packages used:
library(tidyverse)


#----------------------------------------------------------------------------------------------------------------------------------------------------------
  
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

sumge7 <- filter(both,sum>=7 & d1==3)        # subset of the data where sum >= 7
p_sumge7 <- nrow(sumge7)/nrow(both)  # compute probability
p_sumge7


#tasknumber 6
# 6. Compute the probability that the sum lies between 4 and 9. (2 points)

sumbtw4n9 <- filter(both,4<sum & sum<9) # subset of the data where sum lies between 4 and 9 (excluding 4 and 9)
p_sumbtw4n9 <- nrow(sumbtw4n9)/nrow(both)
p_sumbtw4n9


#tasknumber 7
# 7. What is the probability of the most probable sum? (2 points)

p_sum_mostprob <- sort(prop.table(table(both$sum)), decreasing=T)[1]
p_sum_mostprob

# table(both$sum) counts occurrence of each possible value in both$sum
# prop.table(...) calculates the value of each cell in the occurrence table as a proportion of all occurrences (which are here the probabilities)
# sort(...) sorts the proportions (i.e., the probabilities) in decreasing order

#----------------------------------------------------------------------------------------------------------------------------------------------------------
  
# Assume you take 10 trains, each of which is either delayed (“L”) or on time (“O”). Assume
# the possible probabilities of delay are p=0, p=0.1, p=0.2, p=0.3, p=0.4, p=0.5, p=0.6, p=0.7,
# p=0.8, p=0.9, p=1.

#tasknumber 8
# 8. For each possible probability of delay, compute the probability distribution over all possible
# numbers of delays using the dbinom() function. (2 points)
# Bonus: Store all probability distributions in one data frame, where columns are the possible
# numbers of delay and rows are the possible probabilities of delay (or vice versa).

probabilities <- seq(0,1,0.1)                                                             # possible probabilities of delay
prob_distr <- data.frame(matrix(nrow = 11, ncol = length(probabilities)))                 # create empty data frame
colnames(prob_distr) <- c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1")  # rename columns

# rows are possible numbers of delays
for(i in 0:10){
  # columns are possible probabilities of delay
  for(j in 1:length(probabilities)){
    # compute probability distribution and store in data frame
    k = i+1
    prob_distr[k,j] <- dbinom(x=i, size=10, prob=probabilities[j])
  }
}
# change row names
row.names(prob_distr) <- seq(0,10,1)


#tasknumber 9
# 9. Use the code below to generate train ride data. Use the dbinom() function to compute
# the likelihood of the data (observed number of delays), given p=0, p=0.1, p=0.2, p=0.3,
# p=0.4, p=0.5, p=0.6, p=0.7, p=0.8, p=0.9, p=1. (2 points)

# train ride data
sim_rides <- function(N, p){
  sample(c("L", "O"), size=N, replace=TRUE, prob=c(p, 1-p))
}
set.seed(1237)
obs <- sim_rides(10, .3)  # observed number of delays (out of 10 train rides with a probability of delay of 0.3)
obs

probabilities <- seq(0,1,0.1)                                                             # possible probabilities of delay
likelihood <- data.frame(matrix(nrow = 1, ncol = length(probabilities)))                  # create empty data frame
colnames(likelihood) <- c("0","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1")  # rename columns

# compute the likelihood of the data
for(j in 1:length(probabilities)){
  likelihood[,j] <- dbinom(x=sum(obs == "L"), size=length(obs), prob=probabilities[j])
}

#tasknumber 10
# 10. Assume the below prior probabilities. Use the Bayes’ rule to calculate the posterior
# probability using the prior probabilities and the likelihoods that you computed. (2 points)

prior <- c(0.000, 0.004, 0.041, 0.123, 0.209, 0.246, 0.209, 0.123, 0.041, 0.004, 0.000) # prior probabilities

# Thoughts:
# prior probabilities have been computed with: 10 sided dice since length(probabilities) = 11

posterior <- likelihood * prior




