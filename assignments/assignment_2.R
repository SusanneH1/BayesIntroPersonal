# Assignment 1
# Susanne Hantke
# Matriculation number: 03751129

#----------------------------------------------------------------------------------------------------------------------------------------------------------

# packages used:
library(tidyverse)


#----------------------------------------------------------------------------------------------------------------------------------------------------------

# Bayesian Updating

#tasknumber 1

# Customer satisfaction:
# P(S|A) = 0.7
p_satisfied_when_A = 0.7
# P(S|B) = 0.5
p_satisfied_when_B = 0.5
# P(S|C) = 0.8
p_satisfied_when_C = 0.8
# Prior:
# B twice as likely as A and as C
# P(A) = 1/(2+1+1) = 0.25
p_A = 1/(2+1+1)
# P(B) = 2/(2+1+1) = 0.5
p_B = 2/(2+1+1)
# P(C) = 1/(2+1+1) = 0.25
p_C = 1/(2+1+1)
# 10 reviews:
# 6 positive, 4 negative
# P(S) = 0.6
p_satisfied = 0.6*(p_A+p_B+p_C)
# Posterior probability that the reviews are on A:
# P(A|S) = P(S|A)*P(A)/P(S) = 0.7*0.25/0.6 = 0.29 q.e.d
p_A_when_0.6satisfied = p_satisfied_when_A * p_A / p_satisfied
p_B_when_0.6satisfied = p_satisfied_when_B * p_B / p_satisfied
p_C_when_0.6satisfied = p_satisfied_when_C * p_C / p_satisfied
p_A_when_0.6satisfied

# Nenner für P(H|E): P(H)*P(E|H)+P(-H)*P(E|-H)
# P(S) = P(A)*P(S|A)+P(-A)*P(S|-A)
# 0.25*0.7+(1-0.25)* ??

p_A_when_0.9satisfied+p_B_when_0.9satisfied+p_C_when_0.9satisfied

#tasknumber 2 ??

# Posterior probability that the reviews are on C considering only the first 10 reviews:
# P(C|S) = P(S|C)*P(C)/P(S) = 0.8*0.25/0.6 = 0.33
print('Posterior probability that company C received the reviews considering only the first 10 reviews:')
p_C_when_0.6satisfied
# -> increase by 33 percentage points: p_C_when_0.9satisfied = 0.66 !
 
# 10 further reviews, 9 positive:
# New prior probability is old posterior
# P(S) = 0.9
p_satisfied2 = 0.9*(p_A_when_0.6satisfied+p_B_when_0.6satisfied+p_C_when_0.6satisfied)
p_C_when_0.9satisfied = p_satisfied_when_C * p_C_when_0.6satisfied / p_satisfied2
p_C_when_0.9satisfied
p_A_when_0.9satisfied = p_satisfied_when_A * p_A_when_0.6satisfied / p_satisfied2
p_B_when_0.9satisfied = p_satisfied_when_B * p_B_when_0.6satisfied / p_satisfied2


#tasknumber 3

# Factory receives equally many shipments from factory A and B:
p_A = 0.5
p_B = 0.5
# Shipment entailing defective parts:
# from factory A: 10% of the time
p_defect_when_A = 0.1
# from factory B: 20% of the time
p_defect_when_B = 0.2
# Received shipment contains defective parts
p_defect = 1
p_A_when_defect = p_defect_when_A * p_A / p_defect
p_B_when_defect = p_defect_when_B * p_B / p_defect
# What is the probability that the next shipment from the company will also contain defective products?

# shipment FROM the COMPANY = shipment TO the COMPANY or shipment FROM the FACTORY



