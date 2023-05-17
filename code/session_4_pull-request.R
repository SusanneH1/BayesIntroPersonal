#library(tidyverse)
library(ggplot2)


# draw 10,000 values from a beta distribution with shape parameters a = 4 and b = 4.
n <- 1e4
scale <- 1.5e4
income <- round( rbeta(n=n, shape1=4, shape2=4) * scale, 2)


# Plot the resulting curve
ggplot(data.frame(x = income), aes(x=x)) +
  geom_histogram(color = "#0065BD", fill = "#0065BD", alpha=0.5, bins = 50) +
  scale_x_continuous(breaks = seq(0, scale, 1000)) + 
  labs(x = "Gross income", 
       y = "Counts") + 
  theme_minimal()

