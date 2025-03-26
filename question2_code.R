# load libraries
library(tidyverse)

# simulate fasting glucose data
set.seed(42)
n_per_group <- 30
glucose_obese <- rnorm(n_per_group, mean = 110, sd = 15)
glucose_nonobese <- rnorm(n_per_group, mean = 100, sd = 10)

#make dataframe and look at the data
df <- data.frame(
  glucose = c(glucose_obese, glucose_nonobese),
  group = rep(c("Obese", "NonObese"), each = n_per_group)
)

ggplot(df, aes(x = group, y = glucose, fill = group)) +
  geom_boxplot() +
  labs(title = "Fasting Glucose Levels by Group", y = "Glucose (mg/dL)")

# check assumptions
#use the Shapiro test to check whether the considered data is normally distributed or not
#null states that the population is normally distributed, if the p-value is greater than 0.05 then the null hypothesis is accepted
shapiro.test(glucose_obese) 
shapiro.test(glucose_nonobese) #both are greater which shows that both are normally distributed
#preform F-test for comparing two variances
var.test(glucose_obese, glucose_nonobese)  # equal variances

# two-sample t-test (Welch if variances unequal)
t_test_result <- t.test(glucose_obese, glucose_nonobese, var.equal = FALSE)
print(t_test_result)
