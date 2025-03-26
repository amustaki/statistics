#parameters 
set.seed(47) #for reproducibility 
total_genes <- 1000 #total number of genes that we are going to be preforming on 
bonferroni_alpha <- 0.05/total_genes #adjusted significance level 
baseline_rate <- 0.1 #percent of samples expressing gene G 
effect_size <- 0.1 #10% increase in expression 

lambda_control <- baseline_rate * 5000 #baseline mean expression
lambda_experiment <- lambda_control * (1 + effect_size) # experiemntal mean expression
sample_sizes <- seq(10,300, by = 10) # range of sample sizes to test

#make a function to calculate power
calculate_power <- function(n, lambda_control, lambda_experiment, alpha, simulations = 1000) {
  #simulate the data and make the calculation
  power <- mean(replicate(simulations, {
    control <- rpois(n,lambda = lambda_control)
    experiment <- rpois(n, lambda = lambda_experiment)
    p_value <- t.test(control, experiment)$p.value
    return(p_value < alpha)
  }))
  return(power)
}

#calculate power for each sample size 
power_results <- sapply(sample_sizes, calculate_power, 
                        lambda_control = lambda_control, 
                        lambda_experiment = lambda_experiment, 
                        alpha = bonferroni_alpha)

#find the minimum sample size to achieve 95% power 
required_sample_size <- sample_sizes[which(power_results >= 0.95)[1]]

#plot the power code
plot(sample_sizes, power_results, type = "l", col = "blue", lwd = 2, 
     xlab = "Sample Size per Group (n)",
     ylab = "Statistical Power", 
     main = "Power Analysis for Detecting Expression Change")
abline(h=0.95, col = "red", lty = 2) #line to show the 95% threshold 
abline(v= required_sample_size, col = "green", lty = 2) #line for required sample size 

#output for required sample size 
cat("Minimum sample size per group for 95% power:", required_sample_size, "\n")




