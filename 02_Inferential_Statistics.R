
###############################################################
# PROBLEM 1: Poisson Distribution
###############################################################
lambda <- 2
time_interval <- 3
prob_at_least_3 <- 1 - ppois(2, lambda * time_interval)
cat("Probability of at least 3 patients:", prob_at_least_3, "\n")

###############################################################
# PROBLEM 2: Binomial Distribution
###############################################################
n <- 15; p <- 0.75; k_values <- 0:15
probabilities <- dbinom(k_values, size=n, prob=p)
for (i in 1:length(k_values)) {
  cat("Probability of", k_values[i], "people:", probabilities[i], "\n")
}

###############################################################
# PROBLEM 3: Cumulative Binomial
###############################################################
n <- 400; p <- 0.10
total_prob <- pbinom(55, n, p) - pbinom(44, n, p)
cat("Probability 45<=x<=55:", round(total_prob,4), "\n")

###############################################################
# PROBLEM 4: Normality and Variance
###############################################################
sample_data <- c(132,133,91,108,67,169,54,203,190,133,96,130,187,121,163,166,104,110,157,138)
shapiro.test(sample_data)
var(sample_data)
sd(sample_data)

###############################################################
# CHI-SQUARE TEST FOR VARIANCE
###############################################################
population_sigma <- 45; n <- length(sample_data)
chi_stat <- (n-1) * var(sample_data) / population_sigma^2
df <- n - 1
pchisq(chi_stat, df)

###############################################################
# PROBLEM 5: Independent Samples t-test
###############################################################
n1 <- 31; x1 <- 199; s1 <- 28
n2 <- 31; x2 <- 172; s2 <- 21
alpha <- 0.01
t_stat <- (x1 - x2) / sqrt(s1^2/n1 + s2^2/n2)
df <- n1 + n2 - 2
p_value <- pt(t_stat, df, lower.tail=FALSE)
cat("t-stat:", round(t_stat,4), "p-value:", round(p_value,4), "\n")

###############################################################
# PROBLEM 6: F-test
###############################################################
group1 <- c(202.7,199.5,200.5,201.3,199.9,199.9,199.4,199.9,197.8,200.3)
group2 <- c(7.3,8,7.8,9,8.6,8.6,7.9,6.8,7.9,9.4)
var1 <- var(group1); var2 <- var(group2)
F_stat <- var1 / var2
cat("F-stat:", F_stat, "\n")

###############################################################
# PROBLEM 7: Difference in Proportions
###############################################################
n1 <- 50; x1 <- 18; n2 <- 50; x2 <- 20
p1 <- x1/n1; p2 <- x2/n2; p <- (x1 + x2)/(n1 + n2)
z_stat <- (p1 - p2) / sqrt(p*(1-p)*(1/n1 + 1/n2))
cat("z-stat:", round(z_stat,4), "\n")

###############################################################
# PROBLEM 8: Paired t-test
###############################################################
before <- c(74,84,81,110,105,100,110,68,79,86)
after <- c(115,140,176,191,158,180,179,140,167,157)
t.test(after, before, paired=TRUE)
