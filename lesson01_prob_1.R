## PROBLEM FRIZZY DRINK

mu_0 = 120  # target value
n = 10    # samples number
days <- c(108, 124, 124, 106, 115, 138, 163, 159, 134, 139)
alpha = 0.01
# using a significance level of 1% we want to find the confidence that we reach the target T-test unknown variance
y = mean(days)
s = sd(days)

t_0 = (y - mu_0)/(s/sqrt(n))
t_alpha = qt(alpha, n - 1, lower.tail = T)

# t_0 is not bigger than t_alpha,n-1 -> the outcome is that we have to accept the null hypothesis

t.test(days, mu = mu_0, alternative = "g", conf.level = 0.01) # usually two side, must select one-sided
# we check the p-value, if it less than alpha, the null hypothesis may be rejected