## PROBLEM 2 - ENTERPRISE
mu_0 = 225
alpha = 0.05
work_day <-c (159, 224, 222, 149, 280, 379, 362, 260, 101, 179, 168, 485, 212, 264, 170)
n = length(work_day)
s = sd (work_day)
y = mean(work_day)

t_alpha = qt(alpha, n-1, lower.tail = F)
t_0 = (y - mu_0)/(s/sqrt(n))