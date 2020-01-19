## INTRODUCTION

split.screen(c(2,2))

screen(1)
curve(pt(x,10), xlim = c(-5, 5), xlab = "t", main = "Lower tail")

screen(2)
curve(pt(x,10), xlim = c(0, 1), xlab = "t", main = "Lower tail")

screen(3)
curve(pt(x,10, lower.tail = F), xlim = c(-5, 5), xlab = "t", main = "Upper tail")

screen(4)
curve(pt(x,10, lower.tail = F), xlim = c(0, 1), xlab = "t", main = "Upper tail")

## PROBLEM 1 - Z-TEST AND P-TEST
# desired viscosity -> 800 milliStokes
# number of smpls   -> 16
# previous mean     -> 812 milliStokes
# sigma of pop.     -> 25                 =>  we need a Z-test
# need alpha        -> 0.05
# for the  Z statistic we need that Z_0 > Z_alpha/2
#
# compute Z_0

mu_0 = 800
sigma = 25
n = 16
y = 812
alpha = 0.05

Z_0 = (y - mu_0)/(sigma/sqrt(n))
Z_0_25 = qnorm(alpha/2, lower.tail = F)
# Hence we cannot reject the null hypothesis, the value of Z0 is outside the critical area marked by the 5% of the problem

# we can also calulate the integrals, obtaining p values
pval = pnorm(Z_0, lower.tail = F)
# in this case we calculate a percentage form a Z value
# for this calculation the value pval*2 is higher than 5% so that we cannot reject the null hypothesis, gives
# a better understanding of how much the null hypothesis is far from the expected, better than Z-test for confidence

## PROBLEM 2 - CONFIDENCE INTERVALS
# defining two interval values with sigma, do those contain the expected mean? -> this also can conclude some inference
# it's the same than compute the 95% confidence range
u_l = y + Z_0_25 * sigma/sqrt(n)
l_l = y - Z_0_25 * sigma/sqrt(n)
