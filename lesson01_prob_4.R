## PROBLEM 4 - MORTAR
mortar <- read.table("mortar.dat", header = T)
attach(mortar)
mortar

# Alt + 126 -> tilde
# in this case it's not a paired test, we combine them both
t.test(Strength~Mortar, var.equal = T, conf.level = 0.05)
# the p value is very low so the null hypothesis can be rejected

# to create a boxplot is pretty straightforward
boxplot(Strength~Mortar, ylab = "Strength", xlab = "Mortar")

# QUANTILE PLOT
#qqplot(Mortar, Strength, ylab = "Strength", xlab = "Mortar")

# QUANTILE PLOT FOR SMALL SAMPLES
#source("nqqplot.r")