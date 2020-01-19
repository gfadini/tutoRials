## PROBLEM 5 - CALIPER
caliper <- read.table("bearing.dat", header = T)
attach(caliper)
caliper

t.test(Caliper1, Caliper2, conf.level = 0.05, paired = T)
# the p value is pretty  high
# since the guy is taking the same measure, then the test MUST BE PAIRED, the mmeasurements are influenced the same way
# the pairing of the instruments reduces the one of the inspectors

# to create a boxplot is pretty straightforward
boxplot(Caliper1, Caliper2, ylab = "Measure", xlab = "Caliper")