## PROBLEM 3 - TIP HARDNESS
hardness <- read.table("hardness.dat", header = T)
attach(hardness)
hardness

t.test(Tip1, Tip2, var.equal = T, conf.level = 0.05, paired = T) # we consider that the tool have the same variance
# p value is high so the tips are the same, so the differences are probably related to measurement issues only