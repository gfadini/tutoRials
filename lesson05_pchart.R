rm(list = ls())

library(qcc)

data = read.table("p_data.dat", header = T) # we have a variable sample size so we need the p chart
attach(data)

q <- qcc(D, type = "p", sizes = data$size)