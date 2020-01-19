rm(list = ls())

library(qcc)

data(circuit)
attach(circuit)

q <- qcc(x[trial], type = "c", sizes = size[trial])

# 6 and 20 are outlyers, we must remove the wrong indeces
tuning <- setdiff(which(trial), q$violations$beyond.limits)
q <- qcc(x[tuning],
         type = "c",
         sizes = size[tuning],
         newdata = x[!trial],
         newsizes = size[!trial]
)
