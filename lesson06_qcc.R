library(qcc)
rm(list = ls())

data("pistonrings") # produced by forging, each set of 25 sample groups with 5 piston rings, with some trial
attach(pistonrings)

d1 <- qcc.groups(data = diameter[trial],
                 sample = sample[trial]) # we grouped the data for the tuning

# We use the X bar chart in order to check if everything is in control (easiest test possible)
q1 <- qcc(data = d1, type = "R") # range: the variation of the range of the data sample is given out, for more samples,
                                 # if they're more than 10, I can use the S type chart
q2 <- qcc(data = d1, type = "xbar", center = 74) # the target value (mean expected) is the center 74, average of the samples

# once the range is set, we can take the other data, not containing the trial flag
d2 <- qcc.groups(data = diameter[!trial],
                 sample = sample[!trial])

q3 <- qcc(data = d1, type = "R", newdata = d2)
q4 <- qcc(data = d1, type = "xbar", center = 74, newdata = d2)

# THE PROCESS IS IN CONTROL FOR THE R-CHART, BUT NOT FOR THE X-BAR CHART.
# CASES:
# X-CHART IS FINE
#   R CHART IS FINE -> OK FULL CONTROL
#   R CHART IS NOT -> GREAT VARIABILITY, RANDOMIC
# X-CHAR IS NOT FINE
#   R CHART IS FINE -> NARROW VARIABILITY BUT BIASED
#   R CHAR IS NOT FINE -> NOT GOOD AT ALL

process.capability(q2, spec.limits = c(74 - 0.05, 74 + 0.05)) # we set the specification limits and asses hoe good the process is

q5 <- qcc(data = d1, type = "S", newdata = d2) # meaningless, data is not much, too few samples, very similar to R chart

ewma(data = d1, newdata = d2,
     center = 74, lambda = 0.1) # we find the critcial element out of control, more sensible to smaller variation wrt the nominal value < 1.5 sigma