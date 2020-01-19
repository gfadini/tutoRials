# PROCEDURE
# CALIBRATION -> OUTLYERS -> REMOVE -> TUNING -> QCC -> RETUNE IF NECESSARY
rm (list = ls())
library(qcc)

data(orangejuice) # this is included in the Rstudio library, check leakage
attach(orangejuice)

# we have an np chart, y/n condition, fixed control group

q <- qcc(D[trial], type = "np",
         sizes = size[trial]) # the field is calle dtrial as we chose the trail, the dataset contains data and tuning
# this is the tunig phase, the two points are critical but may be left for the moment
# the introduction of a different object to produce led to the retuning of the machine in the red dots
# in those two points we know something happened

# we remove the two red points from the data
tuning <- setdiff(which(trial), q$violations$beyond.limits) # we use the violation to select outrangers
# we use the set diff to perform the difference between the teo set of indices

# we apply the tuning on the production data, aka those that are not trial!
q <- qcc(D[tuning],
         type = "np",
         sizes = size[tuning],
         newdata = D[!trial],
         newsizes = size[!trial]
         )
# we obtain the plot of the calibration phase and the plot of the production one, with the violations
# the plots change in colour and become yellow, the production process is not in control anymore
# at sample 33 the mahine was modified in the tuning, so the configuration is changed, that would have
# required a new tuning of the machine. We run a new caliration phase

q <- qcc(D[sample > 33],
         type = "np",
         sizes = size[sample > 33]
         )