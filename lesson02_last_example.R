rm(list = ls())

df <- read.table("battery.dat", header = T)
attach(df) # so we use headers instead of columns

df.lm <- lm(Response ~ Material * Temperature)

hist(df.lm$res, xlab = "Residuals", ylab = "Frequencies", main = "Histogram")

shapiro.test(df.lm$res)

qqnorm(df.lm$res)
qqline(df.lm$res)

# plot of residual vs run order
plot(RunOrder, df.lm$res, ylab = "Residuals")
# plot of residual vs fitted
plot(df.lm$fit, df.lm$res,
     xlab = "Fitted values",
     ylab = "Residuals")
# If we see an oscillatory beaviour vs the fit values -> that's not good
# comparison between residual and fitted values if correct, no trend should be visible

# Residuals vs Temperature
plot(as.numeric(Temperature), df.lm$res, xlab = "Temperature", ylab = "Residuals")
# Boxplot of the previous, more condensed information
boxplot(df.lm$residuals ~ as.numeric(Temperature))