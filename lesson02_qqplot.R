## QQPLOT
# we define a gaussian distribution and see its distribution on such a plot
m  <- 15 # mean
sd <- 2  # standard deviation
n  <- 50 # samples

y  <- round(rnorm(n, m, sd), 2)    # random sampling for a normal distribution we round to see better the data

y.s <- sort(y)     # we assign the sorted values in indreasing order to a field of the struct
y.p <- ppoints(n)  # follows (1:n - 3/8)/(n + 1 - 2 * 3/8)
#  with a value of 3/8 we consider a gaussian, other values are possible (1:n - alpha)/(n + 1 - 2 * alpha)

t.q <- qnorm(y.p) # theoretical, not necessary, already embedded in qqplot

# PLOTTING
  qqnorm(
          y, xlab ="Theoretical quantiles", ylab = "Data quantiles", main = "Q-Q plot"
        )           # function tht calculates the qqplot for the comparison, with labeling
  qqline(y)
  grid()
  # from the plot the points that are close to the line follow the gaussian distribution, the others don't
  # Increasing the number of dots we see that the number of deviant point is far less than the confrom ones