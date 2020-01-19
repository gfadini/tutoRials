##  LINEAR REGRESSION
# We use the linear model:    y = k*x + m

k <- 0.7
a <- 1.3
sd_val <- 2
n <- 50

x <- sample(40:70, n, rep = T)  # this command computes the n number of variables in the range with repetitions
y <- k*x + a + rnorm(n, sd=sd_val) # we feed the model with the data plus noise to "create" the experimental data

plot(x, y, xlim = c(0, 90), ylim  = c(0, 60))  # we plot the function x -> y with range

## FITTED MODEL
model <- lm(y ~ x) # the lm is the command for the linear model, the simbol ~ indicates linear dependence
# in model we find the parameters defining the intrconnection between x and y
abline(model, col = "red") # ab line thakes a model and represents it in the plot

## REGRESSION LAW
# Differently from the fitted model, it also considers the uncertainty in the data, sigma
newx <- seq(0, 90) # generates a vector of values from 0 to 90 with a 1 step
pre <- predict(
                model,
                newdata = data.frame(x = newx),
                inteval = c("confidence"),
                level = 0.95,
                type = "response"
              )

# !!!! PRE IS NOT CORRECT SOMEHOW, SEE OTHER CODE ATTACHED AT THE END !!!!
pre <- data.frame(pre) # we rewrite the pre varible after putting it in a data frame
# View(pre)
lines(newx, pre$lwr, col = "red", lty =2) # the pre$lwr is an argument to define the lower
lines(newx, pre$upr, col = "red", lty =2) # the pre$lwr is an argument to define the lower


#linear regression
#y= k*x+m

k <- 0.7

a <- 1.3

sd_val <- 2

n <- 50
x <- sample(40:70, n,rep= T) # random distribution o a range. n sample with repertition
y <- k*x +a + rnorm(n,sd=sd_val) #adding virtual noise

plot(x, y,xlim = c(0,90), ylim = c(0,60))
model <- lm(y~x)
#lm stand for linear model
abline(model,col="red")

newx <- seq(0,90)
pre <- predict(
  model,
  newdata = data.frame(x=newx), 
  interval = c("confidence"),
  level = 0.95,
  type = "response"
)

pre <- data.frame(pre)
lines(newx,pre$lwr,col = "orange", lty = 2)
lines(newx,pre$upr,col = "orange", lty = 2)