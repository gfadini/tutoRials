library(MASS)


set.seed(1) # fix random seed
n <- 100
x <- runif(n,1,5) # random uniform distribution between 1 and 5
y_vals <-x^3 +rnorm(n) # random normal distribution
y <- y_vals - min(y_vals) + 0.01


m <- lm(y ~ x)

bc <- boxcox(y ~ x)

(lambda <- bc$x[which.max(bc$y)])

powerTransformation <- function(y, lambda1,lambda2 = NULL, method = "boxcox")
{
  boxcoxTrans <- function(x, lam1, lam2 = NULL)
  {
    lam2 <- ifelse(is.null(lam2),0,lam2)
    if(lam1 == 0L)
    {
      log(y+lam2)
    } else {
      (((y+lam2)^lam1)-1)/ lam1
    }
  }
  switch(method, boxcox = boxcoxTrans(y,lambda1,lambda2), tukey = y^lambda1)
}


mnew <- lm(powerTransformation(y, lambda) ~ x)

# QQPLOT
op <- par(pty = "s", mfrow = c(1,2)) # like matlab subplot
qqnorm(m$residuals); qqline(m$residuals)
qqnorm(mnew$residuals); qqline(mnew$residuals)
par(op)












