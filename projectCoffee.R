rm(list = ls())
#dev.off(dev.list()["RStudio"]) # closes all current plots
setwd("~/Desktop/R/R")

library(MASS) # for Box Cox

coffee <- read.table("Hometable.dat", header = T) # PREVIOUSLY "coffe_data.dat"
attach(coffee)

# COMPLETE MODEL
coffee.lm_1 = lm(Time ~ WaterTemp * Coffee * Flame, data = coffee)
anova(coffee.lm_1)
n <- length (coffee.lm_1$effects)
effects <- as.vector(coffee.lm_1$effects)[2:n]
qn <- qqnorm(effects, datax = T)
text(qn$x, qn$y,
     lab = names(coffee.lm_1$effects)[2:n],
     pos = 1)
qqline(effects, datax = T)

# SIMPLIFIED MODEL
coffee.lm_2 = lm(Time ~ WaterTemp * Flame + WaterTemp * Coffee + Flame * Coffee, data = coffee)
anova(coffee.lm_2)
n <- length (coffee.lm_2$effects)
effects <- as.vector(coffee.lm_2$effects)[2:n]
qn <- qqnorm(effects, datax = T)
text(qn$x, qn$y,
     lab = names(coffee.lm_2$effects)[2:n],
     pos = 1)
qqline(effects, datax = T)


# INTERACTION PLOT
interaction.plot(Flame, Coffee, Time)
interaction.plot(Flame, WaterTemp, Time)


# PLOT OF THE RESIDUAL
hist(coffee.lm_2$res, xlab = "Residuals", ylab = "Frequencies", main = "Histogram")
shapiro.test(coffee.lm_2$res)
qqnorm(coffee.lm_2$res)
qqline(coffee.lm_2$res)

# PLOT VS RUN ORDER
plot(RunOrder, coffee.lm_2$res, ylab = "Residuals")
# PLOT RESIDUALS VS FITTED
plot(coffee.lm_2$fit, coffee.lm_2$res,
     xlab = "Fitted values",
     ylab = "Residuals")



# RESIDUALS VS WATER TEMPERATURE
plot(as.numeric(WaterTemp), coffee.lm_2$res, xlab = "Water Temperature", ylab = "Residuals")
boxplot(coffee.lm_2$residuals ~ as.numeric(WaterTemp), xlab = "Water Temperature", ylab = "Residuals")
# RESIDUALS VS FLAME
plot(as.numeric(Flame), coffee.lm_2$res, xlab = "Flame level", ylab = "Residuals")
boxplot(coffee.lm_2$residuals ~ as.numeric(Flame), xlab = "Flame level", ylab = "Residuals")

# TUKEY TEST
coffee$Flame <- as.factor(Flame)
coffee$WaterTemp <- as.factor(WaterTemp)
coffee$Coffee <- as.factor(Coffee)

for (t in levels(coffee$WaterTemp))
{
        cat("Temperature = ", as.numeric(t), "\n")
        print(
                TukeyHSD(
                        aov(
                                Time ~ Flame,
                                data = Coffee[WaterTemp == as.numeric(t)]), "Flame"
                )
        )
}

library(lattice)
WT <- seq(15, 50, 5)
CF <- seq(6, 7, 0.1)
FL <- seq(1, 3, 0.1)
g <- expand.grid(WaterTemp = WT, Coffee = CF, Flame = FL) # This creates a grid of the corresponding values
# now we use the command predict
g$Time <- predict(coffee.lm_2, g)

contourplot(Time ~ WaterTemp * Flame,
            data = g[g$Coffee == 6, ],
            cuts = 10,
            region = T,
            xlab = "Water Temp",
            ylab = "Flame",
            main = "Coffee low"
)

contourplot(Time ~ WaterTemp * Flame,
            data = g[g$Coffee == 7, ],
            cuts = 10,
            region = T,
            xlab = "Water Temp",
            ylab = "Flame",
            main = "Coffee high"
)

wireframe(Time ~ WaterTemp * Flame,
          data = g[g$Coffee == 6, ]
          )

wireframe(Time ~ WaterTemp * Flame,
          data = g[g$Coffee == 7, ]
)


library(rgl) # library to 3d plot

g <- g[g$Coffee == 3, ]
z <- matrix(data = g$Time, length(WT), length(FL))
colorTable <- matrix("green", length(WT), length(FL))
colorTable[which(z > 160, a = T)] <- "orange" # changing the colors of the matrix elem with conditional
colorTable[which(z > 200, a = T)] <- "red" # changing the colors of the matrix elem with conditional

open3d()
surface3d(WT, FL, z, colorTable)
aspect3d(1,1,1)
axes3d()
title3d(main = "Low Coffee",
        xlab = "Water Temp",
        ylab = "Flame",
        zlab = "Time")


g <- g[g$Coffee == 4, ]
z <- matrix(data = g$Time, length(WT), length(FL))
colorTable <- matrix("green", length(WT), length(FL))
colorTable[which(z > 160, a = T)] <- "orange" # changing the colors of the matrix elem with conditional
colorTable[which(z > 200, a = T)] <- "red" # changing the colors of the matrix elem with conditional

open3d()
surface3d(WT, FL, z, colorTable)
aspect3d(1,1,1)
axes3d()
title3d(main = "High Coffee",
        xlab = "Water Temp",
        ylab = "Flame",
        zlab = "Time")

# BOX COX

model <- lm(Time ~ WaterTemp * Coffee * Flame)

boxcox <- boxcox(Time ~ WaterTemp * Coffee * Flame)

lambda <- boxcox$x[which.max(boxcox$y)]

powerTransformation <- function(Time, lambda1,lambda2 = NULL, method = "boxcox")
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


mnew <- lm((Time ^ lambda - 1) / lambda  ~ Coffee + WaterTemp + Flame)
# 
# 
# op <- par(pty = "s", mfrow = c(1,2)) # like matlab subplot
# qqnorm(m$residuals); qqline(m$residuals)
# qqnorm(mnew$residuals); qqline(mnew$residuals)
# par(op)
