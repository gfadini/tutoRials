rm(list = ls())

df <- read.table("cutting.dat", header = T)
attach(df)

df.lm <- lm(Response ~ Angle * Speed * I(Angle^2) * I (Speed^2))

anova(df.lm)

# ISOCURVES
library(lattice)
Ar <- seq(15, 25, 0.1)
Sr <- seq(125, 175, 1)
g <- expand.grid(Angle = Ar, Speed = Sr) # This creates a grid of the corresponding values
# now we use the command predict
g$Life <- predict(df.lm,
                  data.frame(Angle = g$Angle,
                             Speed = g$Speed)
                  )# works on the linear model (builds the intermediate values throgh regression)
wireframe(Life~Angle * Speed,
          data = g,
          shade = F,
          light.source = c(10, 0, 10),
          drape = F,
          scales = list(arrows = F)
          )
contourplot(Life ~ Angle * Speed,
             data = g,
             cuts = 10,
             region = T
             )
#

library(rgl) # library to 3d plot
z <- matrix(data = g$Life, length(Ar), length(Sr))
colorTable <- matrix("green", length(Ar), length(Sr))
colorTable[which(z < 4, a = T)] <- "orange" # changing the colors of the matrix elem with conditional
colorTable[which(z < 1, a = T)] <- "red" # changing the colors of the matrix elem with conditional


open3d()
surface3d(Ar, Sr, z, colorTable)
aspect3d(1,1,1)
axes3d()
title3d(main = "Surf",
        xlab = "Angle",
        ylab = "Speed",
        zlab = "Life")

library(qcc) # library fo quality control chart
