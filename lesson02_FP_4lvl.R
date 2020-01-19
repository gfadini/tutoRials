## 4 FACTORS MODEL: A, B, C, D --> T
# we do a 2**4 unreplicated FP, with 16

# we define the number o levels
lvl <- c("-", "+")
df <- expand.grid(A = lvl, B = lvl, C = lvl, D = lvl) # this command creates the Yates table, providing the paramenters

# we write the outcome of the 16 experiments, with the command $ we attach a name to a column
df$Y <- c(
          1.68, 1.98, 4.98, 5.70,
          3.24, 3.44, 9.97, 9.07,
          2.07, 2.44, 7.77, 9.43,
          4.09, 4.53, 11.75, 16.30
         )

# COMPLETE MODEL
df.lm <- lm(Y ~ A * B * C * D, data = df) # with this command we add a linear model to the data, complete interactions
# we compute now the umber of the effect, all teh combinations among factors
n <- length (df.lm$effects) # name and possible interaction
# we need to identify which interactions are relevant and which not, to do that we use the Daniels method
# we plot the QQplot of the effects, we remove the marginal influence on the process, the data closer to the curve
effects <- as.vector(df.lm$effects)[2:n] # we save the effects form the second to the last

qn <- qqnorm(effects, datax = T) # we compute the distribution of quantiles of the effects
text(qn$x, qn$y,
     lab = names(df.lm$effects)[2:n],
     pos = 1)
qqline(effects, datax = T)
# from his we can simplify the model by removing the useless parameters

# SIMPLIFIED MODEL
df.lm2 <- lm(Y ~ A + B * C * D, data = df) # the interaction between A and the other variables is removed
anova(df.lm2)
# From the values of p we can be sure that A is not influencing the yield and so does B:C:D

df.lm3 <- lm(Y ~ B * D + B * C, data = df)
anova(df.lm3)
# now all the factors are relevant, we can now consider the residuals of the model