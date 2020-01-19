rm(list = ls())
df <- read.table("battery.dat", header = T)
df$Temp <- as.factor(df$Temperature)
attach(df)

df.lm <- lm(Response ~ Material * Temperature)
# see if the residuals are distributed as gaussians with Shapiro-test
hist(df.lm$residuals)
shapiro.test(df.lm$residuals)
boxplot(df.lm$res~as.numeric(Temperature))
boxplot(df.lm$res~as.factor(Temperature))
anova(df.lm)

# we do a second linear model considering that the temperature is not a significant parameter

df.lm <- lm(Response ~ Material + Temperature)
# see if the residuals are distributed as gaussians with Shapiro-test
hist(df.lm$residuals)
shapiro.test(df.lm$residuals)
boxplot(df.lm$res~as.numeric(Temperature))
boxplot(df.lm$res~as.factor(Temperature))
anova(df.lm)

interaction.plot(Temperature, Material, Response) # how the factors change modifying the parameters

## PERFORM A TUKEY TEST
df$Mat <- as.factor(Material)

for (t in levels(df$Temp))
{
  cat("Temperature = ", as.numeric(t), "\n")
    print(
      TukeyHSD(
        aov(
          Response~Mat,
          data = df[Temperature == as.numeric(t), ]), "Mat"
        )
      )
}

# TUKEY performs anova considering one pair of materials at a time, paired T test for each factor combination
# this analysis is performed for each level of temperature, the Tukey increases the detail of comparison
# at 15 degrees the materials do not perform differently, the p value is high:

        # Temperature =  15 
        # Tukey multiple comparisons of means
        # 95% family-wise confidence level
        # 
        # Fit: aov(formula = Response ~ Mat, data = df[Temperature == as.numeric(t), ])
        # 
        # $Mat
        # diff     lwr    upr     p adj
        # 2-1  21.00 -45.344 87.344 0.6633090
        # 3-1   9.25 -57.094 75.594 0.9205830
        # 3-2 -11.75 -78.094 54.594 0.8756855

# in the second temperature value, the values are significantly different:

        # Temperature =  70 
        # Tukey multiple comparisons of means
        # 95% family-wise confidence level
        # 
        # Fit: aov(formula = Response ~ Mat, data = df[Temperature == as.numeric(t), ])
        # 
        # $Mat
        # diff       lwr       upr     p adj
        # 2-1 62.5  22.59911 102.40089 0.0045670
        # 3-1 88.5  48.59911 128.40089 0.0004209
        # 3-2 26.0 -13.90089  65.90089 0.2177840

# This can also be easily seen from the plot of the interaction plot

## REGRESSION MODEL
attach(df)
df.lmr <- lm(formula = Response ~ (Mat*I(Temperature) + I(Temperature^2)*Mat))

sq = seq(15, 125, 10)
plot(Response ~ Temperature, main = "Regression")

for(i in c(1,2,3))
{
  lines(
    sq,
    predict(
      df.lmr,
      data.frame(
        Mat = levels(df$Mat)[i],
        Temperature = sq)),
    col = i)
}

grid()
