# UNREPLICATED FACTORIAL DESIGN
# A = temperature
# B = pressure
# C = % formaldeide
# D = stirring rate
# with enough data, we can have a linear model if the repetitions are enough, in htis case N = 1
# to deal with this, we need to simplify the model, dropping some factors, reducing the dimension of the problem.
# for 3 factors, for example, the cube is squeezed onto a single plane, the one of the most significant parameters
# run an additional test able to discard neglegible factors with the Daniel test -> eliminate points close to the
# quantile-quantile line

dat <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)

lvl <- c("-", "+")
df <- data.frame(expand.grid(A = lvl, B = lvl, C = lvl, D = lvl), R = dat) 

df.lm <- lm(R ~ A * B * C * D, dat = df)

n <- length (df.lm$effects)

effects <- as.vector(df.lm$effects)[2:18] 
qn <- qqnorm(effects, datax = T)
text(qn$x, qn$y,
     lab = names(df.lm$effects)[2:n],
     pos = 1)
qqline(effects, datax = T)
# from his we can simplify the model by removing B

# SIMPLIFIED MODEL
df.lm2 <- lm(R ~ A * C * D, data = df)
anova(df.lm2)
# From the values of p we can be sure that A:C:D, C:D are not influencing the yield