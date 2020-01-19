# 2**(5-1)
# I = ABCDE --> E = ABCD
rm(list = ls())
lvl = c(-1,1)
df <- expand.grid(A = lvl, B = lvl, C = lvl, D = lvl)
attach(df)
df$E <- A*B*C*D # we reduce the degree of freedom, we automatically select this way a factorial plan
detach(df)

df$A <- as.factor(df$A)
df$B <- as.factor(df$B)
df$C <- as.factor(df$C)
df$D <- as.factor(df$D)
df$E <- as.factor(df$E)

df$Yield <- c(8,9,34,52,16,22,45,60,6,10,30,50,15,21,44,63) # not repeated factional factorial plan

df.lm <- lm(Yield ~ A*B*C*D*E, data = df)

n = length(df.lm$effects)
effects <- as.vector(df.lm$effects[2:n])
qn <- qqnorm(effects, datax = T, ylim = c(-70, 30))
text(qn$x, qn$y, lab = names(df.lm$effects[2:n]), pos = 4)
qqline(effects, datax = T)

df.lm2 <- lm(Yield  ~ A*B + C, data = df)
anova(df.lm2)
