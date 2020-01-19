## 2 * 2 FP with a mean value
lvl <- c(-1, 1)
df <- expand.grid(A=lvl, B=lvl)
df[5:9, ] = 0
# we expand the vector by adding the
df$Y <- c(
  39.3, 40.9, 40.0, 41.5, 40.3, 40.5, 40.7, 40.2, 40.6
)

df.lm_1 = lm(Y ~ A*B, data = df)
anova(df.lm_1)

df.lm_2 = lm(Y ~ A*B + poly(B,2), data = df)
anova(df.lm_2)

df.lm_3 = lm(Y ~ A*B + poly(A,2), data = df)
anova(df.lm_3)

df.lm_4 = lm(Y ~ A + B, data = df)
anova(df.lm_4)
# this model is better, it's more close to the expected values, 
# In any case we must always consider higher order polynomial functions and central values
# we usually inclde th ecentral value because that point knowledge is lost with the linear model, we simplify the center
# if we have no central point the fit takes place only in the corners