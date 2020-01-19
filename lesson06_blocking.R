# BLOCKING AND ALIASING
rm(list = ls())
# we consider a 2**2 factorial plan, we cannot do teh experiements all together, we create a set of blocks
# in te case we have an operatuve condition that forces us to divide the sets, we can build the yates table,
# I   A   B   AB    BL
# +   -   -   +     1
# +   +   -   -     2
# +   -   +   -     2
# +   +   +   +     1
#
# -/+     +/+
# (*)    (^)
# b - - - ab
# |       |
# |       |
# |       |
# 1 - - - a
# (^)    (*)
# -/-     +/-
# B1 (*)
# B2 (^)
# B1 : A = 1/2 * [/pm (1) + a - b /pm ab]
# B2 : B = 1/2 * [-1 /pm a /pm b + ab]
# AB = 1/2 * []
# blocking we cannot detect interactions can't say if the interaction is due to the blocking or to the innner working between them
lvl <- c(-1, 1)
g <- expand.grid(A = lvl, B = lvl, C = lvl, D = lvl)

data <- c(45, 71, 48, 65, 68, 60, 80, 65, 43, 100, 45, 104, 75, 86, 70, 96)
# let's block the data and see the outcome of teh analysis (pretending that something changed in the experiment)
df <- data.frame(g, Block = g$A*g$B*g$C, R = data) # select only one part with the product sign
df$R[df$Bock == -1] = df$R[df$Block == -1] - 20

df.lm <- lm(R ~ A*B*C*D + Block, d = df)
