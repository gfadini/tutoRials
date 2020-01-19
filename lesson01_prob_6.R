## PROBLEM 6 - INDUSTRIAL PLANT
n_0 = 12
ss_0 = 14.5
n_1 = 10
ss_1 = 10.8
alpha = 0.05

# This is an analysis of vairance, we need a F test
# F_0 > F_alpha, a-1, b-1
# the first comes from a Snedecor distributionm the second from the alpha value

F0 = ss_0 / ss_1
F_alpha = qf (alpha, n_0 - 1, n_1 - 1, lower.tail = F)
# the kast two parameters are the dof of numerator and denominator respectively,
# since the F0 < F_alpha we canot reject