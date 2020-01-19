library(pwr)
rm(list = ls())

# power is 1 - type 2 error, assesses the raliablit of the prediction, the reliance depend on the number of points that were collected
# the reliability depends on the OCC curves

n_levels <- c(2, 3, 4, 5, 10, 20, 50, 100)

ty <- 0.7
for (n in n_levels)
{
  if (n == 2) new_plot  = F else new_plot = T
  
  curve(1 - pwr.t.test(n,
                       x,
                       pow = NULL,
                       type = "two.sample")$power,
        0, 6,
        add = new_plot,
        ylim = c(0, 1), xlim = c(0,6), xlab = "d", ylab = "P(Type II error), 1 - PWR")
  tx <- (pwr.t.test(n, d = NULL,
                    pow = 1 - ty,
                    type = "two.sample")$d) # specifies where to put the number
  symbols(x = tx, y = ty, # puts a white patch over the graph to put the number into
          sq = c(0.3),
          add = T,
          inches = F,
          bg = "white",
          fg = NULL)
  text(x = tx, y = ty, lab = n)
  ty <- ty - 0.08
} 
# d is the difference between the distributions, normalized on std, the highest curve is the one with less elements in the samples
# the most powerfule is the lowest, with the highest number ofa samples elements

for (n in n_levels)
{
  if (n == 2) new_plot  = F else new_plot = T
  
  curve(1 - pwr.t.test(n,
                       x,
                       pow = NULL,
                       type = "one.sample")$power,
        0, 6,
        add = new_plot,
        ylim = c(0, 1), xlim = c(0,6), xlab = "d", ylab = "P(Type II error), 1 - PWR")
  tx <- (pwr.t.test(n, d = NULL,
                    pow = 1 - ty,
                    type = "one.sample")$d) # specifies the kind of test to perform. This command overall where to put the number
  symbols(x = tx, y = ty, # puts a white patch over the graph to put the number into
          sq = c(0.3),
          add = T,
          inches = F,
          bg = "white",
          fg = NULL)
  text(x = tx, y = ty, lab = n)
  ty <- ty - 0.08
} 