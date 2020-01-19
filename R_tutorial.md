
Clean the console with with the sweep icon
[d|p|q|r] 		-> they are percentile quatile, ... of one distribution
[norm|t|chisq|f] 	-> are the distribuiton of data

QUANTILE
PERCENTILE

split.screen(column, rows)		-> multiplot
curve(pt(x,10))		-> percentile of a t sudent dist with 11 samples, (lower case from right part to left part?)

lower.tail = F		-> right to left values, integral area definition from +infnty
the command to fine the quantile is q, from a gaussian distribuiton (Z test), so qnorm()
qnorm() -> decides automatically form the lowertail ! We need to put lower.tail = F
days<-c() -> creates a vector

we use the command t.test(vector, parameters) instead, in the examthe complete procedure may be required

LOAD A FILE CONTENT AS A TABLE
hardness <- read.table("hardness.dat", header = T)
attach(hardness)
hardness


COMBINE
t.test(Strength~Mortar)
with tilde we combine in some analysis
