rm(list = ls()) # clears the workspace
## we are going to create the function that randomizes the run of experiments

measure <- c(224, 140, 309, 290, 143, 132, 221, 109, 130, 115, 206, 126)

Y1 <-  measure + round(runif(12, -15, 19))
Y2 <- measure + round(runif(12, -21, 18))
Y3 <- measure + round(runif(12, -15, 15))

Y <- c(Y1, Y2, Y3)

prepare <- function(factors = list(A = 1:2, B = 1:2, C = 1:2),
                    k = 3,
                    runorder = TRUE,
                    file = ""
                    )
{
  factors$Rep = 1 : k # colum with the repetition number
  df.g = expand.grid(factors)
  n = nrow(df.g)
  key = data.frame(r = runif(n, 0, 1), s = 1:n)
  key = key[order(key$r),]$s # takes the column of rand numb and sorts them in increasing order and uses the sorting values
  
  df.h = data.frame(
                      RunOrder = key,
                      StdOrder = 1:n
                   )
  
  df.t = data.frame(Time = Y) # else Y to forge or rep(NA, n) to initialize
  df = cbind(df.h, df.g, df.t) # merges different data frames into one
  
  if(runorder)
  {
    df = df [order(df$RunOrder), ] # if the flag of RunOrder is true, the vector is reordered
  }
  
  if(file != "")
  {
    # df <- c(df.g, df.t)
    write.table(df, file, col.names = T, row.names = F, quote = F, sep = "\t")
  }
  
  return(df)
}

df <- prepare(list(WaterTemp = c(14, 50),
                   Coffee = c(9, 11),
                   Flame = c(1, 2, 3)),
              k = 3,
              runorder = F,
              file = "myTablenew.dat")