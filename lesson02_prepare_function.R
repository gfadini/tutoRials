rm(list = ls()) # clears the worspace
## we are going to create the function that randomizes the run of experiments

prepare <- function(factors = list(A = 1:3, B = 1:3),
                    k = 4,
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
  
  df.t = data.frame(Yield=rep(NA, n))
  df = cbind(df.h, df.g, df.t) # merges different data frames into one
  
  if(runorder)
  {
    df = df [order(df$RunOrder), ] # if the flag of RunOrder is true, the vector is reordered
  }
  
  if(file != "")
  {
    write.table(df, file, col.names = T, row.names = F, quote = F, sep = "\t")
  }
  
  return(df)
}

df <- prepare(list(Temp = c(15, 70, 125),
                   Mat = c("1", "2", "3")),
              k = 4,
              runorder = F,
              file = "myTable.dat")
