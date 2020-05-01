# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function that computes the error between the reported and estimated population sizes,
# given a weight parameter 'a' and a bias parameter 'b'
# * a: weight parameter for estimation function
# * b: bias parameter for estimation function
popError <- function(param, popEst, popRep) {
  a <- param[1]
  b <- param[2]
  
  popEst_weighted <- a * popEst + b
  return(sum((popEst_weighted - popRep)^2))
  
}