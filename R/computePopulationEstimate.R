# Project: Corona NTL
# Version: 02-05-2020
# Author: Kamiel Verhelst

# Function that computes the population estimate by fitting it to the reported population
# * bfmR: raster output of BFAST Monitor
# * VI_m: masked vegetation index raster
computePopulationEstimate <- function (bfmR, VI_m, stableMean) {
  
  # initialize results vector
  popEst <- numeric(length(dates))
  for (i in 1:nrow(ts.df)) { # loop through time
    
    sum <- 0
    
    for (j in 2:ncol(ts.df[i,2:ncol(ts.df)])) { # loop over pixels
      
      if (!is.na(ts.df[i,j])) {
        colname <- names(ts.df)[j]
        px <- as.numeric(substr(colname, 3, nchar(colname)))
        val <- values(stableMean)[px] - ts.df[i,j]
        sum <- sum + val
      }
      
    }
    popEst[i] <- sum
  }

  popEst_ts <- timeser(popEst, dates)
  
  # load reported population estimates as time series
  popRep_df <- read.csv('data/ReportedRefugeePopulation.csv')[,1:2]
  popRep_df[,1] <- as.Date(popRep_df[,1], "%Y-%m-%d") # convert date column to dates
  popRep_ts <- ts(rep(NA, length(popEst_ts)), start=start(popEst_ts), frequency=frequency(popEst_ts))
  
  for (i in 1:length(popRep_ts)) {
    val <- popRep_df$pop[abs(decimal_date(popRep_df$date) - time(popRep_ts)[i]) < 0.01]
    if (length(val) > 0) {
      popRep_ts[i] <- val
    }
  }
  # interpolate the Reported data
  popRep_ts <- na.approx(popRep_ts, maxgap=10)
  popRep_ts[is.na(popRep_ts)] <- 0
  popRep <- as.numeric(popRep_ts)
  
  # Optimize the weight and bias parameters to fit to reported data
  res <- optim(c(1, 1), fn=popError, popEst=popEst, popRep=popRep)
  a <- res$par[1]
  b <- res$par[2]
  popEst_ts <- timeser(a * popEst + b, dates)
  
  comb_ts <- as.data.frame(time(popEst_ts))
  comb_ts[2] <- popEst_ts
  comb_ts[3] <- popRep_ts
  names(comb_ts) <- c('time', 'popEst', 'popRep')
  
  return (list(comb_ts[150:nrow(comb_ts),], a, b))
}