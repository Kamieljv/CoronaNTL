# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst


# Function to create time series object
# * val_array: data array for one single pixel (length is number of time steps)
# * time_array: array with dates at which raster data is recorded (same length as val_array)
timeser <- function(val_array, time_array) {
  
  z <- zoo(val_array, time_array) # create zoo object
  yr <- as.numeric(format(time(z), "%Y")) # extract the year numbers
  jul <- as.numeric(format(time(z), "%j")) # extract the day numbers (1-365)
  delta <- min(unlist(tapply(jul, yr, diff))) # calculate minimum time difference (days) between observations
  zz <- aggregate(z, yr + (jul - 1) / delta / 23) # aggregate into decimal year timestamps
  (tso <- as.ts(zz)) # convert into timeseries object
  
  return(tso)
}