# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function to calculate the stable mean for the first few years in a time series
# Current nyears is set at 5
# * pixels: pixels to run the bfm on
calcStableMean = function(pixels)
{
  nyears = 5
  tspx <- timeser(pixels, dates) # create a timeseries of all pixels
  tspx_window <- window(tspx, start=start(tspx), end=start(tspx)+nyears)
  stableMean <- mean(tspx_window, na.rm=T)
  
  return(stableMean) 
}