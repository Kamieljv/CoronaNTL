# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function to create a dataframe of only pixels in which a break 
# happened, with only data after the break
# * raster: RasterObject with date-coded layers
# * bfm: RasterObject returned by bfast_monitor()
# * format: date format with which the layer is date-coded
# * pb: whether to print a progess bar [boolean]
prepareEstimationDF <- function (raster, bfm, format = "X%Y.%m.%d", pb = FALSE) {
  
  # Create time-series dataframe of right size with dummy data
  # The column values will be removed layer
  if (pb) {
    ts.df <- ts_df(timeser(as.vector(raster[1]), as.Date(names(raster), format)))
  }
  
  prog <- txtProgressBar(min = 1, max = ncell(raster), style = 3) # define progressbar
  for (px in 1:ncell(raster)) { # loop through raster's time layers
    
    if (!is.na(bfm$time.of.break[px])) { # check if a breakpoint occured on this pixel
      # define time series, interpolate NA values with na.approx
      tser <- na.approx(timeser(as.vector(raster[px]), as.Date(names(raster), format)))
      # add column to the dataframe with only values after breakpoint
      ts.df[sprintf("px%d", px)] <- ts.union(tser, window(tser, start = bfm$time.of.break[px]))[,2]
    }
    
    if (pb) {
      setTxtProgressBar(prog, px) #step progress bar
    }
  }
  
  # remove dummy column (2) from dataframe
  return (subset(ts.df, select = -c(2)))

}


