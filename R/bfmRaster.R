# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function to compute the bfastmonitor for all pixels in an image
# * pixels: pixels to run the bfm on
bfmRaster = function(pixels)
{
  tspx <- timeser(pixels, dates) # create a timeseries of all pixels
  
  bfm <- bfastmonitor(tspx, response ~ trend + harmon, order = 3, history = c(2010, 1), start = c(2016.5)) # run bfast on all pixels
  
  return(c(bfm$breakpoint, bfm$magnitude)) 
}