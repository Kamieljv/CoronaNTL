# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function to plot a masking example
# * lat: latitude of site, decimal degrees
# * lon: longitude of site, decimal degrees
# * base_name: base file name for writing the data
# * km_lr: kilometers left and right of site coordinates to download data for
# * km_ab: kilometers above and below site coordinates to download data for
# * start: start date for MODIS data [string, Y-m-d]
# * end: end date for MODIS data [string, Y-m-d]
plotMaskingExample <- function(VI_r, QA_r, VI_m, t = 9, write = FALSE, outdir = 'output/') {
  
  if (write) {
    pdf(file=paste0(outdir, "MaskingExample.pdf"))
  }
  
  # Initialize plotting area
  par(mfrow = c(1, 3))
  
  # define date string for chosen time point
  date <- names(QA_r)[t]
  plot(QA_r, t, main=sprintf("QA mask (%s)", substr(date, 2, nchar(date))))
  plot(VI_r, t, zlim=c(0,1), main=sprintf("NDVI (%s)", substr(date, 2, nchar(date))))
  plot(VI_m, t, zlim=c(0,1), main=sprintf("Masked NDVI (%s)", substr(date, 2, nchar(date))))
  
  if (write) {
    dev.off()
  }
  
}