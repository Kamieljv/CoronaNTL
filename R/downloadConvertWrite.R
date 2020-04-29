# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst

# Function to download MODIS data, convert it to a raster and write it to a file
# * lat: latitude of site, decimal degrees
# * lon: longitude of site, decimal degrees
# * base_name: base file name for writing the data
# * km_lr: kilometers left and right of site coordinates to download data for
# * km_ab: kilometers above and below site coordinates to download data for
# * start: start date for MODIS data [string, Y-m-d]
# * end: end date for MODIS data [string, Y-m-d]
timeser <- function(lat, lon, base_name, km_lr = 5, km_ab = 5, start = "2010-01-01", end = "2020-01-01") {

  # Download Vegetation Index and Quality data
  VI <- mt_subset(product = "MOD13Q1",
                  lat = lat,
                  lon = lon,
                  band = '250m_16_days_NDVI',
                  start = start,
                  end = end,
                  km_lr = km_lr,
                  km_ab = km_ab,
                  progress = TRUE)
  QA <- mt_subset(product = "MOD13Q1",
                  lat = lat,
                  lon = lon,
                  band = "250m_16_days_pixel_reliability",
                  start = start,
                  end = end,
                  km_lr = km_lr,
                  km_ab = km_ab,
                  progress = TRUE)
  
  
  # convert df to raster
  VI_r <- mt_to_raster(df = VI)
  QA_r <- mt_to_raster(df = QA)
  
  ## Vegetation indices
  writeRaster(VI_r, paste0(datdir, base_name, ".tif"), overwrite=T) #write raster
  write.csv(names(VI_r), file=paste0(datdir, base_name, ".csv"), row.names=F) #write layer names
  
  ## Quality Assessment
  writeRaster(QA_r, paste0(datdir, paste0(base_name, "_QA"), ".tif"), overwrite=T) #write raster
  write.csv(names(QA_r), file=paste0(datdir, base_name, ".csv"), row.names=F) #write layer names
}