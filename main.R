# Project: Corona NTL
# Version: 29-04-2020
# Author: Kamiel Verhelst
# 
#
# This is the main project file, which: 
# * Loads the MODIS data
# * ... ? ADD

# Loading libraries
# pkgTest is a helper function to load packages and install packages only when they are not installed yet.
pkgTest <- function(x)
{
  if (x %in% rownames(installed.packages()) == FALSE) {
    install.packages(x, dependencies= TRUE)
  }
  library(x, character.only = TRUE)
}
neededPackages <- c("ggplot2", "strucchange","zoo", "bfast", "raster", "leaflet", "MODISTools", "tsbox", "shinyWidgets", "lubridate")
for (package in neededPackages){pkgTest(package)}

# Loading supporting R-scripts
invisible(sapply(list.files('./R', full.names = T), source))

# Define data and output directories
datdir <- 'data/'
outdir <- 'output/'

# Define coordinages of Kutupalong Camp, Bangladesh (21.1975, 92.1523)
camp_lat <- 21.1975
camp_lon <- 92.1523

# download, convert and write MODIS data, to data directory
downloadConvertWrite(camp_lat, camp_lon, "NDVI_Kutupalong_4km_2010-2020")

# load raster from file
stacks <- list.files(path=datdir, 
                    pattern="\\.tif$", 
                    full.names=TRUE)
names <- list.files(path=datdir, 
                     pattern="\\.csv$", 
                     full.names=TRUE)

# Select and Load VI data from data directory
VI_r <- stack(stacks[3])
names(VI_r) <- as.character(read.csv(names[3])$x)

# Select and Load QA data from data directory
QA_r <- stack(stacks[4])
names(QA_r) <- as.character(read.csv(names[4])$x)

# create mask on pixel reliability flag set all values <0 or >1 NA
m <- QA_r
m[(QA_r < 0 | QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

# mask all values from VI raster NA
VI_m <- mask(VI_r, m, maskvalue=NA, updatevalue=NA)

# Define the capture dates as date objects
dates <- as.Date(names(VI_m), "X%Y.%m.%d")

# Optional: run shiny app
# shiny::runApp('Explorer')

# Create time series in single pixel
# convert pixel 1 to a time series
px <- 100
plot(tspx, main = sprintf('NDVI in pixel %d', px)) # NDVI time series cleaned using the "reliability information"
bfm1 <- bfast(na.approx(tspx))#, response ~ trend + harmon, order = 2, history = c(2010,1), start = c(2016,13), verbose=T) # Note: the first observation in 2019 marks the transition from 'history' to 'monitoring'
plot(bfm1)


# Read bfastmonitor result from file if written before
base_name <- "bfm_Kutupalong_4km_2010-2020_st20165"
if (file.exists(paste0(outdir, base_name, '.tif'))) {
  
  bfmR <- stack(paste0(outdir, base_name, '.tif'))
  names(bfmR) <- as.character(read.csv(paste0(outdir, base_name, '.csv'))$x)
  
} else {
  # Run the bfastmonitor on the complete raster image
  bfmR <- calc(VI_m, bfmRaster)
  names(bfmR) <- c('time of break', 'magnitude of change')

  # write out result
  
  writeRaster(bfmR, paste0(outdir, base_name,".tif"), overwrite=T) #write raster
  write.csv(names(bfmR), file=paste0(outdir, base_name, ".csv"), row.names=F) #write layer names
}

# Remove those pixels from the 'time of break' where the magnitude of change is positive
bfmR_neg <- bfmR
bfmR_neg$time.of.break[bfmR_neg$magnitude.of.change >= 0] <- NA

# Create a dataframe of only pixels in which a break happened, with only data after the break
# THIS CAN TAKE A WHILE
ts.df <- prepareEstimationDF(VI_m, bfmR_neg, pb = TRUE)

# Optional: plot some pixel time series in single plot
# plotPixelSeries(ts.df[,1:5])

# Compute the population estimation 
# get the largest negative change in VI
dVI_min <- min(getValues(bfmR_neg$magnitude.of.change))
stableMean <- calc(VI_m, calcStableMean)

# initialize results vector
popEst <- numeric(length(dates))
for (i in 1:nrow(ts.df)) { # loop through time
  
  sum <- 0
  
  for (j in 2:ncol(ts.df[i,2:ncol(ts.df)])) { # loop over pixels
    
    if (!is.na(ts.df[i,j])) {
      colname <- names(ts.df)[j]
      px <- as.numeric(substr(colname, 3, nchar(colname)))
      val <- ts.df[i,j] - values(stableMean)[px]
      sum <- sum + val
    }
    
  }
  popEst[i] <- sum / dVI_min
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

# Plot result on map
layer = bfmR$time.of.break
colbin <- colorBin(palette = "RdYlGn", domain = c(min(values(layer), na.rm=T), max(values(layer), na.rm=T)), na.color = NA, pretty=T, bins=9)
leaflet(data = layer) %>%
  addProviderTiles(providers$OpenStreetMap) %>% # Add basemap
  addScaleBar(position = "topright") %>% # Add scalebar
  addRasterImage(layer, opacity=0.7, colors = colbin) %>% # Add the raster layer
  setView(lng=camp_lon, lat=camp_lat, zoom=13) %>%
  addLegend(pal = colbin,
            values = values(layer),
            title = "BFastMonitor Time of Break",
            position = "bottomright") # Add a legend

