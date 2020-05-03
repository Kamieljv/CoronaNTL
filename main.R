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
neededPackages <- c("ggplot2", "strucchange","zoo", "bfast", "ggpubr", "raster", "leaflet", "MODISTools", "tsbox", "shinyWidgets", "lubridate", "sf")
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
QA_r <- stack(stacks[5])
names(QA_r) <- as.character(read.csv(names[5])$x)

# create mask on pixel reliability flag set all values <0 or >1 NA
m <- QA_r
m[(QA_r < 0 | QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

# mask all values from VI raster NA
VI_m <- mask(VI_r, m, maskvalue=NA, updatevalue=NA)

# Optional: plot masking example
plotMaskingExample(VI_r, QA_r, VI_m)

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

# Remove those pixels from the 'time of break' where the magnitude of change is positive
bfmR_neg <- bfmR
bfmR_neg$time.of.break[bfmR_neg$magnitude.of.change >= 0] <- NA

# Create a dataframe of only pixels in which a break happened, with only data after the break
# THIS CAN TAKE A WHILE
ts.df <- prepareEstimationDF(VI_m, bfmR_neg, pb = TRUE)

# Optional: plot some pixel time series in single plot
# plotPixelSeries(ts.df[,1:5])

# get the largest negative change in VI
stableMean <- calc(VI_m, calcStableMean)

# Compute the population estimation 
res <- computePopulationEstimate(bfmR_neg, VI_m, stableMean)
comb_ts <- res[[1]]
a <- res[[2]]
b <- res[[3]]

# Plot the two population graphs
p1 <- ggplot(comb_ts, aes(x=time)) +
        geom_line(aes(y = popEst, color = "pop. Est.")) + 
        geom_line(aes(y = popRep, color = "pop. Rep.")) +
        scale_color_manual("", values = c("pop. Est."="red", "pop. Rep."="blue")) +
        labs(title="Estimated and Reported Population of Kutupalong Camp", y="Population", x="Time")+
        theme(legend.position = c(0.92, 0.9)) + 
        coord_cartesian(xlim = c(2016.5,2020))
p1 

comb_ts$err <- comb_ts$popEst - comb_ts$popRep

ggplot(comb_ts, aes(x=time, y = err)) +
  geom_line(color = "black") +
  geom_hline(yintercept = 0) +
  labs(title="Error in population estimate w.r.t. reported population", y="Error (individuals)", x="Time")


# Calculate a pixel-based population estimate
NDVI <- VI_m$X2018.12.19 # Get the NDVI values for the date we have detailed population data for
NDVI_m <- mask(NDVI, bfmR_neg$time.of.break, maskvalue = NA, updatevalue = NA)
stableMean_m <- mask(stableMean, bfmR_neg$time.of.break, maskvalue = NA, updatevalue = NA)
nbreak <- length(bfmR_neg$time.of.break[bfmR_neg$time.of.break < decimal_date(dates[207])]) # define number of pixels with breaks at 2019-04-23 (time point 215)
popEst_map <- overlay(NDVI_m,
                      stableMean_m,
                      fun=function(ndvi, mu){return(a * (mu - ndvi) + b / nbreak)})
# write to disk
writeRaster(popEst_map, paste0(outdir, "populationEstimates_20181212.tif"), overwrite=T) #write raster

# load shapefile
camps_poly <- st_read('data/campshp/CoxBazar_Rohingya_cropped_20181212.shp')

estPop_perpoly <- extract(popEst_map, camps_poly, fun=sum, na.rm=TRUE, df=TRUE)
camps_poly$estPop <- estPop_perpoly$layer

# Plot the two population graphs
p2 <- ggplot(camps_poly, aes(x=Total_Pop, y=estPop)) +
        geom_point() +
        geom_abline(slope=1, intercept=0) +
        labs(title="Estimated and Reported Population of Subcamps", y="Estimated", x="Reported") +
        coord_cartesian(xlim = c(0, 45000), ylim = c(0, 45000)) 
p2

# Combine two plots in one figure
ggarrange(p1, p2, ncol = 2, nrow = 1)
