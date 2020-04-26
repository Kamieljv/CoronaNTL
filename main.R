# Project: Corona NTL
# Version: 24-04-2020
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
neededPackages <- c("ggplot2", "strucchange","zoo", "bfast", "raster", "leaflet", "MODISTools", "tsbox", "shinyWidgets")
for (package in neededPackages){pkgTest(package)}

# Loading supporting R-scripts
invisible(sapply(list.files('./R', full.names = T), source))

# defining time series function
timeser <- function(index, dt) {
  z <- zoo(index, dt)
  yr <- as.numeric(format(time(z), "%Y"))
  jul <- as.numeric(format(time(z), "%j"))
  delta <- min(unlist(tapply(jul, yr, diff))) # 16
  zz <- aggregate(z, yr + (jul - 1) / delta / 23)
  (tso <- as.ts(zz))
  return(tso)
}

# Define coordinages of Moria Camp, Lesbos, Greece (39.13493, 26.50351)
camp_lat <- 39.13493
camp_lon <- 26.50351

# Vegetation index at the Moria Camp, Lesbos, Greece (39.13493, 26.50351)
VI <- mt_subset(product = "MOD13Q1",
                lat = camp_lat,
                lon = camp_lon,
                band = '250m_16_days_NDVI',
                start = "2010-01-01",
                end = "2020-01-01",
                km_lr = 2,
                km_ab = 2,
                progress = TRUE)
QA <- mt_subset(product = "MOD13Q1",
                lat = camp_lat,
                lon = camp_lon,
                band = "250m_16_days_pixel_reliability",
                start = "2010-01-01",
                end = "2020-01-01",
                km_lr = 2,
                km_ab = 2,
                progress = TRUE)


# convert df to raster
VI_r <- mt_to_raster(df = VI)
QA_r <- mt_to_raster(df = QA)


# write raster to disk
datdir <- 'data/'

## Vegetation indices
base_name <- "NDVI_Moria_2km_2010-2020"
writeRaster(VI_r, paste0(datdir, base_name,".tif"), overwrite=T) #write raster
write.csv(names(VI_r), file=paste0(datdir, base_name, ".csv"), row.names=F) #write layer names

## Quality Assessment
base_name <- "NDVI_Moria_2km_2010-2020_QA"
writeRaster(QA_r, paste0(datdir, base_name,".tif"), overwrite=T) #write raster
write.csv(names(QA_r), file=paste0(datdir, base_name, ".csv"), row.names=F) #write layer names

# load raster from file
stacks <- list.files(path=datdir, 
                    pattern="\\.tif$", 
                    full.names=TRUE)
names <- list.files(path=datdir, 
                     pattern="\\.csv$", 
                     full.names=TRUE)

# Load VI data
VI_r <- stack(stacks[1])
names(VI_r) <- as.character(read.csv(names[1])$x)

# Load QA data
QA_r <- stack(stacks[1])
names(QA_r) <- as.character(read.csv(names[1])$x)

## clean the data
# create mask on pixel reliability flag set all values <0 or >1 NA
m <- QA_r
m[(QA_r < 0 | QA_r > 1)] <- NA # continue working with QA 0 (good data), and 1 (marginal data)

# mask all values from VI raster NA
VI_m <- mask(VI_r, m, maskvalue=NA, updatevalue=NA)



# plot the first image
plot(m,1) # plot mask
plot(VI_m,1) # plot cleaned NDVI raster

# Reproject to WGS 84


# Inspect the area using Leaflet
layer <- VI_m$X2010.01.01
colbin <- colorBin(palette = "RdYlGn", domain = c(0, 1), na.color = NA, pretty=T, bins=9)

leaflet(data = layer) %>%
  addProviderTiles(providers$OpenStreetMap) %>% # Add basemap
  addScaleBar(position = "topright") %>% # Add scalebar
  setView(lng=camp_lon, lat=camp_lat, zoom=13) %>%
  addRasterImage(layer, opacity=0.7, colors = colbin) %>% # Add the raster layer
  addLegend(pal = colbin,
            values = values(layer),
            title = "legendTitle",
            position = "bottomright") # Add a legend


# Create time series in single pixel
px <- 78 # pixel number so adjust this number to select the center pixel
tspx <- timeser(as.vector(VI_m[px]),as.Date(names(VI_m), "X%Y.%m.%d")) # convert pixel 1 to a time series
plot(tspx, main = sprintf('NDVI in pixel %d', px)) # NDVI time series cleaned using the "reliability information"

# Create a dataframe with all times series combined
ts.df <- ts_df(timeser(as.vector(VI_m[1]), as.Date(names(VI_m), "X%Y.%m.%d")))
for (px in 2:ncell(VI_m)) {
  ts.df[px+1] <- timeser(as.vector(VI_m[px]), as.Date(names(VI_m), "X%Y.%m.%d"))
}
names(ts.df)[2:ncol(ts.df)] <- paste0('px', 1:ncell(VI_m))




bfm1 <- bfastmonitor(tspx, response ~ trend + harmon, order = 4, start = c(2016,1)) # Note: the first observation in 2019 marks the transition from 'history' to 'monitoring'
plot(bfm1)

