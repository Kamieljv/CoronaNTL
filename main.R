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
neededPackages <- c("strucchange","zoo", "bfast", "raster", "leaflet", "MODISTools")
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



# Vegetation index at the Moria Camp, Lesbos, Greece (39.13493, 26.50351)
VI <- mt_subset(product = "MOD13Q1",
                lat = 39.13493,
                lon = 26.50351,
                band = '250m_16_days_NDVI',
                start = "2015-01-01",
                end = "2020-01-01",
                km_lr = 1,
                km_ab = 1,
                progress = TRUE)
QA <- mt_subset(product = "MOD13Q1",
                lat = 39.13493,
                lon = 26.50351,
                band = "250m_16_days_pixel_reliability",
                start = "2015-01-01",
                end = "2020-01-01",
                km_lr = 1,
                km_ab = 1,
                progress = FALSE)


# convert df to raster
VI_r <- mt_to_raster(df = VI)
QA_r <- mt_to_raster(df = QA)


# write raster to disk
datdir <- 'data/'

## Vegetation indices
base_name <- "NDVI_Moria_1km_2015-2020"
writeRaster(VI_r, paste0(datdir, base_name,".tif"), overwrite=T) #write raster
write.csv(names(VI_r), file=paste0(datdir, base_name, ".csv"), row.names=F) #write layer names

## Quality Assessment
base_name <- "NDVI_Moria_1km_2015-2020_QA"
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

# Create time series in single pixel
px <- 78 # pixel number so adjust this number to select the center pixel
tspx <- timeser(as.vector(VI_m[px]),as.Date(names(VI_m), "X%Y.%m.%d")) # convert pixel 1 to a time series
plot(tspx, main = sprintf('NDVI in pixel %d', px)) # NDVI time series cleaned using the "reliability information"




