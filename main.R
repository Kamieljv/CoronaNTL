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


# convert df to raster
VI_r <- mt_to_raster(df = VI)


# write raster to disk
datdir <- 'data/'
writeRaster(VI_r, 
            path=datdir,
            filename=names(VI_r),  
            format="GTiff", 
            overwrite=TRUE)
base_name <- "NDVI_Moria_1km_2015-2020"
writeRaster(VI_r, paste0(datdir, base_name,".tif"), overwrite=T) #write raster
write.csv(names(VI_r), file=paste0(datdir, base_name, ".csv"), row.names=F) #write layer names

# load raster from file
stacks <- list.files(path=datdir, 
                    pattern="\\.tif$", 
                    full.names=TRUE)
names <- list.files(path=datdir, 
                     pattern="\\.csv$", 
                     full.names=TRUE)
r <- stack(files[1])
names(r) <- as.character(read.csv(names[1])$x)



