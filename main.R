# Project: Corona NTL
# Version: 18-04-2020
# Author: Kamiel Verhelst

# Loading libraries
library(leaflet)

# Loading supporting R-scripts
invisible(sapply(list.files('./R', full.names = T), source))


wms <- 'https://gibs.earthdata.nasa.gov/wms/epsg4326/best/wms.cgi'

leaflet() %>% 
  setView(lng = 4.287638, lat = 50.703039, zoom = 15) %>% 
  addWMSTiles(
    wms,
    layers = "VIIRS_SNPP_DayNightBand_ENCC",
    options = WMSTileOptions(format = "image/png", transparent = TRUE)
  )

