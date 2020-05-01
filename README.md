# Refugee Camp Detection with MODIS time-series data

The aim of this small research project is to develop a light-weight method to detect (changes in) refugee camps around the world, e.g. their footprint or their impact on their environment.

The main data for the project comes from the MODIS/Terra satellite, taking 16-day images with a 250m spatial resolution. Specifically, the pre-calculated NDVI-layer will be used. The data is loaded in R using the [MODISTools package](https://cran.r-project.org/web/packages/MODISTools/MODISTools.pdf). The time-series analysis is done using the [BFAST package](https://cran.r-project.org/web/packages/bfast/index.html), which decomposes the time-series data into trend, seasonal and remainder components, and can therefore separate regular changes from irregular ones.


Populaion maps: https://data.humdata.org/event/rohingya-displacement
