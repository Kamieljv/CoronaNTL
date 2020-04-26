#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

date_vec <- as.Date(substr(names(VI_m), 2, nchar(names(VI_m)[1])), format="%Y.%m.%d")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NDVI at Moria Camp, Lesbos, Greece"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderTextInput("dateSlider", "Scroll through time...",
                            choices = date_vec)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("NDVImap", width = "100%", height = 600)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    layer <- VI_m$X2010.01.01
    colbin <- colorBin(palette = "RdYlGn", domain = c(0, 1), na.color = NA, pretty=T, bins=9)
    
    output$NDVImap <- renderLeaflet({
        leaflet(data = layer) %>%
            addProviderTiles(providers$OpenStreetMap) %>% # Add basemap
            addScaleBar(position = "topright") %>% # Add scalebar
            setView(lng=camp_lon, lat=camp_lat, zoom=13) %>%
            addLegend(pal = colbin,
                      values = values(layer),
                      title = "legendTitle",
                      position = "bottomright") # Add a legend
    })
    
    # If any input changes, update the raster map layer
    observeEvent(input$dateSlider, {
            # Get data
            NDVIdata <- VI_m
            name <- gsub('-', '.', paste0('X', input$dateSlider))
            # Get layer name from data brick using dateString
            layerName <- grep(name, names(NDVIdata), value = TRUE)
            # Subset data brick to select correct layer
            layer <- subset(NDVIdata, layerName)
            
            # Add to existing leaflet map
            leafletProxy("NDVImap", data = layer) %>%
                clearImages() %>% # Clear raster from previous date
                clearControls() %>% # Clear map controls from previous date
                setView(lng=camp_lon, lat=camp_lat, zoom=13) %>%                
                addRasterImage(layer, opacity=0.7, colors = colbin) %>% # Add the raster layer
                addLegend(pal = colbin, 
                          values = values(layer), 
                          title = "legendTitle",
                          position = "bottomright") # Add a legend
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
