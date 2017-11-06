library(shiny)
library(leaflet)
library(RColorBrewer)
library(geojsonio)

#rm(list = ls())
options(stringsAsFactors = FALSE)

setwd("/Users/FCORVAGL/Desktop/Data/Map")
D15m <- read.csv("D15m.csv")
#D15m <- read.csv("D15AVGm.csv")
#names(D15m)[6] <- "AVG Public Transport/Walking"
#names(D15m)[7] <- "AVG Cycle"
#names(D15m)[8] <- "AVG Car"
UKMapG <- geojsonio::geojson_read("Lower_Layer_Super_Output_Areas_December_2011_Generalised_Clipped__Boundaries_in_England_and_Wales.geojson.json", what = "sp")
UKMap <- UKMapG
UKMap@data <- merge(UKMap@data, D15m, by.x = "lsoa11cd" , by.y = "LSOA_code", all.x  = TRUE)

pal <- colorBin( c("#F6E0D1",  "#F1B189", "#D25F15", "#871E00"), domain = c(0, 200), 
                 bins = c(0, 15, 30, 45, 200))

#Add Columns in the GEOJSON for giving different colors to the county
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, left = 100,
                selectInput("point", "Point of interest",
                            names(UKMap)[11:ncol(UKMap)], 
                            multiple = FALSE, selectize = TRUE)
  )
)



server <- function(input, output) {
  
  output$mymap <- renderLeaflet({
    leaflet(UKMap) %>%
      addTiles() %>%
      setView(lng = -2.932931, lat = 54.892473, zoom = 6) #Coordinates Carlisle
  })
  
  observeEvent(input$point, {
    leafletProxy("mymap", data=UKMap) %>% 
      clearShapes() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  fillColor = ~pal(UKMap@data[, input$point]),
                  label = ~paste0(UKMap@data[, 9], ": ", formatC(UKMap@data[, input$point], big.mark = ","))) %>%
      clearControls() %>%
      addLegend(colors = c("#F6E0D1",  "#F1B189", "#D25F15", "#871E00"),
                labels= c("0-15", "15-30","30-45", "Over 45"), title = input$point,
                opacity = 1.0)
    
  })
  
}

shinyApp(ui, server)

