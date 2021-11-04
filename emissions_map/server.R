library(shiny)
library(ggplot2) 
library(tidyverse)
library(sp)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(rgdal)
library(dplyr)

shinyServer(function(input, output) {
    
    tb_update <- reactive({
        tb %>% 
            filter(Year == input$year_input)
    })
    
    tb_ordered <- reactive ({
        tb_update()[order(match(tb_update()$region, countries$GEOUNIT)),]
    })
   
    labels <- reactive({
          paste("<h6>", tb_ordered()$region, "</h6>",
            "<h6>","Total Emissions: ", round(tb_ordered()$emissions, digits = 1)," Kt C02","</h6>",
            sep = "")
    })
    
    output$mymap <- renderLeaflet(
        leaflet() %>%
            setView(20,20,2) %>%
            addProviderTiles(providers$Stamen.TopOSMRelief) %>% 
            addPolygons(data = countries,
                        weight = 1,
                        smoothFactor = 0.5,
                        color = 'white',
                        fillOpacity = 0.8,
                        fillColor = pal(tb_ordered()$emissions),
                        label = lapply(labels(), HTML),
                        highlightOptions = highlightOptions(color = "white",
                                                            weight = 1, bringToFront = FALSE, opacity = 5)) %>%
            
            addLegend(pal = pal,
                      title = 'Kt of C02',
                      values = (tb_ordered()$emissions),
                      opacity = 0.7,
                      position = 'topright')
    )
    
    output$summary_table <- renderDataTable(tb_ordered())

})



