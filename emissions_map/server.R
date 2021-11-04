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
        if (input$dtype == 'emissions') {
          paste("<strong>", tb_ordered()$region, "</strong>",
            "<h6>", round(tb_ordered()$emissions, digits = 1)," kt C02","</h6>",
            sep = "")
        } else {
            paste("<strong>", tb_ordered()$region, "</strong>",
                  "<h6>", round(tb_ordered()$em_ratio, digits = 1)," t C02","</h6>",
                  sep = "")
        }
    })
    
    output$mymap <- renderLeaflet(
       
        if (input$dtype == 'emissions') {
            bins= c(0,25,50,100,200,500,1000,2500)
            pal= colorBin('Reds', domain = c(0,2500),bins = bins)
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
                                                                weight = 1,
                                                                bringToFront = TRUE,
                                                                opacity = 5)) %>%
            
                addLegend(pal = pal,
                          title = 'Kt of C02',
                          values = (tb_ordered()$emissions),
                          opacity = 0.7,
                          position = 'topright')
        } else {
            bins2= c(0,1,3,7,70)
            pal2= colorBin('Reds', domain = c(0,70),bins = bins2)
            leaflet() %>%
                setView(20,20,2) %>%
                addProviderTiles(providers$Stamen.TopOSMRelief) %>% 
                addPolygons(data = countries,
                            weight = 1,
                            smoothFactor = 0.5,
                            color = 'white',
                            fillOpacity = 0.8,
                            fillColor = pal2(tb_ordered()$em_ratio),
                            label = lapply(labels(), HTML),
                            highlightOptions = highlightOptions(color = "white",
                                                                weight = 1,
                                                                bringToFront = TRUE,
                                                                opacity = 5)) %>%
                
                addLegend(pal = pal2,
                          title = 'Kt of C02',
                          values = (tb_ordered()$em_ratio),
                          opacity = 0.7,
                          position = 'topright')
        }
    )
    
    output$summary_table <- renderDataTable(tb_ordered())

})



