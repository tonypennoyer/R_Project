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
          paste("<p>", tb_ordered()$region, "</p>",
            "<p>","Total Emissions: ", round(tb_ordered()$emissions, digits = 3)," Kt C02","</p>",
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
                        fillColor = pal(tb_ordered()$emmissions),
                        label = lapply(labels(), HTML)) %>%
            addLegend(pal = pal,
                      title = 'Kt of C02',
                      values = (tb_ordered()$emissions),
                      opacity = 0.7,
                      position = 'topright')
    )
    
    # output$mymap <- renderLeaflet(
    #     leaflet() %>%
    #         setView(20,20,2) %>%
    #         addProviderTiles(providers$Stamen.TopOSMRelief) %>% 
    #         addPolygons(data = countries,
    #                     weight = 1,
    #                     smoothFactor = 0.5,
    #                     color = 'white',
    #                     fillOpacity = 0.8,
    #                     fillColor = pal(tb_ordered()$em_ratio),
    #                     label = lapply(labels_r(), HTML)) %>%
    #         addLegend(pal = pal,
    #                     title = 'Kt of C02',
    #                     values = (tb_ordered()$em_ratio),
    #                     opacity = 0.7,
    #                     position = 'topright')
    # )
    # 
    output$summary_table <- renderDataTable(tb_ordered())

})



