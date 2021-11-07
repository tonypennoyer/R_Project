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
    
    tb_update_2 <- reactive({
        tb_update() %>% 
            arrange(desc(emissions)) %>% 
            mutate(tot_emissions_rank = row_number()) %>% 
            arrange(desc(em_ratio)) %>%
            mutate(emissions_per_capita_rank = row_number()) %>%
            mutate(sup_rat = case_when(emissions_per_capita_rank %% 10 == 1 & (is.element(emissions_per_capita_rank, teen_nums) == FALSE) ~ 'st',
                                       emissions_per_capita_rank %% 10 == 2 & (is.element(emissions_per_capita_rank, teen_nums) == FALSE) ~ 'nd',
                                       emissions_per_capita_rank %% 10 == 3 & (is.element(emissions_per_capita_rank, teen_nums) == FALSE) ~ 'rd',
                                       TRUE ~ 'th')) %>% 
            mutate(sup_tot = case_when(tot_emissions_rank %% 10 == 1 & (is.element(tot_emissions_rank, teen_nums) == FALSE) ~ 'st',
                                       tot_emissions_rank %% 10 == 2 & (is.element(tot_emissions_rank, teen_nums) == FALSE) ~ 'nd',
                                       tot_emissions_rank %% 10 == 3 & (is.element(tot_emissions_rank, teen_nums) == FALSE) ~ 'rd',
                                       TRUE ~ 'th')) %>%
            group_by(Year) %>%
            mutate(emissions_global_total = sum(emissions)) %>% 
            group_by(region) %>%
            mutate(global_perc = ((emissions / emissions_global_total) * 100))
            
    })
    
    tb_ordered <- reactive ({
        tb_update_2()[order(match(tb_update_2()$region, countries$GEOUNIT)),]
    })
   
    labels <- reactive({
        if (input$dtype == 'emissions') {
          paste("<h6>","<b>", tb_ordered()$region, "</b>","</h6>",
            "<h6>", "<b>",round(tb_ordered()$emissions, digits = 1),"</b>"," kt C0","<sub>",'2',"</sub>","</h6>",
            "<h6>","<b>", tb_ordered()$tot_emissions_rank,"<sup>",tb_ordered()$sup_tot,"</sup>","</b>",' out of ','159', "</h6>",
            "<h6>","<b>", round(tb_ordered()$global_perc,digits = 2),"%","</b>"," of global total", "</h6>",
            sep = "")
        } else {
            paste("<h6>","<b>", tb_ordered()$region, "</b>","</h6>",
                  "<h6>", "<b>",round(tb_ordered()$em_ratio, digits = 1),"</b>"," t C0","<sub>",'2',"</sub>","</h6>",
                  "<h6>","<b>", tb_ordered()$emissions_per_capita_rank,"<sup>",tb_ordered()$sup_rat,"</sup>","</b>",' out of ','159', "</h6>",
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
                            highlightOptions = highlightOptions(color = "black",
                                                                weight = 1,
                                                                bringToFront = TRUE,
                                                                fillOpacity = 0.9)) %>%
            
                addLegend(pal = pal,
                          title = 'Kt of C02',
                          values = (tb_ordered()$emissions),
                          opacity = 0.7,
                          position = 'topright')
        } else {
            bins2= c(0,1,2,5,10,20,90)
            pal2= colorBin('Blues', domain = c(0,90),bins = bins2)
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
                            highlightOptions = highlightOptions(color = "black",
                                                                weight = 1.5,
                                                                bringToFront = TRUE,
                                                                opacity = 5)) %>%
                
                addLegend(pal = pal2,
                          title = 't of C02',
                          values = (tb_ordered()$em_ratio),
                          opacity = 0.7,
                          position = 'topright')
        }
    )
    
    output$summary_table <- renderDataTable(tb_ordered())

})



