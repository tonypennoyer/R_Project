library(shiny)
library(shinydashboard)
library(ggplot2) 
library(tidyverse)
library(sp)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(rgdal)
library(dplyr)

countries <- readOGR('/Users/tonypennoyer/Desktop/R_data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')
tb <- read.csv('/Users/tonypennoyer/desktop/R_data/emissionsWpop.csv')
tb <- subset(tb, is.element(tb$region,countries$GEOUNIT))
countries <- subset(countries, is.element(countries$GEOUNIT,tb$region))
tb <- tb[order(match(tb$region, countries$GEOUNIT)),]

bins <- c(0,25,50,100,200,500,1000,2500)
pal <- colorBin('Reds', domain = c(0,2500),bins = bins)


ui <- dashboardPage(
    skin = 'red',
    dashboardHeader(title = 'Food System Emissions'),
    dashboardSidebar(
        sliderInput("year_input", label = "Year:",
                    min = min(tb$Year),
                    max = max(tb$Year),
                    value = c(min(tb$Year)),
                    sep = "",
                    step = 1),
        selectInput(inputId = "sel_dtype",
                    label = 'Choose data type: ',
                    list("Total Emissions","Emissions per 1000 people"))
    ),
    dashboardBody(
        fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
        fluidRow(box(width = 12, dataTableOutput(outputId = 'summary_table')))
    )
)
