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
tb = subset(tb, select = -c(X,rank_num))
tb <- subset(tb, is.element(tb$region,countries$GEOUNIT))
countries <- subset(countries, is.element(countries$GEOUNIT,tb$region))
tb <- tb[order(match(tb$region, countries$GEOUNIT)),]
teen_nums <- c(11,12,13,111,112,113)

ui <- dashboardPage(
    skin = 'red',
    dashboardHeader(title = 'Food System Emissions'),
    dashboardSidebar(
        # sidebarUserPanel("Tony Pennoyer",
        #                  subtitle = 'NYCDSA Fall 2021 Project',
        #                  image = "./NYCDSA.png"),
        sliderInput("year_input", label = "Year:",
                    min = min(tb$Year),
                    max = max(tb$Year),
                    value = c(min(tb$Year)),
                    sep = "",
                    step = 1),
        selectInput("dtype",'Choose data type: ', 
                    c("Total Emissions" = "emissions",
                      "Emissions per capita" = "em_ratio"),
                    selected = "Total Emissions")
        # menuItem("Dashboard",tabName = 'dashboard'),
        # menuItem("What are Food Emissions?",tabName = 'what')
    ),
    dashboardBody(
        
                fluidRow(box(width = 12, leafletOutput(outputId = "mymap"))),
                fluidRow(box(width = 12, dataTableOutput(outputId = 'summary_table'))),
    ),
)
