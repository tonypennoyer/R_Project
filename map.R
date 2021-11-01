library(ggplot2)
library(tidyverse)
library(sp)
library(sf)
library(htmltools)
library(leaflet)
library(rgdal)
library(raster)
library(dplyr)

tb <- read.csv('Table_3_GHG_food_system_emissions.csv')
# cord_df <- read.csv('coords.csv')

names(tb) <- gsub("Name", "region", names(tb), fixed = TRUE)
names(tb) <- gsub("Country_code_A3", "ccode", names(tb), fixed = TRUE)
# names(cord_df) <- gsub("Alpha.3.code", "ccode", names(cord_df), fixed = TRUE)
# names(cord_df) <- gsub("Country", "region", names(cord_df), fixed = TRUE)
# 
# tb <-inner_join(tb,cord_df,by='region')

tb <- tb %>%  
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    names_prefix = "yr",
    values_to = "emissions",
    values_drop_na = TRUE
  )

tb$Year <-gsub("X","",as.character(tb$Year))
tb$Year <- as.numeric(tb$Year)
tb <- tb[tb$ccode != 'TKL',]
# tb <- tb[ -c(5) ]
# tb <- tb[ -c(3:4) ]
# names(tb) <- gsub("ccode.x", "ccode", names(tb), fixed = TRUE)

tb2015 <- tb[tb$Year == 2015,]

tb2015$region[which(tb2015$region == "United States")] <- 'United States of America'
tb2015$region[which(tb2015$region == "Viet Nam")] <- 'Vietnam'
tb2015$region[which(tb2015$region == "Russian Federation")] <- 'Russia'
tb2015$region[which(tb2015$region == "Congo")] <- 'Democratic Republic of the Congo'
tb2015$region[which(tb2015$region == "Libyan Arab Jamahiriya")] <- 'Libya'
tb2015$region[which(tb2015$region == "Tanzania_United Republic of")] <- 'Tanzania'
# tb2015$region[which(tb2015$region == "United Kingdom")] <- 'UK'
tb2015$region[which(tb2015$region == "Iran, Islamic Republic of")] <- 'Iran'
tb2015$region[which(tb2015$region == "Korea, Republic of")] <- 'South Korea'
tb2015$region[which(tb2015$region == "Korea, Democratic People's Republic of")] <- 'North Korea'



# 
# mapdata <- map_data("world")
# 
# mapdata <- left_join(mapdata, tb2015, by='region')
# 
# mapdata <- mapdata %>% filter(!is.na(mapdata$emissions))
# 
# map1 <- ggplot(mapdata, aes(x = long, y = lat, group = group)) +
#   geom_polygon(aes(fill = emissions), color = 'black')
# 
# map2 <- map1 + scale_fill_gradient(name = "2015 Food System Emissions", low = 'yellow', high = 'red', na.value = 'grey50') +
#   theme(axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         rect = element_blank())
# 
# class(mapdata) # dataframe
# 
# coordinates(mapdata) <- ~long+lat
# proj4string(mapdata)<- CRS("+proj=longlat +datum=WGS84")
# raster::shapefile(mapdata, "MyShapefile.shp")
# 
# emissions_spdf <- readOGR(
#   dsn = "/Users/tonypennoyer/desktop/R_data/myShape",
#   layer="MyShapefile",
#   verbose = FALSE
# )
# 
# e_trans <- spTransform(emissions_spdf,crs("+init=epsg:4326"))

countries <- readOGR('/Users/tonypennoyer/Desktop/R_data/ne_110m_admin_0_countries/ne_110m_admin_0_countries.shp')

# countries <- subset(countries, is.element(countries$GU_A3, tb2015$ccode))
# countries <- subset(countries, is.element(tb2015$ccode, countries$GU_A3))

tb2015 <- tb2015[order(match(countries$GU_A3, tb2015$ccode)),]

bins <- c(0,3000)
pal <- colorBin('RdYlBu', domain = tb2015$emissions,bins = bins)

labels <- paste("<p>", tb2015$region, "</p>",
                "<p>","2015 Emissions: ", round(tb2015$emissions,digits = 3)," Kt of C02", "</p>",
                sep = "")

m <- leaflet() %>% 
  setView(91,5,2) %>%
  addProviderTiles(providers$Stamen.Toner) %>% 
  addPolygons(data = countries,
              weight = 1,
              smoothFactor = 0.5,
              color = 'white',
              fillOpacity = 0.8,
              fillColor = pal(tb2015$emissions),
              label = lapply(labels, HTML))

m



