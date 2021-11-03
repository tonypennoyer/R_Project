library(ggplot2)
library(tidyverse)
library(sp)
library(sf)
library(htmltools)
library(htmlwidgets)
library(leaflet)
library(rgdal)
library(raster)
library(dplyr)

tb <- read.csv('Table_3_GHG_food_system_emissions.csv')
pop <- read.csv('population.csv')

names(tb) <- gsub("Name", "region", names(tb), fixed = TRUE)
names(tb) <- gsub("Country_code_A3", "ccode", names(tb), fixed = TRUE)

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

pop = subset(pop, select = -c(Indicator.Name,Indicator.Code))
pop <- pop %>%  
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    names_prefix = "yr",
    values_to = "Population",
    values_drop_na = TRUE
  )
names(pop) <- gsub("Country.Name", "region", names(pop), fixed = TRUE)
names(pop) <- gsub("Country.Code", "ccode", names(pop), fixed = TRUE)
pop$Year <-gsub("X","",as.character(pop$Year))
pop$Year <- as.numeric(pop$Year)
pop <- pop[(pop$Year > 1989),]

tb2015 <- tb[tb$Year == 2015,]

tb2015$region[which(tb2015$region == "United States")] <- 'United States of America'
tb2015$region[which(tb2015$region == "Viet Nam")] <- 'Vietnam'
tb2015$region[which(tb2015$region == "Russian Federation")] <- 'Russia'
tb2015$region[which(tb2015$region == "Democratic Republic of the Congo")] <- 'Republic of the Congo'
tb2015$region[which(tb2015$region == "Congo_the Democratic Republic of the")] <- 'Democratic Republic of the Congo'
tb2015$region[which(tb2015$region == "Libyan Arab Jamahiriya")] <- 'Libya'
tb2015$region[which(tb2015$region == "Tanzania_United Republic of")] <- 'Tanzania'
tb2015$region[which(tb2015$region == "Czech Republic")] <- 'Czechia'
tb2015$region[which(tb2015$region == "Iran, Islamic Republic of")] <- 'Iran'
tb2015$region[which(tb2015$region == "Korea, Republic of")] <- 'South Korea'
tb2015$region[which(tb2015$region == "Korea, Democratic People's Republic of")] <- 'North Korea'
tb2015$region[which(tb2015$region == "Syrian Arab Republic")] <- 'Syria'
tb2015$region[which(tb2015$region == "Moldova, Republic of")] <- 'Moldova'
tb2015$region[which(tb2015$region == "Serbia and Montenegro")] <- 'Republic of Serbia'
tb2015$region[which(tb2015$region == "Macedonia, the former Yugoslav Republic of")] <- 'North Macedonia'
tb2015$region[which(tb2015$region == "Lao People's Democratic Republic")] <- 'Laos'
tb2015$region[which(tb2015$region == "Cote d'Ivoire")] <- 'Ivory Coast'

pop15$region[which(pop15$region == "United States")] <- 'United States of America'
pop15$region[which(pop15$region == "Viet Nam")] <- 'Vietnam'
pop15$region[which(pop15$region == "Russian Federation")] <- 'Russia'
pop15$region[which(pop15$region == "Democratic Republic of the Congo")] <- 'Republic of the Congo'
pop15$region[which(pop15$region == "Congo_the Democratic Republic of the")] <- 'Democratic Republic of the Congo'
pop15$region[which(pop15$region == "Libyan Arab Jamahiriya")] <- 'Libya'
pop15$region[which(pop15$region == "Tanzania_United Republic of")] <- 'Tanzania'
pop15$region[which(pop15$region == "Czech Republic")] <- 'Czechia'
pop15$region[which(pop15$region == "Iran, Islamic Republic of")] <- 'Iran'
pop15$region[which(pop15$region == "Korea, Republic of")] <- 'South Korea'
pop15$region[which(pop15$region == "Korea, Democratic People's Republic of")] <- 'North Korea'
pop15$region[which(pop15$region == "Syrian Arab Republic")] <- 'Syria'
pop15$region[which(pop15$region == "Moldova, Republic of")] <- 'Moldova'
pop15$region[which(pop15$region == "Serbia and Montenegro")] <- 'Republic of Serbia'
pop15$region[which(pop15$region == "Macedonia, the former Yugoslav Republic of")] <- 'North Macedonia'
pop15$region[which(pop15$region == "Lao People's Democratic Republic")] <- 'Laos'
pop15$region[which(pop15$region == "Cote d'Ivoire")] <- 'Ivory Coast'

tb <- inner_join(tb,pop,by='region')
tb = subset(tb, select = -c(ccode.y,Year.x))
names(tb) <- gsub("ccode.x", "ccode", names(tb), fixed = TRUE)
names(tb) <- gsub("Year.y", "Year", names(tb), fixed = TRUE)

tb <- tb %>%
          group_by(region) %>%
          mutate(em_ratio = (emissions / Population)*1000000)

tb <- tb %>% 
  mutate(Rank = case_when(em_ratio < .8  ~ 'A', 
                        em_ratio >.8 & em_ratio < 1.7 ~ 'B',
                        em_ratio >1.7 & em_ratio < 4 ~ 'C',
                        em_ratio > 4 & em_ratio < 12 ~ 'D',
                        em_ratio > 12~ 'F',))
         
ggplot(data = tb2015,aes(x=Population,y=emissions)) + geom_point()


# 0.03481652  1.24949402  1.94920357  3.88123372 62.82547970 



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

tb2015 <- subset(tb2015, is.element(tb2015$region,countries$GEOUNIT))
countries <- subset(countries, is.element(countries$GEOUNIT,tb2015$region))

tb2015 <- tb2015[order(match(tb2015$region, countries$GEOUNIT)),]

bins <- c(0,40,200,500,1000,2000,2500)
pal <- colorBin('Blues', domain = tb2015$emissions,bins = bins)

labels <- paste("<p>", tb2015$region, "</p>",
                "<p>","2015 Food System Emissions: ", round(tb2015$emissions,digits = 3)," (Kt of C02)", "</p>",
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
              label = lapply(labels, HTML)) %>%
  addLegend(pal = pal,
            values = tb2015$emissions,
            opacity = 0.7,
            position = 'topright')

m



