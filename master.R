library(openxlsx)
library(tidyverse)
library(plotly)
library(zoo)
library(readr)
library(dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(maptools)
library(cartogram)
library(broom)
library(plotly)
library(sp)
library(rgdal)


#---------------- Read files  -------------------------------------------------------------
tb3 <- read.csv('Table_3_GHG_food_system_emissions.csv')
cord_df <- read.csv('coords.csv')
pop <- read.csv('population.csv')
tb <- data.frame(tb3) 
cord_df <- data.frame(cord_df)
#------------------------------------------------------------------------------------------

#---------------- Stage 1 Cleaning --------------------------------------------------------
names(tb) <- gsub("Name", "name", names(tb), fixed = TRUE)
names(tb) <- gsub("Country_code_A3", "ccode", names(tb), fixed = TRUE)
names(cord_df) <- gsub("Alpha.3.code", "ccode", names(cord_df), fixed = TRUE)
names(cord_df) <- gsub("Country", "name", names(cord_df), fixed = TRUE)

tb <-inner_join(tb,cord_df,by='name')

tb[ , order(names(tb))]
names(tb)

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
tb <- tb[ -c(5) ]
tb <- tb[ -c(3:4) ]
names(tb) <- gsub("ccode.x", "ccode", names(tb), fixed = TRUE)

mtb = subset(mtb, select = -c(Indicator.Name,Indicator.Code) )

names(mtb) <- gsub("Country.Name", "name", names(mtb), fixed = TRUE)
names(mtb) <- gsub("Country.Code", "ccode", names(mtb), fixed = TRUE)

mtb <- mtb %>%  
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    names_prefix = "yr",
    values_to = "mortality",
    values_drop_na = TRUE
  )

mtb$Year <-gsub("X","",as.character(mtb$Year))
mtb$Year <- as.numeric(mtb$Year)
mtb <- mtb[mtb$Year > 1989,] 
mtb <- mtb[mtb$Year < 2016,] 



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
pop15 <- pop[(pop$Year == 2015),]


#-------------------------------------------------------------------------------------------

#---------------- group ghg  ---------------------------------------------------------------
ang <- tb[tb$ccode == "AGO",]
zwe <- tb[tb$ccode == "ZWE",]
usa <- tb[tb$ccode == "USA",]
usa_p5 <- usa[usa$Year >= 2010,]
#-------------------------------------------------------------------------------------------

#---------------- total mortality sum df  --------------------------------------------------
mort_sum <- aggregate(x = mtb$mortality,             
                     by = list(mtb$Year),              
                     FUN = sum) 
#-------------------------------------------------------------------------------------------

#---------------- total ghg sum df  --------------------------------------------------------
ghg_sum <- aggregate(x = tb$emissions,             
                  by = list(tb$Year),              
                  FUN = sum) 
#-------------------------------------------------------------------------------------------

#---------------- linear regression total ghg emissions  -----------------------------------
# Call:
#   lm(formula = x ~ Group.1, data = ghg_sum)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -979.55 -265.06   15.17  189.58  633.43 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.111e+05  1.911e+04  -5.812 5.42e-06 ***
#   Group.1      6.392e+01  9.543e+00   6.697 6.30e-07 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 365 on 24 degrees of freedom
# Multiple R-squared:  0.6514,	Adjusted R-squared:  0.6369 
# F-statistic: 44.86 on 1 and 24 DF,  p-value: 6.297e-07
#-------------------------------------------------------------------------------------------

#-------------- mortality ------------------------------------------------------------------
ggplot(mort_sum,aes(x=Group.1,y=x)) + geom_point()
#-------------------------------------------------------------------------------------------

#---------------- sum of global food system emissions --------------------------------------
ggplot(ghg_sum,aes(x=Group.1,y=x)) + geom_point() + theme_bw() +
ggtitle("Global Food System Emissions") +  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) + 
theme(axis.title.x = element_text(color="black", vjust=-0.35), axis.title.y = element_text(color="black" , vjust=0.35)) +
xlab('Year') + ylab(expression('Total Emissions (kt of C0'[2]*')')) + stat_smooth(method = "lm", col = "red")
#-------------------------------------------------------------------------------------------

#---------------- highest and lowest emitters ----------------------------------------------
e_sum <- tb %>% group_by(ccode) %>% summarise(emissions = sum(emissions)) 
e_sum <- e_sum %>% arrange(across(starts_with('emissions'),desc))
top10 <- e_sum[1:10,]
e_sum <- e_sum %>% arrange(across(starts_with('emissions')))
bottom10 <- e_sum[1:10,]                          
top10
bottom10
t10 <- tb[(tb$ccode == 'USA' | tb$ccode == 'CHN' | tb$ccode == 'BRA' | tb$ccode == 'IDN' | tb$ccode == 'IND' | tb$ccode == 'RUS' | tb$ccode == 'CAN'
           | tb$ccode == 'ARG' | tb$ccode == 'SDN' | tb$ccode == 'ZMB'),]
b10 <- tb[(tb$ccode == 'MHL' | tb$ccode == 'NRU' | tb$ccode == 'SHN' | tb$ccode == 'AIA' | tb$ccode == 'TUV' | tb$ccode == 'SPM' | tb$ccode == 'TCA'
           | tb$ccode == 'COK' | tb$ccode == 'WLF' | tb$ccode == 'VGB'),]
# write.csv(t10,"top10.csv", row.names = FALSE)
#-------------------------------------------------------------------------------------------

#---------------- top 10 bottom 10 scatter plots  ------------------------------------------
t10_sum <- aggregate(x = t10$emissions,             
                     by = list(t10$Year),              
                     FUN = sum) 

b10_sum <- aggregate(x = b10$emissions,             
                     by = list(b10$Year),              
                     FUN = sum) 

ggplot(t10_sum,aes(x=Group.1,y=x)) + geom_point() + theme_bw() +
  ggtitle("10 Highest Emitters") +  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) + 
  theme(axis.title.x = element_text(color="black", vjust=-0.35), axis.title.y = element_text(color="black" , vjust=0.35)) +
  xlab('Year') + ylab(expression('Total Emissions (kt of C0'[2]*')')) + stat_smooth(method = "lm", col = "red")
#-------------------------------------------------------------------------------------------
(t10_sum[26,2] - t10_sum[21,2]) / t10_sum[21,2] * 100
(b10_sum[26,2] - b10_sum[21,2]) / b10_sum[21,2] * 100

tot_sum <- tb[tb$Year == 2015,]
tot_10_sum <- t10[t10$Year == 2015,]
tot_sum <- aggregate(x = tot_sum$emissions,             
                     by = list(tot_sum$Year),              
                     FUN = sum) 

tot_10_sum <- aggregate(x = tot_10_sum$emissions,             
                    by = list(tot_10_sum$Year),              
                    FUN = sum) 


tot_sum
tot_10_sum



t10_sum

top_10_percent <- tb %>%
          group_by(Year) %>%
          arrange(Year,desc(emissions)) %>%
          filter(emissions >= quantile(emissions,.90))

t51_90_percent <- tb %>%
  group_by(Year) %>%
  arrange(Year,desc(emissions)) %>%
  filter((emissions < quantile(emissions,.9)) & (emissions >= quantile(emissions,.51)))

b50_percent <- tb %>%
  group_by(Year) %>%
  arrange(Year,desc(emissions)) %>%
  filter(emissions < quantile(emissions,.51))


top_10_percent <- aggregate(x = top_10_percent$emissions,             
                        by = list(top_10_percent$Year),              
                        FUN = sum) 
t51_90_percent <- aggregate(x = t51_90_percent$emissions,             
                            by = list(t51_90_percent$Year),              
                            FUN = sum) 
b50_percent <- aggregate(x = b50_percent$emissions,             
                            by = list(b50_percent$Year),              
                            FUN = sum) 



a <- plot_ly(x = top_10_percent$Group.1, y = top_10_percent$x, type="scatter", mode="none", fill = "tozeroy")
a <- add_trace(a, x = t51_90_percent$Group.1, y = t51_90_percent$x, type="scatter", mode="none", fill = "tonexty")
a <- add_trace(a, x = b50_percent$Group.1, y = b50_percent$x, type="scatter", mode="none", fill = "tonexty")
a

#------------------------ Stacked Area Plotly ------------------------------------------

fig <- plot_ly(x = ~top_10_percent$Group.1, y = ~top_10_percent$x, type = 'scatter', mode = 'lines', name = 'Top 10%', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~t51_90_percent$Group.1, y = ~t51_90_percent$x, name = '51-90%', fill = 'tozeroy')
fig <- fig %>% add_trace(x = ~b50_percent$Group.1, y = ~b50_percent$x, name = 'Bottom 50%', fill = 'tozeroy')
fig <- fig %>% layout(xaxis = list(title = 'Year'),
                      xaxis = showgrid = FALSE
                      yaxis = list(title = 'Kt C02'),
                      title ='Share of Food System Emissions')

fig


#---------------------------------- Area Plot ------------------------------------------
p <- t10_sum %>%
  ggplot( aes(x=Group.1, y=x)) +
  geom_area(fill="#69b3a2", alpha=0.5) +
  geom_line(color="#69b3a2") +
  ylab("top10")

# Turn it interactive with ggplotly
p <- ggplotly(p)
p <- add_trace(p, x = b10_sum$Group.1, y = b10_sum$x, type="scatter", mode="markers", fill = "tonexty")
p
#-------------------------------------------------------------------------------------------








#### MAKE INTERACTIVE MAP ########
# world_map <- st_read('World_Countries__Generalized_.shp')
# st_geometry_type(world_map)
# st_crs(world_map)
# st_bbox(world_map)
# world_map
# ggplot() +
#   geom_sf(data = world_map, size = 10, color = 'black', fill = 'cyan1') +
#   ggtitle('world_map_boundary_plot') +
#   coord_sf()
# 

tb %>% order[tb$name]

data.SP <- SpatialPointsDataFrame()

m <- leaflet() %>% 
  addTiles()

world_spdf <- readOGR(
  dsn = "/Users/tonypennoyer/desktop/R_data/wrld_shape",
  layer="TM_WORLD_BORDERS",
  verbose = FALSE
  )

spdf_fortified <- tidy(my_spdf)

world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
m <- leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorQuantile("YlOrRd", POP2005)(POP2005) )
m

# labels <- sprintf(
#   '<strong>%s</strong>br/>%g Food Emissions',
#   tb$name,tb$emissions) %>%
#   lapply(htmltools::HTML)
# 
# pal <- colorBin(palette = 'OrRd',9,domain = tb$emissions)
# 
# map_interactive <- tb %>%
#   st_transform(crs = '+init=epsg:4326') %>%
#   leaflet() %>%
#   addProviderTiles(provider = 'cartoDB.Positron') %>%
#   addPolygons(label = labels,
#               stroke = FALSE,
#               opacity = 1,
#               fillOpacity = 0.7,
#               fillColor = ~pal(emissions),
#               highlightOptions = highlightOptions(weight = 5,
#                                                   fillOpacity = 1,
#                                                   color = 'black',
#                                                   opacity = 1,
#                                                   bringToFront = TRUE))
# addLegend('bottomright',
#           pal = pal,
#           values = ~ emissions,
#           title = 'Food Emissions by Country',
#           opacity = 0.7)
# 
# saveWidget(l, "emissions_test.html")
#   























