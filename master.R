library(openxlsx)
library(tidyverse)
library(plotly)
library(zoo)
library(dplyr)
library(ggplot2)


tb3 <- read.csv('Table_3_GHG_food_system_emissions.csv')

tb <- data.frame(tb3) 

names(tb) <- gsub("Name", "name", names(tb), fixed = TRUE)
names(tb) <- gsub("Country_code_A3", "ccode", names(tb), fixed = TRUE)
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

tail(tb)

ang <- tb[tb$ccode == "AGO",]
zwe <- tb[tb$ccode == "ZWE",]
usa <- tb[tb$ccode == "USA",]
usa_p5 <- usa[usa$Year >= 2010,]

sum(tb$emissions)

ghg_sum <- aggregate(x = tb$emissions,             
                  by = list(tb$Year),              
                  FUN = sum) 


ggplot(ghg_sum,aes(x=Group.1,y=x)) + geom_point() + theme_bw() +
ggtitle("Global Food System Emissions") +  theme(plot.title = element_text(hjust = 0.5)) +
xlab('Year') + ylab('Total Emissions (kt of C02)')

model = lm(x ~ Group.1, data = ghg_sum)





ggplot(usa,aes(x=Year,y=emissions)) + geom_line()
xlab('Year') # for the x axis label
ylab('Emissions: ')

ggplot(ang,aes(x=Year,y=emissions)) + geom_line()

       



# ang <- tb[tb$ccode == "AGO",]  




