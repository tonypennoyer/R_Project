library(openxlsx)
library(tidyverse)
library(plotly)
library(zoo)

tb3 <- read.csv('Table_3_GHG_food_system_emissions.csv')

tb <- data.frame(tb3)

# sub("X","",tb[1,])
# tb3[3:28] <- 

names(tb) <- gsub("X", "", names(tb), fixed = TRUE)
names(tb)

