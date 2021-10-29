library(openxlsx)
library(tidyverse)
library(plotly)
library(zoo)
library(dplyr)
library(ggplot2)

#---------------- Read file  --------------------------------------------------------------
tb3 <- read.csv('Table_3_GHG_food_system_emissions.csv')

tb <- data.frame(tb3) 
#------------------------------------------------------------------------------------------

#---------------- Stage 1 Cleaning --------------------------------------------------------
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
#-------------------------------------------------------------------------------------------


ang <- tb[tb$ccode == "AGO",]
zwe <- tb[tb$ccode == "ZWE",]
usa <- tb[tb$ccode == "USA",]
usa_p5 <- usa[usa$Year >= 2010,]


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

#---------------- sum of global food system emissions --------------------------------------
ggplot(ghg_sum,aes(x=Group.1,y=x)) + geom_point() + theme_bw() +
ggtitle("Global Food System Emissions") +  theme(plot.title = element_text(hjust = 0.5)) + theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) + 
theme(axis.title.x = element_text(color="black", vjust=-0.35), axis.title.y = element_text(color="black" , vjust=0.35)) +
xlab('Year') + ylab(expression('Total Emissions (kt of C0'[2]*')')) + stat_smooth(method = "lm", col = "red")
#-------------------------------------------------------------------------------------------



# bold text
# theme(plot.title = element_text(size=20, face="bold",  margin = margin(10, 0, 10, 0)))
                       





ggplot(usa,aes(x=Year,y=emissions)) + geom_line()
xlab('Year') # for the x axis label
ylab('Emissions: ')

ggplot(ang,aes(x=Year,y=emissions)) + geom_line()

       



# ang <- tb[tb$ccode == "AGO",]  




