
library(rgbif)
library(tidyverse)

rm(list = ls())
fox_data = occ_data(scientificName = "Vulpes macrotis", limit = 4000)$data
foxes_2017=subset(fox_data,year %in% c(2017,2018,2019))
foxes_2015=subset(fox_data,year %in% c(2014,2015,2016))
foxes_2013=subset(fox_data,year %in% c(2011,2012,2013))

yearcounts <- as.data.frame(table(fox_data$year))

ggplot()+
  geom_point(data=foxes_2017, aes(x=decimalLatitude,y=decimalLongitude),col="blue") +
  geom_point(data=foxes_2015, aes(x=decimalLatitude,y=decimalLongitude),col="red") +
  geom_point(data=foxes_2013, aes(x=decimalLatitude,y=decimalLongitude),col="yellow")
