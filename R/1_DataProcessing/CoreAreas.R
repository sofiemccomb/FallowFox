#Created Highest Core Areas for the San Joaquin Kit Fox in Kern County

#Read in packages
###############################
library(tidyverse) #data wrangling
library(sf) #shapefiles
library(raster) #rasters
library(fasterize) #fast rasterize
library(rgeos)
library(cleangeo)
options(scipen = 999) #no scientific notation
rasterOptions(tmpdir="Z:/Abandonment/Data/RasterTemp")


#San Joaquin Kit Fox Highest Core Areas
###############################################
#Read in Kern Shapefile to crop
Kern<-read_sf("Data/1_DataProcessing/KernCounty/Kern.shp")
kerncounty <- sf:::as_Spatial(Kern)#sf back to sp 
kc<-st_as_sf(kerncounty)

#Kit Fox Shapefile created from downloaded CWHR Habitat Raster: https://map.dfg.ca.gov/metadata/ds2599.html 
  #Kit Fox geodatabase file (ds2599 folder unzipped) made into TIF file using Lookup tool (for Mean_100 value column) (ds2599.tif)
    #Then aggregated at 270 meters using median value, then raster to polygon (ds_2599_agg270median tif and shp)
      #Then ran through this R code that aggregates polygon with 270m buffer using only Mean_100=92 values (highest/best core area values)

kitfox<-read_sf("Data/1_DataProcessing/CWHR/KitFox/ds2599_agg270median.shp")
kitfox_high<-kitfox %>% 
  dplyr::filter(gridcode>=92)%>% 
  st_transform(crs=crs(kerncounty))%>% #CRS of Kern county
  st_intersection(kc) %>% #Intersected wtih kern county
  group_by() %>% 
  summarise() %>% #Aggregates 
  st_buffer(.,270) %>% # Buffer to solve weird geometry issues
  as(.,"Spatial") %>% 
  disaggregate(.) %>%
  st_as_sf() %>% 
  st_buffer(.,-270 ) %>% 
  dplyr::select() %>% 
  mutate(CircuitID = 1:nrow(.)) #Give each core area unique ID

#Save Kit Fox Highest Shapefiles
# write_sf(kitfox_high, "Data/1_DataProcessing/CWHR/KitFox/KitFox_Highest.shp", delete_layer = TRUE)
# write_sf(kitfox_high, "Data/1_DataProcessing/CoreAreas/KitFox_Highest.shp", delete_layer = TRUE)


#Examine area size of each polygon
area<-kitfox_high %>%  dplyr::mutate(Area_st=st_area(kitfox_high),
                                     Area=rgeos::gArea(as(kitfox_high,"Spatial"), byid=TRUE),
                                     Area_ha=Area/10000)
#Remove polygons with area less than 5 ha (51 out of 225, leaving 174)
kitfox_high_5ha<-area %>% filter(Area_ha>=5) %>% dplyr::select(CircuitID)


#Save Kit Fox Highest, with all polygons greater than 5 hectares
# write_sf(kitfox_high_5ha, "Data/1_DataProcessing/CWHR/KitFox/KitFox_Highest_5ha.shp", delete_layer = TRUE)
# write_sf(kitfox_high_5ha, "Data/1_DataProcessing/CoreAreas/KitFox_Highest_5ha.shp", delete_layer = TRUE)
