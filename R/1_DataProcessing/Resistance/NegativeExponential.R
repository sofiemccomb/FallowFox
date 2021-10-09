##Create Kit Fox resistance layers for Kit Fox in Kern County with Negative Exponential Function

#Read in packages
###############################
library(tidyverse) #data wrangling
library(sf) #shapefiles
library(raster) #rasters
library(fasterize) #fast rasterize
library(rgeos)
library(cleangeo)
options(scipen = 999) #no scientific notation


# Keeley et al. 2017: Habitat suitability is a poor proxy for landscape connectivity during dispersal and mating movements
  #Negative exponentials--habitat suitability to resistance layers
    #"To transform suitability values into resistance values we used eight curves 
    # based on the transformation function (2)to define the relationship between suitability and resistance (Trainor et al., 2013)."
#Transformation function: 
#100–99 ∗ ((1-exp(-c ∗ h))/(1-exp(-c)))
  #where h is the suitability matrix and c = 0.25, 0.5, 1, 2, 4, 8, 16 or 32.
  # For each ofthe 8 transformations, resistance = 1 when suitability = 1 and resistance = 100 when suitability = 0.
  #At c = 0.25 the relationship# is nearly linear, but as c increases, resistance values become an # increasingly nonlinear negative exponential function of suitability


#Read in packages
###############################
library(tidyverse) #data wrangling
library(sf) #shapefiles
library(raster) #rasters
library(fasterize) #fast rasterize
library(rgeos)
library(cleangeo)
options(scipen = 999) #no scientific notation

#Read in Kern County
##########################
#Kern County
Kern<-read_sf("Data/1_DataProcessing/KernCounty/Kern.shp")
kerncounty <- sf:::as_Spatial(Kern)#sf back to sp 

#Create Kern County raster that will be used to rasterize all groups
#Initialize empty rasters
ras<-raster()
#Set the raster extent
extent(ras) = extent(kerncounty)
#Set raster resolution (meters)
res(ras) = 30
#Define the CRS
crs(ras) = crs(kerncounty)



#Reclassify Land Use as habitat suitability values
#####################################################
#Examine and Create Reclassification Scheme for KitFox
#Based on this paper for Kit Fox Habitat Suitability
#https://www.canids.org/CBC/16/san_joaquin_kit_fox_habitat_suitability.pdf

#Original Land Cover: FAM (1), NLCD (0, 11-95), and Land IQ (101-147)
#0=Unclassified, 1=Fallow (defined from FAM),
#11=Open Water, 12=Perennial Ice/Snow, 21=Developed, Open Space, 22=Developed, Low Intensity,
#23=Developed, Medium Intensity, 24=Developed, High Intensity, 31=Barren Land (Rock/Sand/Clay),
#41=Deciduous Forest, 42=Evergreen Forest, 43=Mixed Forest, 52=Shrub/Scrub, 71=Grassland/Herbaceous,
#81=Pasture/Hay, 82=Cultivated Crops, 90=Woody Wetlands, 95=Herbaceous Wetlands
#And then 101-147 as different Land IQ crop types

#Read in landuse layers  
lc_2011<-raster("Data/1_DataProcessing/Landuse/Landuse_2011.tif")
lc_2015<-raster("Data/1_DataProcessing/Landuse/Landuse_2015.tif")
lc_2017<-raster("Data/1_DataProcessing/Landuse/Landuse_2017.tif")

#Read in habitat suitability values table, derived from Kit Fox Paper
#Fallow and Idle Separate
kf_rclmat<-read_csv("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues.csv")%>% 
  dplyr::mutate(Value_Suit=Suitability/100) %>% 
  dplyr::select(Value_Low, Value_High, Value_Suit) %>% 
  as.matrix()

#Reclassify land use as habitat suitability
rc_2011 <- reclassify(lc_2011, kf_rclmat)#Reclassify land use
rc_2015 <- reclassify(lc_2015, kf_rclmat)#Reclassify land use
rc_2017 <- reclassify(lc_2017, kf_rclmat)#Reclassify land use

#Resample to ras again to ensure same extent for all
rc_all_2011 <- resample(rc_2011,ras, method="ngb")
writeRaster(rc_all_2011,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/SuitabilityValues_2011.tif", overwrite = T)
rc_all_2015 <- resample(rc_2015,ras, method="ngb")
writeRaster(rc_all_2015,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/SuitabilityValues_2015.tif", overwrite = T)
rc_all_2017 <- resample(rc_2017,ras, method="ngb")
writeRaster(rc_all_2017,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/SuitabilityValues_2017.tif", overwrite = T)

#######################################################



#############################################
# Roads
#############################################
# Downloading TIGER 2019 Primary and Secondary Roads for CA here:
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=Roads
roads_sf<-read_sf("Data/1_DataProcessing/Roads/tl_2019_06_prisecroads/tl_2019_06_prisecroads.shp") %>%
  st_transform(crs(ras)) %>% 
  mutate(Value=ifelse(MTFCC=="S1100", 0, 0.1)) %>% 
  dplyr::select(Value)
#Primary roads at 0, Secondary Roads at 0.1

#Rasterize instead of fasterize since they are linestrings
roads_ras = rasterize(roads_sf, ras, field="Value", background=NA)
# Value of 0 or 0.1 for where Roads are--suitability of 0-0.1, background NA

# Crop and Mask to Kern County
roads_mask= mask(roads_ras, kerncounty)
writeRaster(roads_mask,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/Roads_ResistanceValues.tif", overwrite = T)


##############################################
#Rivers
##############################################
#Downloading National Hydrologic Dataset for CA here:
#https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
rivers_sf<-read_sf("Data/1_DataProcessing/Rivers/NHD_H_California_State_Shape/Shape/NHDArea.shp") %>%
  st_transform(crs(ras)) %>% 
  filter(!FType%in%c(318,403,484)) %>% #According to NHDArea User Guide, 318 is Bridges, 403 is Inundation Areas, 484 is wash
  mutate(Value=0) %>% dplyr::select(Value) #Suitability of 0
#According to NHD AREA Feature Catalog removed select Ftypes
#318--Bridges
#403--Inundation areas, area of land subject fo flooding (but not all year)
#484--Wash, which is the usually dry portion of streambed, oly water during rainstorm

#Rasterize rivers to raster
rivers_ras = fasterize(rivers_sf, ras, field="Value", background=NA)
#Value of 0 for where rivers are because not suitable, set background suitablity to NA

# Crop and Mask to Kern County
rivers_mask= mask(rivers_ras, kerncounty)
writeRaster(rivers_mask,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/Rivers_ResistanceValues.tif", overwrite = T)
##############################################

##############################################
#Canals and Aqueducts Local
##############################################
#https://atlas-dwr.opendata.arcgis.com/datasets/b788fb2628844f54b92e46dac5bb7229_0/about
#Downloaded shapefile from website September 5, 2021 
irrigcanals_sf<-read_sf("Data/1_DataProcessing/IrrigationCanals/Canals_and_Aqueducts_local/Canals_and_Aqueducts_local.shp") %>%
  st_transform(crs(ras)) %>% 
  mutate(Value=0) %>% 
  dplyr::select(Value)

#Rasterize instead of fasterize since they are linestrings
irrigcanals_ras = rasterize(irrigcanals_sf, ras, field="Value", background=NA)
# Value of 0 for where irrigation canals are--0 suitability

# Crop and Mask to Kern County
irrigcanals_mask= mask(irrigcanals_ras, kerncounty)
writeRaster(irrigcanals_mask,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/Canals_ResistanceValues.tif", overwrite = T)


#####################################################

#####################################################
#Barriers
##############################################
#Precedence to roads, then irrigation canals, then rivers for values given
#So that if secondary roads pass over rivers, lower resistance

#Precedence
barriers<-cover(roads_mask, irrigcanals_mask, rivers_mask)

writeRaster(barriers, "Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/Barriers_ResistanceValues.tif", overwrite = T)

#####################################################

##############################################
#Read back in all needed files
##############################################
rc_all_2011<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/SuitabilityValues_2011.tif")
rc_all_2015<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/SuitabilityValues_2015.tif")
rc_all_2017<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/SuitabilityValues_2017.tif")
barriers<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/Barriers_ResistanceValues.tif")



####################################################################
#Resistance Layer
##############################################
#Calculate min suitability value across layers
#2011
res_stack_2011 <- stack(rc_all_2011, barriers)
min_suitability_2011 <- min(res_stack_2011,na.rm = TRUE)
writeRaster(min_suitability_2011,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/min_suit_2011.tif",overwrite=T)
#2015
res_stack_2015 <- stack(rc_all_2015, barriers)
min_suitability_2015 <- min(res_stack_2015,na.rm = TRUE)
writeRaster(min_suitability_2015,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/min_suit_2015.tif",overwrite=T)
#2017
res_stack_2017 <- stack(rc_all_2017, barriers)
min_suitability_2017 <- min(res_stack_2017,na.rm = TRUE)
writeRaster(min_suitability_2017,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/min_suit_2017.tif",overwrite=T)

#Calculate resistance surface using Trainor et al. 2013, Keeley et al. 2017 negative expoential transformation function:
#Transformation function: 
  #100–99 ∗ ((1-exp(-c ∗ h))/(1-exp(-c)))
  #where h is the suitability matrix with values 0-1 and c = 0.25, 0.5, 1, 2, 4, 8, 16 or 32
#Do for each year for all c values of 0.25, 4, 8
# Do 4 and 8--Zeller et al. 2018--cited Keeley
#Values produced r=1 when h=1, and r=100 when h=0. AT c=0.25 relationship is linear

#Define c value variables
  c.25=0.25
  c4=4
  c8=8

  #Scale resistance out of 1000 [works better in circuitscape]--NO SLOPE ADJUSTMENT
#C.25
resistance_2011_c.25<-(100-99 * ((1-exp(-c.25 * min_suitability_2011))/(1-exp(-c.25))))*10
resistance_2015_c.25<-(100-99 * ((1-exp(-c.25 * min_suitability_2015))/(1-exp(-c.25))))*10
resistance_2017_c.25<-(100-99 * ((1-exp(-c.25 * min_suitability_2017))/(1-exp(-c.25))))*10
#C4
resistance_2011_c4<-(100-99 * ((1-exp(-c4 * min_suitability_2011))/(1-exp(-c4))))*10
resistance_2015_c4<-(100-99 * ((1-exp(-c4 * min_suitability_2015))/(1-exp(-c4))))*10
resistance_2017_c4<-(100-99 * ((1-exp(-c4 * min_suitability_2017))/(1-exp(-c4))))*10
#C8
resistance_2011_c8<-(100-99 * ((1-exp(-c8 * min_suitability_2011))/(1-exp(-c8))))*10
resistance_2015_c8<-(100-99 * ((1-exp(-c8 * min_suitability_2015))/(1-exp(-c8))))*10
resistance_2017_c8<-(100-99 * ((1-exp(-c8 * min_suitability_2017))/(1-exp(-c8))))*10

#Aggregate rasters to 270 m resolution for faster processing- Factor of 9
  #C.25
  res2011_c.25<-raster::aggregate(resistance_2011_c.25, fact=9)
  res2015_c.25<-raster::aggregate(resistance_2015_c.25, fact=9)
  res2017_c.25<-raster::aggregate(resistance_2017_c.25, fact=9)
  #C4
  res2011_c4<-raster::aggregate(resistance_2011_c4, fact=9)
  res2015_c4<-raster::aggregate(resistance_2015_c4, fact=9)
  res2017_c4<-raster::aggregate(resistance_2017_c4, fact=9)
  #C8
  res2011_c8<-raster::aggregate(resistance_2011_c8, fact=9)
  res2015_c8<-raster::aggregate(resistance_2015_c8, fact=9)
  res2017_c8<-raster::aggregate(resistance_2017_c8, fact=9)

# Make sure all NA values are -9999 for Circuitscape
  #C.25
  NAvalue(res2011_c.25) <- -9999
  NAvalue(res2015_c.25) <- -9999
  NAvalue(res2017_c.25) <- -9999
  #C4
  NAvalue(res2011_c4) <- -9999
  NAvalue(res2015_c4) <- -9999
  NAvalue(res2017_c4) <- -9999
  #C8
  NAvalue(res2011_c8) <- -9999
  NAvalue(res2015_c8) <- -9999
  NAvalue(res2017_c8) <- -9999

# # Write output ASCII
#   #C.25
#   writeRaster(res2011_c.25,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2011_c.25.asc",format="ascii",overwrite=T)
#   writeRaster(res2015_c.25,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2015_c.25.asc",format="ascii",overwrite=T)
#   writeRaster(res2017_c.25,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2017_c.25.asc",format="ascii",overwrite=T)
#   #C4
#   writeRaster(res2011_c4,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2011_c4.asc",format="ascii",overwrite=T)
#   writeRaster(res2015_c4,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2015_c4.asc",format="ascii",overwrite=T)
#   writeRaster(res2017_c4,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2017_c4.asc",format="ascii",overwrite=T)
#   #C8
#   writeRaster(res2011_c8,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2011_c8.asc",format="ascii",overwrite=T)
#   writeRaster(res2015_c8,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2015_c8.asc",format="ascii",overwrite=T)
#   writeRaster(res2017_c8,"Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2017_c8.asc",format="ascii",overwrite=T)
