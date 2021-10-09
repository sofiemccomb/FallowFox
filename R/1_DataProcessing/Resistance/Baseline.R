##Create Kit Fox resistance layers for Kit Fox in Kern County 

  #Dickson et al. 2017 approach: https://doi.org/10.1111/conl.12322 
  #Informing Strategic Efforts to Expand & Connect Protected Areas Using a Model of Ecological Flow, with Application to the Western United States
  #Resistance surfaces based on human modification of the landscape & natural barriers
  #SI equations 1, 3, 5
    #R1=1+(H^(0.8-s)^2)x1000--Low permisitivity
    #R3=1+(1000xH^2)+(s/4)--Moderate permisitivity
    #R5=(H+1)^10+(s/4)--High permisitivity
  #Using resistance values derived from habitat suitability linear inverse direct relationship

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


#Reclassify Land Use as resistance values
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

#Read in resistance values table, derived from Kit Fox Paper
#Fallow and Idle Separate
kf_rclmat<-read_csv("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues.csv") %>% 
  dplyr::select(Value_Low, Value_High, Value_Res) %>% 
  as.matrix()
#Reclassify land use
rc_2011 <- reclassify(lc_2011, kf_rclmat)#Reclassify land use
rc_2015 <- reclassify(lc_2015, kf_rclmat)#Reclassify land use
rc_2017 <- reclassify(lc_2017, kf_rclmat)#Reclassify land use

#Resample to ras again to ensure same extent for all
rc_all_2011 <- resample(rc_2011,ras, method="ngb")
writeRaster(rc_all_2011,"Data/1_DataProcessing/Resistance/Revisions/Baseline/Landuse_ResistanceValues_2011.tif", overwrite = T)
rc_all_2015 <- resample(rc_2015,ras, method="ngb")
writeRaster(rc_all_2015,"Data/1_DataProcessing/Resistance/Revisions/Baseline/Landuse_ResistanceValues_2015.tif", overwrite = T)
rc_all_2017 <- resample(rc_2017,ras, method="ngb")
writeRaster(rc_all_2017,"Data/1_DataProcessing/Resistance/Revisions/Baseline/Landuse_ResistanceValues_2017.tif", overwrite = T)

#######################################################


#Slope    #Kept slope processing under DEM folder--commented out since takes awhile
#####################################################
#Slope for resistance layer from DEM
#USDA National Elevation Dataset (NED) at 30m
#Used https://viewer.nationalmap.gov/advanced-viewer/
#Selected DEM near Kern, put in cart, and downloaded
#https://viewer.nationalmap.gov/uget-instructions/index.html

# #Read in DEM files
# dem_list <- list.files(path = "Data/1_DataProcessing/DEM/NED",recursive = T,full.names = T)
# dem_rasters <- lapply(dem_list, raster)
# 
# # Merge each dem file into one raster layer
# dem <- do.call(merge,dem_rasters) # This takes ~12-15 minutes
# # writeRaster(dem,"Data/1_DataProcessing/DEM/DEM/DEM_merge.tif", overwrite = TRUE)
# dem<-raster("Data/1_DataProcessing/DEM/DEM/DEM_merge.tif")
# 
# # Reproject DEM to NLCD projection
# dem_proj = projectRaster(dem,crs = crs(ras)) # This takes ~3+ hrs?
# # writeRaster(dem_proj,"Data/1_DataProcessing/DEM/DEM/DEM_proj.tif", overwrite = TRUE)
# dem_proj<-raster("Data/1_DataProcessing/DEM/DEM/DEM_proj.tif")
# 
# # Crop and Mask to Kern County
# dem_crop = crop(dem_proj,extent(kerncounty))
# 
# #Calculate slope in radians
# slope_rad = terrain(dem_crop, opt = 'slope', unit='radians')
# # writeRaster(slope_rad,"Data/1_DataProcessing/DEM/DEM/slope_rad.tif", overwrite = T)
# slope_rad<-raster("Data/1_DataProcessing/DEM/DEM/slope_rad.tif")
# #Resample slope to ras to ensure same extent, origin, crs
# slope_res <- resample(slope_rad,ras)
# # writeRaster(slope_res,"Data/1_DataProcessing/DEM/slope_radians.tif", overwrite = T)
# 
# #Read in slope in radians
# slope_res<-raster("Data/1_DataProcessing/DEM/slope_radians.tif")
# 
# #Calculate percent  and proportion slope: https://www.calcunation.com/calculator/degrees-to-percent.php
# #Percent slope is rise/run *100, and tan(slope in radians) is rise/run- Where 45 deg is 100%
# # percentslope= tan(slope_res)*100
# prop_slope=tan(slope_res)
# slope_mask<-mask(prop_slope, kerncounty)
# # writeRaster(slope_mask, "Data/1_DataProcessing/DEM/slope.tif", overwrite=T)

##################################################

#Barriers layer: Rivers, Irrigation Canals, Roads
######################################################################################

#Rivers
##############################################
#Downloading National Hydrologic Dataset for CA here:
#https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
  rivers_sf<-read_sf("Data/1_DataProcessing/Rivers/NHD_H_California_State_Shape/Shape/NHDArea.shp") %>%
    st_transform(crs(ras)) %>% 
    filter(!FType%in%c(318,403,484)) %>% #According to NHDArea User Guide, 318 is Bridges, 403 is Inundation Areas, 484 is wash
    mutate(Value=1) %>% dplyr::select(Value)
  #According to NHD AREA Feature Catalog removed select Ftypes
  #318--Bridges
  #403--Inundation areas, area of land subject fo flooding (but not all year)
  #484--Wash, which is the usually dry portion of streambed, oly water during rainstorm
  
  #Rasterize rivers to raster
  rivers_ras = fasterize(rivers_sf, ras, field="Value", background=0)
  #Value of 1 for where rivers are, but also resistance values of 1
  
  # Crop and Mask to Kern County
  rivers_mask= mask(rivers_ras, kerncounty)
  writeRaster(rivers_mask,"Data/1_DataProcessing/Resistance/Revisions/Baseline/Rivers_ResistanceValues.tif", overwrite = T)
  ##############################################


#Canals and Aqueducts Local
##############################################
#https://atlas-dwr.opendata.arcgis.com/datasets/b788fb2628844f54b92e46dac5bb7229_0/about
#Downloaded shapefile from website September 5, 2021 
  irrigcanals_sf<-read_sf("Data/1_DataProcessing/IrrigationCanals/Canals_and_Aqueducts_local/Canals_and_Aqueducts_local.shp") %>%
    st_transform(crs(ras)) %>% 
    mutate(Value=1) %>% 
    dplyr::select(Value)

  #Rasterize instead of fasterize since they are linestrings
  irrigcanals_ras = rasterize(irrigcanals_sf, ras, field="Value", background=0)
  # Value of 1 for where irrigation canals are, but also resistance values of 1
  
  # Crop and Mask to Kern County
  irrigcanals_mask= mask(irrigcanals_ras, kerncounty)
  writeRaster(irrigcanals_mask,"Data/1_DataProcessing/Resistance/Revisions/Baseline/Canals_ResistanceValues.tif", overwrite = T)
  

#####################################################

# Roads
#############################################
# Downloading TIGER 2019 Primary and Secondary Roads for CA here:
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=Roads
  roads_sf<-read_sf("Data/1_DataProcessing/Roads/tl_2019_06_prisecroads/tl_2019_06_prisecroads.shp") %>%
    st_transform(crs(ras)) %>% 
    mutate(Value=ifelse(MTFCC=="S1100", 1, 0.9)) %>% 
    dplyr::select(Value)
    #Primary roads at 1, Secondary Roads at 0.9

  #Rasterize instead of fasterize since they are linestrings
  roads_ras = rasterize(roads_sf, ras, field="Value", background=0)
  # Value of 1 for where Roads are, but also resistance values of 1
  
  # Crop and Mask to Kern County
  roads_mask= mask(roads_ras, kerncounty)
  writeRaster(roads_mask,"Data/1_DataProcessing/Resistance/Revisions/Baseline/Roads_ResistanceValues.tif", overwrite = T)

#####################################################
  
#Barriers
  #Precedence to roads, then irrigation canals, then rivers for values given
    #So that if secondary roads pass over rivers, lower resistance
    
  #Give 0 values for roads and irrigcanals NA so don't overwrite presence of lower two layers
  roads_mask[roads_mask<0.1]<-NA #Keep 1 and 0.9 values
  irrigcanals_mask[irrigcanals_mask<0.1]<-NA #Keep 1 values
  #Precedence
  barriers<-cover(roads_mask, irrigcanals_mask, rivers_mask)

  writeRaster(barriers, "Data/1_DataProcessing/Resistance/Revisions/Baseline/Barriers_ResistanceValues.tif", overwrite = T)
  
#####################################################
#Read back in all needed files
#############
rc_all_2011<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/Landuse_ResistanceValues_2011.tif")
rc_all_2015<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/Landuse_ResistanceValues_2015.tif")
rc_all_2017<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/Landuse_ResistanceValues_2017.tif")
slope<-raster("Data/1_DataProcessing/DEM/slope.tif")
barriers<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/Barriers_ResistanceValues.tif")


#Calculate max resistance value across layers
#####################################################
#2011
res_stack_2011 <- stack(rc_all_2011, barriers)
max_resistance_2011 <- max(res_stack_2011,na.rm = TRUE)
writeRaster(max_resistance_2011,"Data/1_DataProcessing/Resistance/Revisions/Baseline/max_2011.tif",overwrite=T)
#2015
res_stack_2015 <- stack(rc_all_2015, barriers)
max_resistance_2015 <- max(res_stack_2015,na.rm = TRUE)
writeRaster(max_resistance_2015,"Data/1_DataProcessing/Resistance/Revisions/Baseline/max_2015.tif",overwrite=T)
#2017
res_stack_2017 <- stack(rc_all_2017, barriers)
max_resistance_2017 <- max(res_stack_2017,na.rm = TRUE)
writeRaster(max_resistance_2017,"Data/1_DataProcessing/Resistance/Revisions/Baseline/max_2017.tif",overwrite=T)


#Calculate Resistance Surfaces
##############################################
#Calculate resistance surface using Dickson et al 2017 equation #1: R1=1+(H^(0.8-s)^2)x1000--Low permisitivity 
resistance_2011_eq1<-1 + (max_resistance_2011^((0.8-slope)^2)) * 1000
resistance_2015_eq1<-1 + (max_resistance_2015^((0.8-slope)^2)) * 1000
resistance_2017_eq1<-1 + (max_resistance_2017^((0.8-slope)^2)) * 1000

#Calculate resistance surface using Dickson et al 2017 equation #3: R3=1+(1000xH^2)+(s/4)
resistance_2011_eq3<-1 + (1000*(max_resistance_2011^2)) + (slope/4)
resistance_2015_eq3<-1 + (1000*(max_resistance_2015^2)) + (slope/4)
resistance_2017_eq3<-1 + (1000*(max_resistance_2017^2)) + (slope/4)

#Calculate resistance surface using Dickson et al 2017 equation #5: #R5=(H+1)^10+(s/4)
resistance_2011_eq5 <-((max_resistance_2011+1)^10) + (slope/4)
resistance_2015_eq5 <-((max_resistance_2015+1)^10) + (slope/4)
resistance_2017_eq5 <-((max_resistance_2017+1)^10) + (slope/4)


#Aggregate rasters to 270 m resolution for faster processing- Factor of 9
##############################################
res2011_eq1<-raster::aggregate(resistance_2011_eq1, fact=9)
res2015_eq1<-raster::aggregate(resistance_2015_eq1, fact=9)
res2017_eq1<-raster::aggregate(resistance_2017_eq1, fact=9)

res2011_eq3<-raster::aggregate(resistance_2011_eq3, fact=9)
res2015_eq3<-raster::aggregate(resistance_2015_eq3, fact=9)
res2017_eq3<-raster::aggregate(resistance_2017_eq3, fact=9)

res2011_eq5<-raster::aggregate(resistance_2011_eq5, fact=9)
res2015_eq5<-raster::aggregate(resistance_2015_eq5, fact=9)
res2017_eq5<-raster::aggregate(resistance_2017_eq5, fact=9)

# Make sure all NA values are -9999 for Circuitscape
##############################################
NAvalue(res2011_eq1) <- -9999
NAvalue(res2015_eq1) <- -9999
NAvalue(res2017_eq1) <- -9999

NAvalue(res2011_eq3) <- -9999
NAvalue(res2015_eq3) <- -9999
NAvalue(res2017_eq3) <- -9999

NAvalue(res2011_eq5) <- -9999
NAvalue(res2015_eq5) <- -9999
NAvalue(res2017_eq5) <- -9999

# Write output ASCII
##############################################
# writeRaster(res2011_eq1,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2011_eq1.asc",format="ascii",overwrite=T)
# writeRaster(res2015_eq1,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2015_eq1.asc",format="ascii",overwrite=T)
# writeRaster(res2017_eq1,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2017_eq1.asc",format="ascii",overwrite=T)
# 
# writeRaster(res2011_eq3,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2011_eq3.asc",format="ascii",overwrite=T)
# writeRaster(res2015_eq3,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2015_eq3.asc",format="ascii",overwrite=T)
# writeRaster(res2017_eq3,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2017_eq3.asc",format="ascii",overwrite=T)
# 
# writeRaster(res2011_eq5,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2011_eq5.asc",format="ascii",overwrite=T)
# writeRaster(res2015_eq5,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2015_eq5.asc",format="ascii",overwrite=T)
# writeRaster(res2017_eq5,"Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2017_eq5.asc",format="ascii",overwrite=T)
# 
