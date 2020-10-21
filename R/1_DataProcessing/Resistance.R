#Create Kit Fox resistance layers for Kit Fox in Kern County

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
  lc_2011_comp<-raster("Data/1_DataProcessing/Landuse/Landuse_2011_compare.tif")
  lc_2012<-raster("Data/1_DataProcessing/Landuse/Landuse_2012.tif")
  lc_2013<-raster("Data/1_DataProcessing/Landuse/Landuse_2013.tif")
  lc_2014<-raster("Data/1_DataProcessing/Landuse/Landuse_2014.tif")
  lc_2015<-raster("Data/1_DataProcessing/Landuse/Landuse_2015.tif")
  lc_2016<-raster("Data/1_DataProcessing/Landuse/Landuse_2016.tif")
  lc_2017<-raster("Data/1_DataProcessing/Landuse/Landuse_2017.tif")

#Read in resistance values table, derived from Kit Fox Paper
#Fallow and Idle Separate
kf_rclmat<-read_csv("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues.csv") %>% 
  dplyr::select(Value_Low, Value_High, Value_Res) %>% 
  as.matrix()
#Reclassify land use
  rc_2011 <- reclassify(lc_2011, kf_rclmat)#Reclassify land use
  rc_2011comp <- reclassify(lc_2011_comp, kf_rclmat)#Reclassify land use
  rc_2012 <- reclassify(lc_2012, kf_rclmat)#Reclassify land use
  rc_2013 <- reclassify(lc_2013, kf_rclmat)#Reclassify land use
  rc_2014 <- reclassify(lc_2014, kf_rclmat)#Reclassify land use
  rc_2015 <- reclassify(lc_2015, kf_rclmat)#Reclassify land use
  rc_2016 <- reclassify(lc_2016, kf_rclmat)#Reclassify land use
  rc_2017 <- reclassify(lc_2017, kf_rclmat)#Reclassify land use

#Resample to ras again to ensure same extent for all
  rc_all_2011 <- resample(rc_2011,ras, method="ngb")
  # writeRaster(rc_all_2011,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2011.tif", overwrite = T)
  rc_all_2011comp <- resample(rc_2011comp,ras, method="ngb")
  # writeRaster(rc_all_2011comp,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2011comp.tif", overwrite = T)
  rc_all_2012 <- resample(rc_2012,ras, method="ngb")
  # writeRaster(rc_all_2012,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2012.tif", overwrite = T)
  rc_all_2013 <- resample(rc_2013,ras, method="ngb")
  # writeRaster(rc_all_2013,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2013.tif", overwrite = T)
  rc_all_2014 <- resample(rc_2014,ras, method="ngb")
  # writeRaster(rc_all_2014,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2014.tif", overwrite = T)
  rc_all_2015 <- resample(rc_2015,ras, method="ngb")
  # writeRaster(rc_all_2015,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2015.tif", overwrite = T)
  rc_all_2016 <- resample(rc_2016,ras, method="ngb")
  # writeRaster(rc_all_2016,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2016.tif", overwrite = T)
  rc_all_2017 <- resample(rc_2017,ras, method="ngb")
  # writeRaster(rc_all_2017,"Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2017.tif", overwrite = T)

#######################################################



#####################################################
#Slope for resistance layer from DEM
  #USDA National Elevation Dataset (NED) at 30m
  #Used https://viewer.nationalmap.gov/advanced-viewer/
  #Selected DEM near Kern, put in cart, and downloaded
  #https://viewer.nationalmap.gov/uget-instructions/index.html
  
  #Read in DEM files
  dem_list <- list.files(path = "Data/1_DataProcessing/DEM/NED",recursive = T,full.names = T)
  dem_rasters <- lapply(dem_list, raster)

  # Merge each dem file into one raster layer
  dem <- do.call(merge,dem_rasters) # This takes ~12-15 minutes
  # writeRaster(dem,"Data/1_DataProcessing/DEM/DEM/DEM_merge.tif", overwrite = TRUE)
  dem<-raster("Data/1_DataProcessing/DEM/DEM/DEM_merge.tif")

  # Reproject DEM to NLCD projection
  dem_proj = projectRaster(dem,crs = crs(ras)) # This takes ~3+ hrs?
  # writeRaster(dem_proj,"Data/1_DataProcessing/DEM/DEM/DEM_proj.tif", overwrite = TRUE)
  dem_proj<-raster("Data/1_DataProcessing/DEM/DEM/DEM_proj.tif")

  # Crop and Mask to Kern County
  dem_crop = crop(dem_proj,extent(kerncounty))

  #Calculate slope in radians
  slope_rad = terrain(dem_crop, opt = 'slope', unit='radians')
  # writeRaster(slope_rad,"Data/1_DataProcessing/DEM/DEM/slope_rad.tif", overwrite = T)
  slope_rad<-raster("Data/1_DataProcessing/DEM/DEM/slope_rad.tif")
  #Resample slope to ras to ensure same extent, origin, crs
  slope_res <- resample(slope_rad,ras)
  # writeRaster(slope_res,"Data/1_DataProcessing/DEM/slope_radians.tif", overwrite = T)

  #Read in slope in radians
  slope_res<-raster("Data/1_DataProcessing/DEM/slope_radians.tif")

  #Calculate percent  and proportion slope: https://www.calcunation.com/calculator/degrees-to-percent.php
    #Percent slope is rise/run *100, and tan(slope in radians) is rise/run- Where 45 deg is 100%
  # percentslope= tan(slope_res)*100
  prop_slope=tan(slope_res)
  slope_mask<-mask(prop_slope, kerncounty)
  # writeRaster(slope_mask, "Data/1_DataProcessing/DEM/slope.tif", overwrite=T)

#############################################
# Roads
  # Downloading TIGER 2019 Primary and Secondary Roads for CA here:
  # https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=Roads
  roads_sf<-read_sf("Data/1_DataProcessing/Roads/tl_2019_06_prisecroads/tl_2019_06_prisecroads.shp") %>%
    st_transform(crs(ras))

  #Rasterize instead of fasterize since they are linestrings
  roads_ras = rasterize(roads_sf, ras, field=1, background=0)
  # Value of 1 for where Roads are, but also resistance values of 1
  # writeRaster(roads_ras,"Data/1_DataProcessing/Roads/roads_ras.tif", overwrite = T)

  # Crop and Mask to Kern County
  roads_mask= mask(roads_ras, kerncounty)
  # writeRaster(roads_mask,"Data/1_DataProcessing/Roads/roads_Kern.tif", overwrite = T)

##############################################
#Rivers
  #Downloading National Hydrologic Dataset for CA here:
  #https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
  rivers_sf<-read_sf("Data/1_DataProcessing/Rivers/NHD_H_California_State_Shape/Shape/NHDArea.shp") %>%
    st_transform(crs(ras))

  #Rasterize rivers to raster
  rivers_ras = fasterize(rivers_sf, ras, field=NULL, background=0)
    #Value of 1 for where rivers are, but also resistance values of 1

  # Crop and Mask to Kern County
  rivers_mask= mask(rivers_ras, kerncounty)
  # writeRaster(rivers_mask,"Data/1_DataProcessing/Rivers/rivers_Kern.tif", overwrite = T)

####################################################################
#Resistance Layer

#Read in data again
  rc_all_2011<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2011.tif")
  rc_all_2011comp<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2011comp.tif")
  rc_all_2012<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2012.tif")
  rc_all_2013<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2013.tif")
  rc_all_2014<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2014.tif")
  rc_all_2015<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2015.tif")
  rc_all_2016<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2016.tif")
  rc_all_2017<-raster("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues_2017.tif")
  slope<-raster("Data/1_DataProcessing/DEM/slope.tif")
  roads<-raster("Data/1_DataProcessing/Roads/roads_Kern.tif")
  rivers<-raster("Data/1_DataProcessing/Rivers/rivers_Kern.tif")

#Calculate max resistance value across layers
  #2011
    res_stack_2011 <- stack(rc_all_2011, roads, rivers)
    max_resistance_2011 <- max(res_stack_2011,na.rm = TRUE)
    # writeRaster(max_resistance_2011,"Data/1_DataProcessing/Resistance/max_2011.tif",overwrite=T)
  #2011 compare
    res_stack_2011comp <- stack(rc_all_2011comp, roads, rivers)
    max_resistance_2011comp <- max(res_stack_2011comp,na.rm = TRUE)
    # writeRaster(max_resistance_2011comp,"Data/1_DataProcessing/Resistance/max_2011comp.tif",overwrite=T)
  #2012
    res_stack_2012 <- stack(rc_all_2012, roads, rivers)
    max_resistance_2012 <- max(res_stack_2012,na.rm = TRUE)
    # writeRaster(max_resistance_2012,"Data/1_DataProcessing/Resistance/max_2012.tif",overwrite=T)
  #2013
    res_stack_2013 <- stack(rc_all_2013, roads, rivers)
    max_resistance_2013 <- max(res_stack_2013,na.rm = TRUE)
    # writeRaster(max_resistance_2013,"Data/1_DataProcessing/Resistance/max_2013.tif",overwrite=T)
  #2014
    res_stack_2014 <- stack(rc_all_2014, roads, rivers)
    max_resistance_2014 <- max(res_stack_2014,na.rm = TRUE)
    # writeRaster(max_resistance_2014,"Data/1_DataProcessing/Resistance/max_2014.tif",overwrite=T)
  #2015
    res_stack_2015 <- stack(rc_all_2015, roads, rivers)
    max_resistance_2015 <- max(res_stack_2015,na.rm = TRUE)
    # writeRaster(max_resistance_2015,"Data/1_DataProcessing/Resistance/max_2015.tif",overwrite=T)
  #2016
    res_stack_2016 <- stack(rc_all_2016, roads, rivers)
    max_resistance_2016 <- max(res_stack_2016,na.rm = TRUE)
    # writeRaster(max_resistance_2016,"Data/1_DataProcessing/Resistance/max_2016.tif",overwrite=T)
  #2017
    res_stack_2017 <- stack(rc_all_2017, roads, rivers)
    max_resistance_2017 <- max(res_stack_2017,na.rm = TRUE)
    # writeRaster(max_resistance_2017,"Data/1_DataProcessing/Resistance/max_2017.tif",overwrite=T)

#Calculate resistance surface using Dickson et al 2017 equation #5 
  resistance_2011 <-((max_resistance_2011+1)^10) + (slope/4)
  resistance_2011comp <-((max_resistance_2011comp+1)^10) + (slope/4)
  resistance_2012 <-((max_resistance_2012+1)^10) + (slope/4)
  resistance_2013 <-((max_resistance_2013+1)^10) + (slope/4)
  resistance_2014 <-((max_resistance_2014+1)^10) + (slope/4)
  resistance_2015 <-((max_resistance_2015+1)^10) + (slope/4)
  resistance_2016 <-((max_resistance_2016+1)^10) + (slope/4)
  resistance_2017 <-((max_resistance_2017+1)^10) + (slope/4)

#Aggregate rasters to 270 m resolution for faster processing- Factor of 9
  res2011<-raster::aggregate(resistance_2011, fact=9)
  res2011comp<-raster::aggregate(resistance_2011comp, fact=9)
  res2012<-raster::aggregate(resistance_2012, fact=9)
  res2013<-raster::aggregate(resistance_2013, fact=9)
  res2014<-raster::aggregate(resistance_2014, fact=9)
  res2015<-raster::aggregate(resistance_2015, fact=9)
  res2016<-raster::aggregate(resistance_2016, fact=9)
  res2017<-raster::aggregate(resistance_2017, fact=9)

# Make sure all NA values are -9999 for Circuitscape
  NAvalue(res2011) <- -9999
  NAvalue(res2011comp) <- -9999
  NAvalue(res2012) <- -9999
  NAvalue(res2013) <- -9999
  NAvalue(res2014) <- -9999
  NAvalue(res2015) <- -9999
  NAvalue(res2016) <- -9999
  NAvalue(res2017) <- -9999

# Write output ASCII
  # writeRaster(res2011,"Data/1_DataProcessing/Resistance/resistance_2011.asc",format="ascii",overwrite=T)
  # writeRaster(res2011comp,"Data/1_DataProcessing/Resistance/resistance_2011compare.asc",format="ascii",overwrite=T)
  # writeRaster(res2012,"Data/1_DataProcessing/Resistance/resistance_2012.asc",format="ascii",overwrite=T)
  # writeRaster(res2013,"Data/1_DataProcessing/Resistance/resistance_2013.asc",format="ascii",overwrite=T)
  # writeRaster(res2014,"Data/1_DataProcessing/Resistance/resistance_2014.asc",format="ascii",overwrite=T)
  # writeRaster(res2015,"Data/1_DataProcessing/Resistance/resistance_2015.asc",format="ascii",overwrite=T)
  # writeRaster(res2016,"Data/1_DataProcessing/Resistance/resistance_2016.asc",format="ascii",overwrite=T)
  # writeRaster(res2017,"Data/1_DataProcessing/Resistance/resistance_2017.asc",format="ascii",overwrite=T)


