##Create Kit Fox resistance layers for Kit Fox in Kern County on the basis of Naturalness & Human Modification

#Theobold et al. 2010 & 2012: https://conbio-onlinelibrary-wiley-com.ezproxy.library.ubc.ca/doi/10.1111/j.1755-263X.2011.00218.x
# Steps: 
  #1) Compute "Naturalness" as function of land cover types, housing density, presence of roads, and effects of highway traffic, adjusted minimally by canopy cover and slope
  #2) Estimate resistance values for the least-cost calculation using the inverse of the "naturalness" value"
#Degree of human modification H at 270 m resolution by five factors from Theobold et al. 2010
  #H is the max of 
    #c=proportion of land cover modified
    #h=proportion moidfied by residential housing
    #r=proportion modified by roads
    #t=proportion modified by highway traffic
    #e=proportion modified by extractive resources
  #We will just use land cover, roads, rivers, irrigation canals, & impervious cover (along with canopy & slope)
#Table 1 gives the proportion of human-modification for 13 major land cover groups, estimated by calculating the proportion of human-modificaiton by land cover/use types
      # Agricultural cropland	0.68	
      # Agricultural pasture/hay	0.56
      # Developed high intensity	0.85
      # Developed medium intensity	0.76
      # Developed low intensity	0.64
      # Developed open space	0.52
      # Forest	0.07
      # Shrubland	0.05
      # Grassland	0.17
      # Wetlands	0.11
      # Other disturbed lands	0.24%
      # Mine/quarry	0.58	
      # Sparsely vegetated	0.02

#We will compute roads, river, irrigation canals as we have been in baseline, and add impervious cover as well

#Movement resistance values W using degree of human modification H as well as canopy cover and terrain slope as 
#W= H^(1-s+x)



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

######################################################
#Reclassify Land Use as Human Modification Values
#####################################################
#Using combination of 2016 NLCD and 2011, 2015, 2017 FAM
#Original Land Cover: FAM (1), NLCD (0, 11-95)
#0=Unclassified, 1=Fallow (defined from FAM),
#11=Open Water, 12=Perennial Ice/Snow, 21=Developed, Open Space, 22=Developed, Low Intensity,
#23=Developed, Medium Intensity, 24=Developed, High Intensity, 31=Barren Land (Rock/Sand/Clay),
#41=Deciduous Forest, 42=Evergreen Forest, 43=Mixed Forest, 52=Shrub/Scrub, 71=Grassland/Herbaceous,
#81=Pasture/Hay, 82=Cultivated Crops, 90=Woody Wetlands, 95=Herbaceous Wetlands

#Combined land use datasets
################################
#Combine every year of FAM with 2016 NLCD
#Precedence: FAM,  then NLCD

#Read in created rasters
fam_2011<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2011.tif")
fam_2015<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2015.tif")
fam_2017<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2017.tif")
nlcd_2016<-raster("Data/1_DataProcessing/Landuse/NLCD/NLCD_Kern_2016.tif")

#Combine rasters (fill in NAs of first raster, with values from second raster, and so on)
landuse_2011<-cover(fam_2011, nlcd_2016)
landuse_2015<-cover(fam_2015, nlcd_2016)
landuse_2017<-cover(fam_2017,  nlcd_2016)

#Subset Land Use Rasters to Kern County, and save
##############################
#2011
landuse_2011_crop<-raster::crop(landuse_2011, extent(kerncounty))#crop to county
landuse_2011_mask<-raster::mask(landuse_2011_crop, kerncounty)#mask to county
writeRaster(landuse_2011_mask, "Data/1_DataProcessing/Resistance/Revisions/Naturalness/Landuse_2011.tif", overwrite=TRUE)
#2015
landuse_2015_crop<-raster::crop(landuse_2015, extent(kerncounty))#crop to county
landuse_2015_mask<-raster::mask(landuse_2015_crop, kerncounty)#mask to county
writeRaster(landuse_2015_mask, "Data/1_DataProcessing/Resistance/Revisions/Naturalness/Landuse_2015.tif", overwrite=TRUE)
#2017
landuse_2017_crop<-raster::crop(landuse_2017, extent(kerncounty))#crop nlcd raster to county
landuse_2017_mask<-raster::mask(landuse_2017_crop, kerncounty)#mask nlcd raster to county
writeRaster(landuse_2017_mask, "Data/1_DataProcessing/Resistance/Revisions/Naturalness/Landuse_2017.tif", overwrite=TRUE)


#Reclass land use to Human modified resistance
  #Theobold 2012 values, Table 1, Adjusted to scale by *1000, where 0 isnatural and 1000 is highly unmodified/unsuitable
#Proportion of human-modification for 13 major land cover gruops across the US
kf_rclmat <- c(-Inf,0.5,NA, # NA values
               0.5, 1.5, 250, #Fallow -- Natural/Developed, gave value between grass/shrub & pasture/hay, similiar to barren
                10.5,11.5,1000, # Open Water--Natural but not permeable for terrestrial animals
                11.5,12.5,200, # Ice and Snow -- Natural (unlikely in region), gave value of barren
                20.5,21.5,520, # Open Space development -- Developed
                21.5,22.5,640, # Low development -- Developed
                22.5,23.5,760, # Medium Development -- Developed
                23.5,24.5,850, # High Development -- Developed
                29,32,200, # Barren -- Natural, assigned value between shrubland and open sapce
                39,45,70, # Forest -- Natural
                49,55,50, # Shrubland -- Natural
                69,72,170, # Grassland -- Natural
                79,81.5,560, #Pasture/Hay -- Developed  
                81.5,85,680, # Cultivated Crops -- Developed
                89,96,110) # Wetlands -- Natural

#Although says they reclassified built-up, artificial surface, and cultivated areas to 1.0, managed areas, mosaic cropland, and mosaic tree to 0.5, water to 0.3, and remaining classes as Natural cover types as 0

#Reclassify land use as Naturalness resistance
rc_2011 <- reclassify(landuse_2011_mask, kf_rclmat)#Reclassify land use
rc_2015 <- reclassify(landuse_2015_mask, kf_rclmat)#Reclassify land use
rc_2017 <- reclassify(landuse_2017_mask, kf_rclmat)#Reclassify land use

#Resample to ras again to ensure same extent for all--Change name from suitability
rc_all_2011 <- resample(rc_2011,ras, method="ngb")
writeRaster(rc_all_2011,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/LandUseResistance_2011.tif", overwrite = T)
rc_all_2015 <- resample(rc_2015,ras, method="ngb")
writeRaster(rc_all_2015,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/LandUseResistance_2015.tif", overwrite = T)
rc_all_2017 <- resample(rc_2017,ras, method="ngb")
writeRaster(rc_all_2017,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/LandUseResistance_2017.tif", overwrite = T)

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


#############################################
# Roads
# Downloading TIGER 2019 Primary and Secondary Roads for CA here:
# https://www.census.gov/cgi-bin/geo/shapefiles/index.php?year=2019&layergroup=Roads
roads_sf<-read_sf("Data/1_DataProcessing/Roads/tl_2019_06_prisecroads/tl_2019_06_prisecroads.shp") %>%
  st_transform(crs(ras)) %>% 
  mutate(Value=ifelse(MTFCC=="S1100", 1000, 900)) %>% 
  dplyr::select(Value)
#Primary roads at 1000, Secondary Roads at 900

#Rasterize instead of fasterize since they are linestrings
roads_ras = rasterize(roads_sf, ras, field="Value", background=0)
# Value of 1 for where Roads are, but also resistance values of 1

# Crop and Mask to Kern County
roads_mask= mask(roads_ras, kerncounty)
writeRaster(roads_mask,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/roads_Kern.tif", overwrite = T)


##############################################
#Rivers
#Downloading National Hydrologic Dataset for CA here:
#https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
#Downloading National Hydrologic Dataset for CA here:
#https://www.usgs.gov/core-science-systems/ngp/national-hydrography/access-national-hydrography-products
rivers_sf<-read_sf("Data/1_DataProcessing/Rivers/NHD_H_California_State_Shape/Shape/NHDArea.shp") %>%
  st_transform(crs(ras)) %>% 
  filter(!FType%in%c(318,403,484)) %>% #According to NHDArea User Guide, 318 is Bridges, 403 is Inundation Areas, 484 is wash
  mutate(Value=1000) %>% dplyr::select(Value)
#According to NHD AREA Feature Catalog removed select Ftypes
#318--Bridges
#403--Inundation areas, area of land subject fo flooding (but not all year)
#484--Wash, which is the usually dry portion of streambed, oly water during rainstorm

#Rasterize rivers to raster
rivers_ras = fasterize(rivers_sf, ras, field="Value", background=0)

# Crop and Mask to Kern County
rivers_mask= mask(rivers_ras, kerncounty)
writeRaster(rivers_mask,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/rivers_Kern.tif", overwrite = T)

##############################################


#Irrigation Canals
#####################################################
irrigcanals_sf<-read_sf("Data/1_DataProcessing/IrrigationCanals/Canals_and_Aqueducts_local/Canals_and_Aqueducts_local.shp") %>%
  st_transform(crs(ras)) %>% 
  mutate(Value=1000) %>% 
  dplyr::select(Value)

#Rasterize instead of fasterize since they are linestrings
irrigcanals_ras = rasterize(irrigcanals_sf, ras, field="Value", background=0)

# Crop and Mask to Kern County
irrigcanals_mask= mask(irrigcanals_ras, kerncounty)
writeRaster(irrigcanals_mask,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/irrigcanals_Kern.tif", overwrite = T)


#####################################################

#####################################################

#Barriers
#Precedence to roads, then irrigation canals, then rivers for values given
#So that if secondary roads pass over rivers, lower resistance

#Give 0 values for roads and irrigcanals NA so don't overwrite presence of lower two layers
roads_mask[roads_mask<100]<-NA #Keep 1000 and 900 values
irrigcanals_mask[irrigcanals_mask<100]<-NA 
#Precedence
barriers<-cover(roads_mask, irrigcanals_mask, rivers_mask)

writeRaster(barriers, "Data/1_DataProcessing/Resistance/Revisions/Naturalness/Barriers_ResistanceValues.tif", overwrite = T)

#####################################################


#Imperviousness
####################################################################
# Read in NLCD Imperviousness layer--downloaded 2016 to match NLCD
  #Percent Developed Impervious 2016: https://www.mrlc.gov/data/nlcd-2016-percent-developed-imperviousness-conus
impervious <- raster("Data/1_DataProcessing/Impervious/nlcd_2016_impervious_l48_20210604.img")

#Resample to Kern County raster
impervious_ras<-resample(impervious, ras, method="ngb") #Resample to kern county, already correct projection
# Crop and Mask to Kern County
impervious_mask= mask(impervious_ras, kerncounty)*10 #Scale to 1000

# Write output file
writeRaster(impervious_mask, "Data/1_DataProcessing/Impervious/impervious_Kern.tif", overwrite=T)

####################################################################



#Canopy Cover
####################################################################
#Read in NLCD Canopy Cover Layer--download 2016 to match NLCD
  #Canopy: https://www.mrlc.gov/data/nlcd-2016-usfs-tree-canopy-cover-conus 

## This reads in with min/max values from 0 to 255, not 0 - 100 like the NLCD legend indicates
canopy <- raster("Data/1_DataProcessing/Canopy/nlcd_2016_treecanopy_2019_08_31.img")

#Resample to Kern County raster
canopy_ras<-resample(canopy, ras, method="ngb") #Resample to kern county, already correct projection
# Crop and Mask to Kern County
canopy_mask= mask(canopy_ras, kerncounty)

# Rescale values to be from 0 to 100
canopy_mask = canopy_mask/maxValue(canopy_mask)*100
#Write raster
writeRaster(canopy_mask,"Data/1_DataProcessing/Canopy/canopy_Kern.tif",overwrite=T)

####################################################################


#Read in from above
#####################################################
rc_all_2011<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/LandUseResistance_2011.tif")
rc_all_2015<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/LandUseResistance_2015.tif")
rc_all_2017<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/LandUseResistance_2017.tif")

barriers<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/Barriers_ResistanceValues.tif")
imperviousness <- raster("Data/1_DataProcessing/Impervious/impervious_Kern.tif")

canopyCover <- raster("Data/1_DataProcessing/Canopy/canopy_Kern.tif")
slope<-raster("Data/1_DataProcessing/DEM/slope.tif")


#Human Modification Factor
#####################################################
#Calculate max human modification factor value across layers
#2011
res_stack_2011 <- stack(rc_all_2011, barriers, imperviousness)
max_resistance_2011 <- max(res_stack_2011,na.rm = TRUE)
writeRaster(max_resistance_2011,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/max_2011.tif",overwrite=T)
#2015
res_stack_2015 <- stack(rc_all_2015, barriers, imperviousness)
max_resistance_2015 <- max(res_stack_2015,na.rm = TRUE)
writeRaster(max_resistance_2015,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/max_2015.tif",overwrite=T)
#2017
res_stack_2017 <- stack(rc_all_2017, barriers, imperviousness)
max_resistance_2017 <- max(res_stack_2017,na.rm = TRUE)
writeRaster(max_resistance_2017,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/max_2017.tif",overwrite=T)


#Resistance to Movement
#####################################################
canopy_res_p <- canopyCover/maxValue(canopyCover)
slope_res_p = slope/maxValue(slope)

#currently scaling back to down to 1 value
max_resistance_2011<-max_resistance_2011/1000
max_resistance_2015<-max_resistance_2015/1000
max_resistance_2017<-max_resistance_2017/1000

#Calculate movement resistance values W using the degree of human modification (max resistance) as well as slope and canopy cover [Theobold et al. 2012 equation 2]
resistance_2011 <-max_resistance_2011^(1-slope_res_p+canopy_res_p)*1000
resistance_2015 <-max_resistance_2015^(1-slope_res_p+canopy_res_p)*1000
resistance_2017 <-max_resistance_2017^(1-slope_res_p+canopy_res_p)*1000

#Aggregate rasters to 270 m resolution for faster processing- Factor of 9
res2011<-raster::aggregate(resistance_2011, fact=9)
res2015<-raster::aggregate(resistance_2015, fact=9)
res2017<-raster::aggregate(resistance_2017, fact=9)

# Make sure all NA values are -9999 for Circuitscape
NAvalue(res2011) <- -9999
NAvalue(res2015) <- -9999
NAvalue(res2017) <- -9999

# Write output ASCII
# writeRaster(res2011,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/Naturalness_resistance_2011.asc",format="ascii",overwrite=T)
# writeRaster(res2015,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/Naturalness_resistance_2015.asc",format="ascii",overwrite=T)
# writeRaster(res2017,"Data/1_DataProcessing/Resistance/Revisions/Naturalness/Naturalness_resistance_2017.asc",format="ascii",overwrite=T)


