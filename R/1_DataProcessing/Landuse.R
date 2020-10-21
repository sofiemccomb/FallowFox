#Script processes and combines all land use datasets: FAM, LandIQ, NLCD
  #Script takes a long time to process so do not run unless you need to rewrite

#Read in packages
###############################
library(tidyverse) #data wrangling
library(sf) #shapefiles
library(raster) #rasters
library(fasterize) #fast rasterize
library(rgeos)
library(cleangeo)
options(scipen = 999) #no scientific notation

######################################################
#Read in Kern County
######################################################
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
#FAM 
######################################################
  #Read in 2011-2017 FAM Datasets, received from Forrest over google drive
  FAM_2011<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2011_Annual_tif_170828.tif")
  FAM_2012<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2012_Annual_tif_170828.tif")
  FAM_2013<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2013_Annual_tif_170828.tif")
  FAM_2014<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2014_Annual_tif_170828.tif")
  FAM_2015<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2015_Annual_tif_170828.tif")
  FAM_2016<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2016_Annual_tif_170828.tif")
  FAM_2017<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_TIF/CA_FAM2017_Annual_tif_171115.tif")
  
  #Set FAM values to 1 (FAM Fallow value changes per year) and all else to NA
  #Each year takes 10 minutes
  #2011
  FAM_2011[FAM_2011!=2]<-NA #Values of 2 are fallow, set all else to NA
  FAM_2011[FAM_2011==2]<-1
  #2012
  FAM_2012[FAM_2012!=10]<-NA #Values of 10 are fallow, set all else to NA
  FAM_2012[FAM_2012==10]<-1
  #2013
  FAM_2013[FAM_2013!=10]<-NA #Values of 10 are fallow, set all else to NA
  FAM_2013[FAM_2013==10]<-1
  #2014
  FAM_2014[FAM_2014!=2]<-NA #Values of 2 are fallow, set all else to NA
  FAM_2014[FAM_2014==2]<-1
  #2015
  FAM_2015[FAM_2015!=3]<-NA #Values of 3 are fallow, set all else to NA
  FAM_2015[FAM_2015==3]<-1
  #2016
  FAM_2016[FAM_2016!=2]<-NA #Values of 2 are fallow, set all else to NA
  FAM_2016[FAM_2016==2]<-1
  #2017
  FAM_2017[FAM_2017!=10]<-NA #Values of 10 are fallow, set all else to NA
  FAM_2017[FAM_2017==10]<-1
  
  #Project Raster FAM  rasters to Kern County raster
  fam_2011<-projectRaster(FAM_2011, ras, method="ngb")
  fam_2012<-projectRaster(FAM_2012, ras, method="ngb")
  fam_2013<-projectRaster(FAM_2013, ras, method="ngb")
  fam_2014<-projectRaster(FAM_2014, ras, method="ngb")
  fam_2015<-projectRaster(FAM_2015, ras, method="ngb")
  fam_2016<-projectRaster(FAM_2016, ras, method="ngb")
  fam_2017<-projectRaster(FAM_2017, ras, method="ngb")
  
  # #Write FAM Rasters (turn on if need to rewrite as process aboves takes awhile)
  # writeRaster(fam_2011, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2011.tif")
  # writeRaster(fam_2012, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2012.tif")
  # writeRaster(fam_2013, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2013.tif")
  # writeRaster(fam_2014, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2014.tif")
  # writeRaster(fam_2015, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2015.tif")
  # writeRaster(fam_2016, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2016.tif")
  # writeRaster(fam_2017, "Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2017.tif")

######################################################
#NLCD
######################################################
  #NLCD 2011
    #clipped to California in ArcGIS for past project and saved in folder (from MLRC CONUS download 4/25/2019 found)
  NLCD_2011<-raster("Data/1_DataProcessing/Landuse/NLCD/CA/NLCD_CA_2011.tif")
  NLCD_2011[NLCD_2011>100]<-NA #Value of 128 is NA in NLCD GIS, NLCD values 0-95, takes 10 min
  nlcd_2011<-resample(NLCD_2011, ras, method="ngb") #Resample to kern county, already correct projection
  # writeRaster(nlcd_2011, "Data/1_DataProcessing/Landuse/NLCD/NLCD_Kern_2011.tif")
  
  #NLCD 2016
    #clipped to California in ArcGIS for past project and saved in folder (from MLRC CONUS download 4/25/2019)
  NLCD_2016<-raster("Data/1_DataProcessing/Landuse/NLCD/CA/NLCD_CA_2016.tif")
  NLCD_2016[NLCD_2016>100]<-NA #Value of 128 is NA in NLCD GIS, NLCD values 0-95, takes 10 min
  nlcd_2016<-resample(NLCD_2016, ras, method="ngb") #Resample to kern county, already correct projection
  # writeRaster(nlcd_2016, "Data/1_DataProcessing/Landuse/NLCD/NLCD_Kern_2016.tif")


######################################################
#LandIQ
######################################################
#LandIQ 
  #Downloaded from: https://data.cnra.ca.gov/dataset/statewide-crop-mapping
  
  #Read in LandIQ 2016 and Process
  LANDIQ_2016<-read_sf("Data/1_DataProcessing/Landuse/LandIQ/i15_crop_mapping_2016_shp/i15_crop_mapping_2016.shp") %>%
    st_transform(crs(ras)) %>%
    dplyr::select(Crop2016) %>%
    dplyr::mutate()
  #Give unique code number for each unique crop type
  uniqcrop<-data.frame(Crop2016=unique(LANDIQ_2016$Crop2016))%>% dplyr::mutate(CropID=100+(1:nrow(.)))
  #Add unique number ID to landIQ shapefile
  landiq_2016<-LANDIQ_2016 %>% full_join(uniqcrop, by="Crop2016")
  #Rasterize landIQ 2016 by CropID
  landIQ_2016_ras = fasterize(landiq_2016, ras, field="CropID")
  #Save raster
  # writeRaster(landIQ_2016_ras, "Data/1_DataProcessing/Landuse/LandIQ/LandIQ_Kern_2016.tif")

  #Read in LandIQ 2014 and Process
  LANDIQ_2014<-read_sf("Data/1_DataProcessing/Landuse/LandIQ/atlas_i15_cropmapping2014/i15_Crop_Mapping_2014_Final_LandIQonAtlas.shp")  %>%
    st_transform(crs(ras)) %>%
    dplyr::select(Crop2014)
  #Give unique code number for each unique crop type
    #To match the unique crop numbers given by 2016 I will use csv I made of land use values and Kit Fox resistance values (based on literature and number assignment)
    #LandIQ crop values derived from unique crop type for 2016 Land IQ above (just copied/pasted)
  uniqcroptype<-read_csv("Data/1_DataProcessing/Resistance/KitFox/ResistanceValues.csv") %>% #Crop numbers created from 2016 LandIQ
    dplyr::filter(Value>=100) %>% dplyr::select(Value, Crop)
  uniqcrop<-data.frame(Crop2014=unique(LANDIQ_2014$Crop2014)) %>% full_join(uniqcroptype, by=c("Crop2014"="Crop"))
  #Add unique number ID to landIQ shapefile
  landiq_2014<-LANDIQ_2014 %>% full_join(uniqcrop, by="Crop2014")
  #Rasterize landIQ 2014 by CropID
  landIQ_2014_ras = fasterize(landiq_2014, ras, field="Value")
  #Save raster
  # writeRaster(landIQ_2014_ras, "Data/1_DataProcessing/Landuse/LandIQ/LandIQ_Kern_2014.tif", overwrite=TRUE)

######################################################
#Combined land use datasets
######################################################
#Combine every year of FAM with 2016 land IQ and 2016 NLCD
  #Also create one for 2011 FAM and 2014 LandIQ and 2016 NLCD
#Precedence: FAM, then LandIQ, then NLCD

#Read in created rasters
fam_2011<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2011.tif")
fam_2012<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2012.tif")
fam_2013<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2013.tif")
fam_2014<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2014.tif")
fam_2015<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2015.tif")
fam_2016<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2016.tif")
fam_2017<-raster("Data/1_DataProcessing/Landuse/FAM/FAM_Fallow_Kern_2017.tif")
nlcd_2016<-raster("Data/1_DataProcessing/Landuse/NLCD/NLCD_Kern_2016.tif")
nlcd_2011<-raster("Data/1_DataProcessing/Landuse/NLCD/NLCD_Kern_2011.tif")
landIQ_2016_ras<-raster("Data/1_DataProcessing/Landuse/LandIQ/LandIQ_Kern_2016.tif")
landIQ_2014_ras<-raster("Data/1_DataProcessing/Landuse/LandIQ/LandIQ_Kern_2014.tif")

#Combine rasters (fill in NAs of first raster, with values from second raster, and so on)
landuse_2011<-cover(fam_2011, landIQ_2016_ras, nlcd_2016)
landuse_2011comp<-cover(fam_2011, landIQ_2014_ras, nlcd_2011)
landuse_2012<-cover(fam_2012, landIQ_2016_ras, nlcd_2016)
landuse_2013<-cover(fam_2013, landIQ_2016_ras, nlcd_2016)
landuse_2014<-cover(fam_2014, landIQ_2016_ras, nlcd_2016)
landuse_2015<-cover(fam_2015, landIQ_2016_ras, nlcd_2016)
landuse_2016<-cover(fam_2016, landIQ_2016_ras, nlcd_2016)
landuse_2017<-cover(fam_2017, landIQ_2016_ras, nlcd_2016)

######################################################
#Subset Land Use Rasters to Kern County, and save
######################################################
  #2011
    landuse_2011_crop<-raster::crop(landuse_2011, extent(kerncounty))#crop to county
    landuse_2011_mask<-raster::mask(landuse_2011_crop, kerncounty)#mask to county
    # writeRaster(landuse_2011_mask, "Data/1_DataProcessing/Landuse/Landuse_2011.tif", overwrite=TRUE)
  #2011 compare
    landuse_2011_comp_crop<-raster::crop(landuse_2011comp, extent(kerncounty))#crop to county
    landuse_2011_comp_mask<-raster::mask(landuse_2011_comp_crop, kerncounty)#mask to county
    # writeRaster(landuse_2011_comp_mask, "Data/1_DataProcessing/Landuse/Landuse_2011_compare.tif", overwrite=TRUE)
  #2012
    landuse_2012_crop<-raster::crop(landuse_2012, extent(kerncounty))#crop to county
    landuse_2012_mask<-raster::mask(landuse_2012_crop, kerncounty)#mask to county
    # writeRaster(landuse_2012_mask, "Data/1_DataProcessing/Landuse/Landuse_2012.tif", overwrite=TRUE)
  #2013
    landuse_2013_crop<-raster::crop(landuse_2013, extent(kerncounty))#crop to county
    landuse_2013_mask<-raster::mask(landuse_2013_crop, kerncounty)#mask to county
    # writeRaster(landuse_2013_mask, "Data/1_DataProcessing/Landuse/Landuse_2013.tif", overwrite=TRUE)
  #2014
    landuse_2014_crop<-raster::crop(landuse_2014, extent(kerncounty))#crop to county
    landuse_2014_mask<-raster::mask(landuse_2014_crop, kerncounty)#mask to county
    # writeRaster(landuse_2014_mask, "Data/1_DataProcessing/Landuse/Landuse_2014.tif", overwrite=TRUE)
  #2015
    landuse_2015_crop<-raster::crop(landuse_2015, extent(kerncounty))#crop to county
    landuse_2015_mask<-raster::mask(landuse_2015_crop, kerncounty)#mask to county
    # writeRaster(landuse_2015_mask, "Data/1_DataProcessing/Landuse/Landuse_2015.tif", overwrite=TRUE)
  #2016
    landuse_2016_crop<-raster::crop(landuse_2016, extent(kerncounty))#crop to county
    landuse_2016_mask<-raster::mask(landuse_2016_crop, kerncounty)#mask to county
    # writeRaster(landuse_2016_mask, "Data/1_DataProcessing/Landuse/Landuse_2016.tif", overwrite=TRUE)
  #2017
    landuse_2017_crop<-raster::crop(landuse_2017, extent(kerncounty))#crop nlcd raster to county
    landuse_2017_mask<-raster::mask(landuse_2017_crop, kerncounty)#mask nlcd raster to county
    # writeRaster(landuse_2017_mask, "Data/1_DataProcessing/Landuse/Landuse_2017.tif", overwrite=TRUE)
