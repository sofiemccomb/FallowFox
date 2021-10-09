#Compare Resistance Layers

#Read in packages
library(raster)
library(xlsx)

#Read in rasters
  #Resistance_Old
   res2011<-raster("Data/1_DataProcessing/Resistance/resistance_2011.asc")
   res2015<-raster("Data/1_DataProcessing/Resistance/resistance_2015.asc")
   res2017<-raster("Data/1_DataProcessing/Resistance/resistance_2017.asc")

  #Baseline
   res2011_eq1<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2011_eq1.asc")
   res2015_eq1<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2015_eq1.asc")
   res2017_eq1<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2017_eq1.asc")
   
   res2011_eq3<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2011_eq3.asc")
   res2015_eq3<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2015_eq3.asc")
   res2017_eq3<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2017_eq3.asc")
   
   res2011_eq5<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2011_eq5.asc")
   res2015_eq5<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2015_eq5.asc")
   res2017_eq5<-raster("Data/1_DataProcessing/Resistance/Revisions/Baseline/resistance_2017_eq5.asc")
   
  #Negative Exponential
   #C.25
   res2011_c.25<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2011_c.25.asc")
   res2015_c.25<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2015_c.25.asc")
   res2017_c.25<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2017_c.25.asc")
   #C4
   res2011_c4<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2011_c4.asc")
   res2015_c4<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2015_c4.asc")
   res2017_c4<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2017_c4.asc")
   #C8
   res2011_c8<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2011_c8.asc")
   res2015_c8<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2015_c8.asc")
   res2017_c8<-raster("Data/1_DataProcessing/Resistance/Revisions/NegativeExponential/resistance_2017_c8.asc")
   
   #Naturalness
   res2011_naturalness<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/Naturalness_resistance_2011.asc")
   res2015_naturalness<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/Naturalness_resistance_2015.asc")
   res2017_naturalness<-raster("Data/1_DataProcessing/Resistance/Revisions/Naturalness/Naturalness_resistance_2017.asc")
   

#Pearson Correlation Test between Raster Layers
  #https://gis.stackexchange.com/questions/277575/how-to-do-spatial-correlation-between-two-sets-of-rasters-in-r

  res_2011_stack<-stack(res2011,res2011_eq1, res2011_eq3, res2011_eq5, res2011_c.25, res2011_c4, res2011_c8, res2011_naturalness)
  res_2015_stack<-stack(res2015,res2015_eq1, res2015_eq3, res2015_eq5, res2015_c.25, res2015_c4, res2015_c8, res2015_naturalness)
  res_2017_stack<-stack(res2017,res2017_eq1, res2017_eq3, res2017_eq5, res2017_c.25, res2017_c4, res2017_c8, res2017_naturalness)
   
  #2011
  stack2011=layerStats(res_2011_stack, 'pearson', na.rm=T)
  corr_matrix_2011=stack2011$'pearson correlation coefficient'
  corr_matrix_2011
  #2015
  stack2015=layerStats(res_2015_stack, 'pearson', na.rm=T)
  corr_matrix_2015=stack2015$'pearson correlation coefficient'
  corr_matrix_2015
  #2017
  stack2017=layerStats(res_2017_stack, 'pearson', na.rm=T)
  corr_matrix_2017=stack2017$'pearson correlation coefficient'
  corr_matrix_2017
  
 #Print Resistance Rasters 
  #2011
  pdf(file = "Data/1_DataProcessing/Resistance/Revisions/ResistanceComparison/Resistance_ras_2011.pdf", width = 11, height = 8.5)
  for (i in 1:raster::nlayers(res_2011_stack)) {  
    plot(res_2011_stack[[i]], main= names(res_2011_stack)[[i]])
  }
  dev.off()
  #2015
  pdf(file = "Data/1_DataProcessing/Resistance/Revisions/ResistanceComparison/Resistance_ras_2015.pdf", width = 11, height = 8.5)
  for (i in 1:raster::nlayers(res_2015_stack)) {  
    plot(res_2015_stack[[i]], main= names(res_2015_stack)[[i]])
  }
  dev.off()
  #2017
  pdf(file = "Data/1_DataProcessing/Resistance/Revisions/ResistanceComparison/Resistance_ras_2017.pdf", width = 11, height = 8.5)
  for (i in 1:raster::nlayers(res_2017_stack)) {  
    plot(res_2017_stack[[i]], main= names(res_2017_stack)[[i]])
  }
  dev.off()
  
#Save csv of correlation values
  write.xlsx(corr_matrix_2011, file="Data/1_DataProcessing/Resistance/Revisions/ResistanceComparison/Resistance_ras_correlation.xlsx", sheetName="Corr2011", row.names=TRUE)
  write.xlsx(corr_matrix_2015, file="Data/1_DataProcessing/Resistance/Revisions/ResistanceComparison/Resistance_ras_correlation.xlsx", sheetName="Corr2015", append=TRUE, row.names=TRUE)
  write.xlsx(corr_matrix_2017, file="Data/1_DataProcessing/Resistance/Revisions/ResistanceComparison/Resistance_ras_correlation.xlsx", sheetName="Corr2017", append=TRUE, row.names=TRUE)
  