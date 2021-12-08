CombinedPoints = readRDS("Documents/ModellingTrends/Data/RandomPoints.rds")
CombinedPoints_moll = CombinedPoints
proj4string(CombinedPoints_moll) <- CRS("+proj=longlat +datum=WGS84")
CombinedPoints_moll <- spTransform(CombinedPoints_moll, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))

Rasters = c(
  "Documents/ModellingTrends/Data/PopDensity/PopDensity1975/GHS_POP_E1975_GLOBE_R2019A_54009_1k_V1_0.tif",
  "Documents/ModellingTrends/Data/PopDensity/PopDensity1990/GHS_POP_E1990_GLOBE_R2019A_54009_1k_V1_0.tif",
  "Documents/ModellingTrends/Data/PopDensity/PopDensity2000/GHS_POP_E2000_GLOBE_R2019A_54009_1k_V1_0.tif",
  "Documents/ModellingTrends/Data/PopDensity/PopDensity2015/GHS_POP_E2015_GLOBE_R2019A_54009_1k_V1_0.tif",
  "D:/LUH(a_u2t1).v1/PrimaryForest.rds",
  "D:/LUH(a_u2t1).v1/PrimaryLand.rds",
  "D:/LUH(a_u2t1).v1/SecondaryForest.rds",
  "D:/LUH(a_u2t1).v1/SecondaryLand.rds",
  "D:/LUH(a_u2t1).v1/Pasture.rds",
  "D:/LUH(a_u2t1).v1/Rangeland.rds",
  "D:/LUH(a_u2t1).v1/Urban.rds",
  "D:/LUH(a_u2t1).v1/C3AnnualCrops.rds",
  "D:/LUH(a_u2t1).v1/C4AnnualCrops.rds",
  "D:/LUH(a_u2t1).v1/C3PerennialCrops.rds",
  "D:/LUH(a_u2t1).v1/C4PerennialCrops.rds",
  "D:/LUH(a_u2t1).v1/C3NitFixCrops.rds")


RasterValuesList = list()
MegaList = list()

for(a in Rasters[1:16]){
  print(paste("File: ", a))
  if(grepl("tif", a)){
    Raster = raster(a)
    RasterValues = 
      tryCatch((data.frame(ID = CombinedPoints_moll@data$ID,
                           Value = raster::extract(Raster,
                                                   CombinedPoints_moll,
                                                   small = T,
                                                   df = T)[,2])),
               error=function(e) e)
    RasterValuesList[[1]] = RasterValues
    MegaList[[a]] = RasterValuesList
  } else{
    RasterList = readRDS(a)
    for(b in seq(1,length(RasterList),1)){
      print(paste("Progress through LUH: ", b))
      Raster = RasterList[[b]]
      RasterValues = 
        tryCatch((data.frame(ID = CombinedPoints@data$ID,
                             Value = raster::extract(Raster,
                                                     CombinedPoints,
                                                     small = T,
                                                     df = T)[,2])),
                 error=function(e) e)
      RasterValuesList[[b]] = RasterValues
      MegaList[[a]] = RasterValuesList
    }
  }
}


saveRDS(MegaList, "Documents/ModellingTrends/Data/Environment/ExtractedRasters.rds")
