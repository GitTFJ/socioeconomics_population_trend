CombinedPolygon = readRDS("Documents/ModellingTrends/Data/Polygons.rds")
CombinedPoints = readRDS("Documents/ModellingTrends/Data/RandomPoints.rds")


RasterMonthList = list()
RasterYearList = list()
RasterDataTypeList = list()
for(a in c("tmax", "tmin")){
  DataType = a
  for(b in 1901:2016){
    Year = as.character(b)
    for(c in c(1:12)){
      Month = as.character(c)
      Raster = tryCatch(raster(paste(
        "D:/ClimateData/CHELSAcruts/",
        DataType,
        "/",
        Year,
        "/",
        Month,
        ".tif",
        sep = "")),
        error=function(e) e
      )
      if(!inherits(Raster, "error")){
        message(Sys.time())
        print(paste(DataType, Year, Month, sep = "_"))
        skip_to_next <- FALSE
        RasterValues = 
          tryCatch((data.frame(ID = CombinedPoints@data$ID,
                               Value = raster::extract(Raster,
                                                       CombinedPoints,
                                                       small = T,
                                                       df = T)[,2])),
                   error=function(e) e)
        RasterMonthList[[Month]] = RasterValues
      } else {
        RasterMonthList[[Month]] = NA
      }
    }
    RasterYearList[[Year]] = RasterMonthList
  }
  RasterDataTypeList[[DataType]] = RasterYearList
}

saveRDS(RasterDataTypeList, "D:/ClimateData/CHELSAcruts/Extracted_buff_pts_raster_1901_2016.rds")
saveRDS(RasterDataTypeList, "Documents/ModellingTrends/Data/Climate/CHELSAcruts/ExtractedRaster_1901_2016.rds")



RasterMonthList = list()
RasterYearList = list()
RasterDataTypeList = list()
for(a in c("tmax", "tmin", "tmean")){
  DataType = a
  for(b in 1979:2013){
    Year = as.character(b)
    for(c in c(1:12)){
      Month = as.character(c)
      Raster = tryCatch(raster(paste(
        "D:/ClimateData/CHELSAtimeseries/",
        DataType,
        "/",
        Year,
        "/",
        Month,
        ".tif",
        sep = "")),
        error=function(e) e
      )
      if(!inherits(Raster, "error")){
        message(Sys.time())
        print(paste(DataType, Year, Month, sep = "_"))
        skip_to_next <- FALSE
        RasterValues = 
          tryCatch((data.frame(ID = CombinedPoints@data$ID,
                               Value = raster::extract(Raster,
                                                       CombinedPoints,
                                                       small = T,
                                                       df = T)[,2])),
                   error=function(e) e)
        RasterMonthList[[Month]] = RasterValues
      } else {
        RasterMonthList[[Month]] = NA
      }
    }
    RasterYearList[[Year]] = RasterMonthList
  }
  RasterDataTypeList[[DataType]] = RasterYearList
}
saveRDS(RasterDataTypeList, "D:/ClimateData/CHELSAtimeseries/Extracted_buff_pts_raster_1979_2013.rds")
saveRDS(RasterDataTypeList, "Documents/ModellingTrends/Data/Climate/CHELSAtimeseries/ExtractedRaster_1979_2013.rds")



RasterMonthList = list()
RasterYearList = list()
RasterDataTypeList = list()
for(a in c("prec")){
  DataType = a
  for(b in 1901:2016){
    Year = as.character(b)
    for(c in c(1:12)){
      Month = as.character(c)
      Raster = tryCatch(raster(paste(
        "D:/ClimateData/CHELSAcruts/",
        DataType,
        "/",
        Year,
        "/",
        Month,
        ".tif",
        sep = "")),
        error=function(e) e
      )
      if(!inherits(Raster, "error")){
        message(Sys.time())
        print(paste(DataType, Year, Month, sep = "_"))
        skip_to_next <- FALSE
        RasterValues = 
          tryCatch((data.frame(ID = CombinedPoints@data$ID,
                               Value = raster::extract(Raster,
                                                       CombinedPoints,
                                                       small = T,
                                                       df = T)[,2])),
                   error=function(e) e)
        RasterMonthList[[Month]] = RasterValues
      } else {
        RasterMonthList[[Month]] = NA
      }
    }
    RasterYearList[[Year]] = RasterMonthList
  }
  RasterDataTypeList[[DataType]] = RasterYearList
}

saveRDS(RasterDataTypeList, "D:/ClimateData/CHELSAcruts/Extracted_buff_pts_raster_1901_2016_prec.rds")
saveRDS(RasterDataTypeList, "Documents/ModellingTrends/Data/Climate/CHELSAcruts/ExtractedRaster_1901_2016_prec.rds")
