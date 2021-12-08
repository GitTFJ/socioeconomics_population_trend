library(raster)
library(rgeos)
library(sf)
library(sp)

PA = readRDS("Documents/ModellingTrends/Data/ProtectedAreas/PA_simple.rds")
PA = subset(PA, MARINE == "terrestrial")
PA = st_transform(PA, crs = "+proj=moll +lon_0=0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

library(fasterize)
r <- raster(as(PA, "Spatial"), ncols = (17554601 + 17425633)/1000, nrows = (8688805 + 8764475)/1000)
for(a in 1960:2020){
  PATemp = subset(PA, STATUS_YR <= a)
  fr <- fasterize(PATemp, r)
  saveRDS(fr, paste("Documents/ModellingTrends/Data/ProtectedAreas/ProtectedAreaRaster_",a,".rds",sep = ""))
}


TempTrends = Trends
TempTrends = TempTrends[order(TempTrends$Study_year_end),] 
Polygons = readRDS("Documents/ModellingTrends/Data/Polygons.rds")
CombDF = NULL
LastEndYear = TempTrends$Study_year_end[1] 
Rast = readRDS(paste("Documents/ModellingTrends/Data/ProtectedAreas/ProtectedAreaRaster_",LastEndYear,".rds",sep = ""))
for(a in 1:nrow(TempTrends)){
  id = TempTrends$ID[a]
  message(a)
  EndYear = TempTrends$Study_year_end[a]
  if(EndYear > LastEndYear){
    Rast = readRDS(paste("Documents/ModellingTrends/Data/ProtectedAreas/ProtectedAreaRaster_",EndYear,".rds",sep = ""))
    print(paste("skipping to:", EndYear))
  } else {
    print(paste("Continue - year:", EndYear))
  }
  Base = Polygons[[as.numeric(id)]]
  Base = spTransform(Base, "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
  Value <- data.frame(Values = (mask(crop(Rast, extent(Base)), Base))@data@values , ID = id)
  Summ = Value %>%
    group_by(ID) %>%
    dplyr::summarise(N = n(), ProtectedCells = sum(Values, na.rm = T))
  CombDF = rbind(CombDF, Summ)
  LastEndYear = EndYear
}

saveRDS(CombDF, "Documents/ModellingTrends/Data/ProtectedAreas/ProtectedAreaEstimates.rds")

