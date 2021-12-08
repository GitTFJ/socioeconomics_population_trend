CombinedPolygon = list()
Buffer = (sqrt(Coords$Area[1]/3.14))*1000
TempCoords = Coords[1,]
coordinates(TempCoords) <- c("Longitude", "Latitude")
proj4string(TempCoords) <- CRS("+proj=longlat +datum=WGS84")
TempCoords <- spTransform(TempCoords, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
as(TempCoords, "SpatialPoints")
TempPolygon = gBuffer(TempCoords, width = Buffer)
TempPolygon = st_as_sf(TempPolygon)
TempPolygon = st_transform(TempPolygon, crs = 4326)
TempPolygon = as(TempPolygon, 'Spatial')
CombinedPolygon[[1]] = TempPolygon
RandomPoints = spsample(TempPolygon@polygons[[1]], n=TempCoords$Points, type = "random") 
CombinedPoints = SpatialPointsDataFrame(RandomPoints, data.frame(ID= rep(1,length(RandomPoints))))
for(a in 2:nrow(Coords)){
  Buffer = (sqrt(Coords$Area[a]/3.14))*1000
  TempCoords = Coords[a,]
  coordinates(TempCoords) <- c("Longitude", "Latitude")
  proj4string(TempCoords) <- CRS("+proj=longlat +datum=WGS84")
  TempCoords <- spTransform(TempCoords, CRS("+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"))
  as(TempCoords, "SpatialPoints")
  TempPolygon = gBuffer(TempCoords, width = Buffer)
  TempPolygon = st_as_sf(TempPolygon)
  TempPolygon = st_transform(TempPolygon, crs = 4326)
  TempPolygon = as(TempPolygon, 'Spatial')
  CombinedPolygon[[a]] = TempPolygon
  RandomPoints = spsample(TempPolygon@polygons[[1]], n=TempCoords$Points, type = "random") 
  RandomPoints = SpatialPointsDataFrame(RandomPoints, data.frame(ID= rep(a,length(RandomPoints))))
  CombinedPoints = rbind(CombinedPoints, RandomPoints)
}
saveRDS(CombinedPolygon, "Documents/ModellingTrends/Data/Polygons.rds")
saveRDS(CombinedPoints, "Documents/ModellingTrends/Data/RandomPoints.rds")
