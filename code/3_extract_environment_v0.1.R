#Title: Trim Land use harmonisation data until 1960

library(ncdf4) # package for netcdf manipulation
library(raster) # package for raster manipulation
library(rgdal) # package for geospatial analysis
library(ggplot2) # package for plotting
library(sf)

nc <- nc_open("Documents/ModellingTrends/Data/LUH/states.nc")
print(nc)
v3      <- nc$var[[3]]
varsize <- v3$varsize
ndims   <- v3$ndims
nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!




PrimaryForest = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "primf", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  PrimaryForest[[name]] = Data
}
saveRDS(PrimaryForest, "Documents/ModellingTrends/Data/LUH/PrimaryForest.rds")

PrimaryLand = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "primn", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  PrimaryLand[[name]] = Data
}
saveRDS(PrimaryLand, "Documents/ModellingTrends/Data/LUH/PrimaryLand.rds")

SecondaryForest = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "secdf", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  SecondaryForest[[name]] = Data
}
saveRDS(SecondaryForest, "Documents/ModellingTrends/Data/LUH/SecondaryForest.rds")

SecondaryLand = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "secdn", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  SecondaryLand[[name]] = Data
}
saveRDS(SecondaryLand, "Documents/ModellingTrends/Data/LUH/SecondaryLand.rds")

Pasture = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "pastr", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  Pasture[[name]] = Data
}
saveRDS(Pasture, "Documents/ModellingTrends/Data/LUH/Pasture.rds")

Rangeland = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "range", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  Rangeland[[name]] = Data
}
saveRDS(Rangeland, "Documents/ModellingTrends/Data/LUH/Rangeland.rds")

Urban = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "urban", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  Urban[[name]] = Data
}
saveRDS(Urban, "Documents/ModellingTrends/Data/LUH/Urban.rds")

C3AnuualCrops = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "c3ann", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  C3AnuualCrops[[name]] = Data
}
saveRDS(C3AnuualCrops, "Documents/ModellingTrends/Data/LUH/C3AnnualCrops.rds")

C4AnnualCrops = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "c4ann", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  C4AnnualCrops[[name]] = Data
}
saveRDS(C4AnnualCrops, "Documents/ModellingTrends/Data/LUH/C4AnnualCrops.rds")

C3PerennialCrops = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "c3per", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  C3PerennialCrops[[name]] = Data
}
saveRDS(C3PerennialCrops, "Documents/ModellingTrends/Data/LUH/C3PerennialCrops.rds")

C4PerennialCrops = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "c4per", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  C4PerennialCrops[[name]] = Data
}
saveRDS(C4PerennialCrops, "Documents/ModellingTrends/Data/LUH/C4PerennialCrops.rds")

C3NitFixCrops = list()
for( i in (1166-76):(1166)) {
  # Initialize start and count to read one timestep of the variable.
  start = rep(1,ndims)	# begin with start=(1,1,1,...,1)
  start[ndims] = i	# change to start=(1,1,1,...,i) to read timestep i
  count = varsize	# begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] = 1	# change to count=(nx,ny,nz,...,1) to read 1 tstep
  Data = ncvar_get( nc, "c3nfx", start=start, count=count )
  Data = raster(t(Data))
  crs(Data) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  extent(Data) = c(-180,180,-90,90)
  name = paste('Year: ',i+850,sep='')
  C3NitFixCrops[[name]] = Data
}
saveRDS(C3NitFixCrops, "Documents/ModellingTrends/Data/LUH/C3NitFixCrops.rds")


