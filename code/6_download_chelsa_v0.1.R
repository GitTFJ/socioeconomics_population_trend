
for(a in c("tmax","tmin")){
  DataType = a
  for(b in 1901:2016){
    Year = as.character(b)
    Directory = (paste(
      "D:/ClimateData/CHELSAcruts/",
      DataType,
      "/",
      Year, 
      sep = ""))
    #dir.create(Directory)
    for(c in c(1:12)){
      Month = c
      A = download.file(paste(
        "https://www.wsl.ch/lud/chelsa/data/timeseries20c/", a, "/CHELSAcruts", 
        DataType, 
        Month, 
        Year,
        "V.1.0.tif", 
        sep = "_"),
        destfile = paste(Directory,"/", Month, ".tif", sep = ""),
        mode = "wb")
      
    }
  }
}

pause_seconds <- function(x){
  print(paste("Restarts at: ", (Sys.time() + x)))
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
  


a = "prec"
DataType = a
for(b in 1901:2016){
  print(b)
  Year = as.character(b)
  Directory = (paste(
    "D:/ClimateData/CHELSAcruts/",
    DataType,
    "/",
    Year, 
    sep = ""))
  dir.create(Directory, showWarnings = F)
  for(c in c(1:12)){
    Month = c
    
    df = tryCatch({
      A = download.file(paste(
        "ftp://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/chelsa_cruts/prec/CHELSAcruts",
        DataType, 
        Month, 
        Year,
        "V.1.0.tif", 
        sep = "_"),
        destfile = paste(Directory,"/", Month, ".tif", sep = ""),
        mode = "wb")
    }, error = function(error_condition) {
      message(paste("Error occured at ", (a/nrow(acc))*100, "%. Pausing. Next retry at ", Sys.time() + (60), sep = ""))
      pause_seconds(60)
      A = download.file(paste(
        "ftp://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/chelsa_cruts/prec/CHELSAcruts",
        DataType, 
        Month, 
        Year,
        "V.1.0.tif", 
        sep = "_"),
        destfile = paste(Directory,"/", Month, ".tif", sep = ""),
        mode = "wb")
    })
    
  }
}


for(a in c("tmax","tmin","tmean")){
  DataType = a
  for(b in 1979:2013){
    Year = as.character(b)
    Directory = (paste(
      "D:/ClimateData/CHELSAtimeseries/",
      DataType,
      "/",
      Year, 
      sep = ""))
    dir.create(Directory)
    for(c in c(1:12)){
      Month = c
      if(nchar(Month) < 2){
        MonthExpand = paste("0",Month, sep = "")
      } else {
        MonthExpand = Month
      }
      A = download.file(paste(
        "https://envidatrepo.wsl.ch/uploads/chelsa/chelsa_V1/timeseries/", a, "/CHELSA_", 
        DataType,
        "_",
        Year,
        "_",
        MonthExpand,
        "_V1.2.1.tif", 
        sep = ""),
        destfile = paste(Directory,"/", Month, ".tif", sep = ""),
        mode = "wb")
    }
  }
}



