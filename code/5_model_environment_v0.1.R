ExtractedRasters = readRDS("Documents/ModellingTrends/Data/Environment/ExtractedRasters.rds")


SummaryJoin = NULL
for(b in 1:4){
  Raster = as.data.frame(ExtractedRasters[[b]][[1]])
  colnames(Raster)[2] = "Value"
  SummariseRaster = Raster %>%
    group_by(ID) %>%
    dplyr::summarise(Median = mean(Value, na.rm = T))
  SummariseRaster$Year = b
  SummaryJoin = rbind(SummaryJoin, SummariseRaster)
}
SummaryJoin$Year[SummaryJoin$Year == 1] = 1975
SummaryJoin$Year[SummaryJoin$Year == 2] = 1990
SummaryJoin$Year[SummaryJoin$Year == 3] = 2000
SummaryJoin$Year[SummaryJoin$Year == 4] = 2015
SummaryJoin$Median[is.na(SummaryJoin$Median)] = 0
PDPredictComb = NULL
for(c in 1:nrow(Coords)){
  print(c)
  TempDF = subset(SummaryJoin, ID == c)
  if(sum(TempDF$Median == 0) == 4){
    PDPredict = data.frame(Year = 1960:2016, 
                           ID = c, 
                           Value = 0, 
                           Var = 0)
  } else{
    Temp = lm(log(Median+1) ~ poly(Year,3), data = TempDF)
    newdat = data.frame(Year = 1960:2016)
    bootfit1 <- bootCase(Temp, function(x)predict(x, newdat), B=100)
    PDPredict = data.frame(Year = 1960:2016, 
                           ID = c, 
                           Value = apply(bootfit1, 2, median), 
                           Var = apply(bootfit1, 2, var))
  }
  PDPredictComb = rbind(PDPredictComb, PDPredict)
}
PDPredictComb$Value = ifelse(PDPredictComb$Value < 0, 0, PDPredictComb$Value)




JoinRaster = NULL
for(b in 5:16){
  SummaryComb = NULL
  for(c in 2:167){
    Raster = as.data.frame(ExtractedRasters[[b]][[c]])
    colnames(Raster)[2] = "Value"
    Summary = Raster %>%
      group_by(ID) %>%
      dplyr::summarise(Median = mean(Value, na.rm = T))
    Summary = as.data.frame(Summary)
    Summary$Year = (1848+c)
    Summary = Summary[,c(1,3,2)]
    SummaryComb = rbind(SummaryComb, Summary)
  }
  if(b == 5){
    JoinRaster = SummaryComb
  } else {
    JoinRaster = left_join(JoinRaster, SummaryComb, by = c("ID", "Year"))
  }
}
colnames(JoinRaster) = c("ID", "Year","PrimaryForest", "PrimaryLand", "SecondaryForest", "SecondaryLand", "Pasture", "Rangeland", "Urban", "C3AnnCrop", "C4AnnCrop", "C3PerCrop", "C4PerCrop", "C3NitFicCrop")
JoinRaster$Primary = rowSums(JoinRaster[,3:4])
JoinRaster$Human = JoinRaster[,9]
JoinRaster$Nature = rowSums(JoinRaster[,c(5,6,8)])
JoinRaster$Ag = rowSums(JoinRaster[,c(7,10:14)])

JoinRaster = subset(JoinRaster, select = c(
  ID,
  Year,
  Primary,
  Human,
  Nature,
  Ag))


EnviroList = list(PD = PDPredictComb,
                  LUH = JoinRaster)
saveRDS(EnviroList, "Documents/ModellingTrends/Data/Environment/EnvironmentEstimates.rds")
