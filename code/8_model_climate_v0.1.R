CRUTS = readRDS("Documents/ModellingTrends/Data/Climate/CHELSAcruts/ExtractedRaster_1901_2016.rds")
TS = readRDS("Documents/ModellingTrends/Data/Climate/CHELSAtimeseries/ExtractedRaster_1979_2013.rds")
PREC = readRDS("Documents/ModellingTrends/Data/Climate/CHELSAcruts/ExtractedRaster_1901_2016_prec.rds")


  CombDF = NULL
  for(b in 1901:2016){
    print(b)
    for(c in 1:12){
      message(c)
      MonthChar = as.character(c)
      YearChar = as.character(b)
      
      
      
      TempDF = tryCatch(CRUTS[["tmax"]][[YearChar]][[MonthChar]],
                        error = function(e) e)
      if(inherits(TempDF, "error")){
        TempDF = NA
      } else {
        TempDF = TempDF
      }
      if(sum(is.null(TempDF), is.na(TempDF)) != 1){
        TempDF[TempDF == -32768] = NA
        CRUTS_DFmax = TempDF %>%
          group_by(ID) %>%
          dplyr::summarise(CRUTS_max = (mean(Value, na.rm = T)/10)) 
        CRUTS_DFmax = CRUTS_DFmax[,2]
      } else {
        CRUTS_DFmax = data.frame(CRUTS_max = NA)
      }
      
      
      
      
      TempDF = tryCatch(CRUTS[["tmin"]][[YearChar]][[MonthChar]],
                        error = function(e) e)
      if(inherits(TempDF, "error")){
        TempDF = NA
      } else {
        TempDF = TempDF
      }
      if(sum(is.null(TempDF), is.na(TempDF)) != 1){
      TempDF[TempDF == -32768] = NA
      CRUTS_DFmin = TempDF %>%
        group_by(ID) %>%
        dplyr::summarise(CRUTS_min = (mean(Value, na.rm = T)/10))
      CRUTS_DFmin = CRUTS_DFmin[,2]
      } else {
        CRUTS_DFmin = data.frame(CRUTS_min = NA)
      }
      
      
      
      TempDF = tryCatch(PREC[["prec"]][[YearChar]][[MonthChar]],
                        error = function(e) e)
      if(inherits(TempDF, "error")){
        TempDF = NA
      } else {
        TempDF = TempDF
      }
      if(sum(is.null(TempDF), is.na(TempDF)) != 1){
        TempDF[TempDF == -32768] = NA
        CRUTS_DFprec = TempDF %>%
          group_by(ID) %>%
          dplyr::summarise(CRUTS_prec = (mean(Value, na.rm = T))) 
        CRUTS_DFprec = CRUTS_DFprec[,2]
      } else {
        CRUTS_prec = data.frame(CRUTS_prec = NA)
      }
      
      if(b > 1978 & b < 2014){
        TempDF = tryCatch(TS[["tmax"]][[YearChar]][[MonthChar]],
                          error = function(e) e)
        if(inherits(TempDF, "error")){
          TempDF = NA
        } else {
          TempDF = TempDF
        }
        if(sum(is.null(TempDF), is.na(TempDF)) != 1){
        TempDF[TempDF == -32768] = NA
        TS_DFmax = TempDF %>%
          group_by(ID) %>%
          dplyr::summarise(TS_max = ((mean(Value, na.rm = T)/10))-273.15)
        TS_DFmax = TS_DFmax[,2]
        } else {
          TS_DFmax = data.frame(TS_max = NA)
        }
        
        TempDF = tryCatch(TS[["tmin"]][[YearChar]][[MonthChar]],
                          error = function(e) e)
        if(inherits(TempDF, "error")){
          TempDF = NA
        } else {
          TempDF = TempDF
        }
        if(sum(is.null(TempDF), is.na(TempDF)) != 1){
        TempDF[TempDF == -32768] = NA
        TS_DFmin = TempDF %>%
          group_by(ID) %>%
          dplyr::summarise(TS_min = ((mean(Value, na.rm = T)/10))-273.15)
        TS_DFmin = TS_DFmin[,2]
        } else {
          TS_DFmin = data.frame(TS_min = NA)
        }
        
        
        
        TempDF = tryCatch(TS[["tmean"]][[YearChar]][[MonthChar]],
                          error = function(e) e)
        if(inherits(TempDF, "error")){
          TempDF = NA
        } else {
          TempDF = TempDF
        }
        if(sum(is.null(TempDF), is.na(TempDF)) != 1){
        TempDF[TempDF == -32768] = NA
        TS_DFmean = TempDF %>%
          group_by(ID) %>%
          dplyr::summarise(TS_mean = ((mean(Value, na.rm = T)/10))-273.15)
        TS_DFmean = TS_DFmean[,2]
        } else {
          TS_DFmean = data.frame(TS_mean = NA)
        }
        
        
        
        TempDF = cbind(ID = seq(1,nrow(Coords), 1),
                       CRUTS_DFmin,
                       TS_DFmin,
                       CRUTS_DFmax,
                       TS_DFmax,
                       TS_DFmean,
                       CRUTS_DFprec) 
      } else {
        TS_DFmin = data.frame(TS_min = rep(NA, nrow(Coords)))
        TS_DFmax = data.frame(TS_max = rep(NA, nrow(Coords)))
        TS_DFmean = data.frame(TS_mean = rep(NA, nrow(Coords)))
        TempDF = cbind(ID = seq(1,nrow(Coords), 1),
                       CRUTS_DFmin,
                       TS_DFmin,
                       CRUTS_DFmax,
                       TS_DFmax,
                       TS_DFmean,
                       CRUTS_DFprec) 
      }
      TempDF$Year = b
      TempDF$Month = c
      CombDF = rbind(CombDF, TempDF)
    }
  }
rm(CRUTS, TS, PREC)


  CombDF$CRUTS_mid = (CombDF$CRUTS_min + CombDF$CRUTS_max)/2
  CRUTS_correct = lm(TS_mean ~ CRUTS_mid, data = CombDF)
  summary(CRUTS_correct)
  DF = data.frame(CRUTS_mid = CombDF$CRUTS_mid)
  CombDF$CRUTS_correct = predict(CRUTS_correct, newdata = DF)
  CombDF$MeanAbsError = (CombDF$TS_mean - CombDF$CRUTS_correct)
  CombDF = left_join(CombDF, Coords[,c(1,3,4)])
  
  tmp_df = list()
  for(a in unique(CombDF$ID)){
    print(a)
    tmp = subset(CombDF, ID == a)
    tmp$thorn = thornthwaite(tmp$CRUTS_correct, tmp$Latitude[1], na.rm = T)
    tmp_df[[a]] = data.frame(
      ID = a,
      Year = tmp$Year,
      Month = tmp$Month,
      spei_ind = spei(tmp$CRUTS_prec - tmp$thorn, scale = 1, na.rm = T)$fitted)
    
  }
  tmp_df = data.table::rbindlist(tmp_df)
  tmp_df = as.data.frame(tmp_df)
  CombDF = left_join(CombDF, tmp_df)

saveRDS(CombDF, "Documents/ModellingTrends/Data/Climate/CorrectedClimateEstimates.rds")


