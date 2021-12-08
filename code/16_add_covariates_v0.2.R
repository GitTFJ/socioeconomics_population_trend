Enviro = readRDS("Documents/ModellingTrends/Data/Environment/EnvironmentEstimates.rds")
Climate = readRDS("Documents/ModellingTrends/Data/Climate/CorrectedClimateEstimates.rds")
Governance = readRDS("Documents/ModellingTrends/Data/WG/GovernanceFormatted.rds")
Traits = readRDS("Documents/ModellingTrends/Data/Traits/TraitsFormatted.rds")[[1]] #Using best estimate of phylogeny
PA = readRDS("Documents/ModellingTrends/Data/ProtectedAreas/ProtectedAreaEstimates.rds")


LUH = Enviro[["LUH"]]
LUH[LUH == "NaN"] = NA
PD = Enviro[["PD"]]
Trends$Quantitative_method = as.character(Trends$Quantitative_method)
Trends$Quantitative_method = ifelse(is.na(Trends$Quantitative_method), "Manual calculation required",Trends$Quantitative_method)
Climate$PET_tho[is.infinite(Climate$PET_tho)] = NA

Lags = c(0,5,10)
TrendsList = list()
for(b in c(1:3)){
  DataFrameComb = NULL  
  for(a in 1:nrow(Trends)){
    print(a)
    StudyStart = Trends$Study_year_start[a]
    StudyEnd = Trends$Study_year_end[a]
    Country = Trends$alpha.3[a]
    Spec = Trends$Species[a]
    ID_ = Trends$ID[a]
    UID = Trends$UniqueID[a]
    PopLat = Trends$Latitude[a]
    Lag = Lags[b]
    #Assign envioirnmental rasters
    
    #Population density
    
    PDtmp = PD[which(
      (PD$Year <= (StudyEnd)) &
        (PD$Year >= (StudyStart - Lag)) &
        (PD$ID == ID_)),]
    PDChange = unname((exp(coef(lm(Value ~ Year, data = PDtmp))[2]) - 1)*100) 
    PDEnd = PD[which(
      (PD$Year == (StudyEnd)) &
        (PD$ID == ID_)),3]


    
    DF = subset(LUH, ID == ID_)
    PrimEnd = DF[which(
      (DF$Year == (StudyEnd)) &
        (DF$ID == ID_)),3]
    Primtmp = DF[which(
      (DF$Year <= (StudyEnd)) &
        (DF$Year >= (StudyStart - Lag)) &
        (DF$ID == ID_)),]
    Primtmp = Primtmp[complete.cases(Primtmp),]
    if(nrow(Primtmp) < 2){
      PriC= NA
      NatC = NA
      AgC = NA
      HumC = NA
    } else {
      PriC = unname((exp(coef(lm(log(Primary + 0.01) ~ Year, data = Primtmp))[2]) - 1)*100)
      NatC = unname((exp(coef(lm(log(Nature + 0.01) ~ Year, data = Primtmp))[2]) - 1)*100)
      AgC = unname((exp(coef(lm(log(Ag + 0.01) ~ Year, data = Primtmp))[2]) - 1)*100)
      HumC = unname((exp(coef(lm(log(Human + 0.01) ~ Year, data = Primtmp))[2]) - 1)*100)
    }

  
    
    #Frequency of extreme-highs
    PreIndTemp_mx_mean = mean(Climate[which(
      (Climate$Year < 1921) &
        (Climate$ID == ID_)),]$CRUTS_max, na.rm = T)
    PreIndTemp_mx_sd = sd(Climate[which(
      (Climate$Year < 1921) &
        (Climate$ID == ID_)),]$CRUTS_max, na.rm = T)
    PreIndTemp_mx_threshold = PreIndTemp_mx_mean + PreIndTemp_mx_sd*2
    PreIndTemp_mx_freq = length(Climate[which(
      (Climate$Year < 1921) &
        (Climate$ID == ID_) &
        Climate$CRUTS_max > PreIndTemp_mx_threshold),]$CRUTS_max)
    PreIndTemp_mx_freq = ifelse(length(PreIndTemp_mx_freq) == 0, 0, PreIndTemp_mx_freq)
    PreIndTemp_mx_freq = PreIndTemp_mx_freq/20
    StudyTemp_mx_freq = length(Climate[which(
      (Climate$Year >= (StudyStart - Lag) & Climate$Year <= StudyEnd) &
        (Climate$ID == ID_) &
        Climate$CRUTS_max > PreIndTemp_mx_threshold),]$CRUTS_max)
    StudyTemp_mx_freq = ifelse(length(StudyTemp_mx_freq) == 0, 0, StudyTemp_mx_freq)
    StudyTemp_mx_freq = StudyTemp_mx_freq/(StudyEnd - (StudyStart - Lag))
    ExHeat = StudyTemp_mx_freq - PreIndTemp_mx_freq
    

    
    
    StudySpei_fl_mean = mean(Climate[which(
      (Climate$Year >= (StudyStart - Lag) & Climate$Year <= StudyEnd) &
        (Climate$ID == ID_)),]$PET_tho, na.rm = T)
    
    #Frequency of extreme-drought
    PreIndSpei_dr_mean = mean(Climate[which(
      (Climate$Year < 1921) &
        (Climate$ID == ID_)),]$PET_tho, na.rm = T)
    PreIndSpei_dr_sd = sd(Climate[which(
      (Climate$Year < 1921) &
        (Climate$ID == ID_)),]$PET_tho, na.rm = T)
    PreIndSpei_dr_threshold = PreIndSpei_dr_mean - PreIndSpei_dr_sd*2
    PreIndSpei_dr_freq = length(Climate[which(
      (Climate$Year < 1921) &
        (Climate$ID == ID_) &
        Climate$MPET_tho < PreIndSpei_dr_threshold),]$PET_tho)
    PreIndSpei_dr_freq = ifelse(length(PreIndSpei_dr_freq) == 0, 0, PreIndSpei_dr_freq)
    PreIndSpei_dr_freq = PreIndSpei_dr_freq/20
    StudySpei_dr_freq = length(Climate[which(
      (Climate$Year >= (StudyStart - Lag) & Climate$Year <= StudyEnd) &
        (Climate$ID == ID_) &
        Climate$Mean < PreIndSpei_dr_threshold),]$PET_tho)
    StudySpei_dr_freq = ifelse(length(StudySpei_dr_freq) == 0, 0, StudySpei_dr_freq)
    StudySpei_dr_freq = StudySpei_dr_freq/(1+ StudyEnd - (StudyStart - Lag))
    DroughtChange = StudySpei_dr_freq - PreIndSpei_dr_freq
    
    #Assign governance
    #HDI
    HDI = Governance[which(
      Governance$Year == StudyEnd & 
        Governance$Code == Country),]$HDI_mean
    HDI_var = Governance[which(
      Governance$Year == StudyEnd & 
        Governance$Code == Country),]$HDI_var
    
    #Governance
    Gov = Governance[which(
      Governance$Year == StudyEnd & 
        Governance$Code == Country),]$Gov_mean
    Gov_var = Governance[which(
      Governance$Year == StudyEnd & 
        Governance$Code == Country),]$Gov_var
    
    Govtmp = Governance[which(
      (Governance$Year <= (StudyEnd)) &
        (Governance$Year >= (StudyStart - Lag)) &
        (Governance$Code == Country)),]
    if(min(Govtmp$Gov_mean) < 0){
      Govtmp$Gov_mean = Govtmp$Gov_mean + abs(min(Govtmp$Gov_mean))
    } else {
    }

    HDI_c = unname((exp(coef(lm(log(HDI_mean + 0.01) ~ Year, data = Govtmp))[2]) - 1)*100)
    Gov_c = unname((exp(coef(lm(log(Gov_mean + 0.01) ~ Year, data = Govtmp))[2]) - 1)*100)
    
    #Conflict present
    Conf = Governance[which(
      Governance$Year > (StudyStart - Lag) &
        Governance$Year < StudyEnd & 
        Governance$Code == Country),]
    Conf = if(any(Conf$Conflicts == "Conflict")){
      "Conflict"
    } else {
      "No conlict"
    }
    
    #Assign traits
    
    #Longevity
    MaxLon = Traits[which(
      Traits$Species == Spec),]$Longevity_log10
    MaxLon_var = Traits[which(
      Traits$Species == Spec),]$Longevity_log10_Var
    
    #Body mass
    BodyMass = Traits[which(
      Traits$Species == Spec),]$BodyMass_log10
    BodyMass_var = Traits[which(
      Traits$Species == Spec),]$BodyMass_log10_Var
    
    #Reproduction rate
    Reprod = Traits[which(
      Traits$Species == Spec),]$ReprodRate_mean
    Reprod_var = Traits[which(
      Traits$Species == Spec),]$ReprodRate_var
    
    #Reproduction rate
    Gen = Traits[which(
      Traits$Species == Spec),]$Gen_mean
    Gen_var = Traits[which(
      Traits$Species == Spec),]$Gen_var
    
    #Reproduction rate
    Gen2 = Traits[which(
      Traits$Species == Spec),]$clim_mn_sd

    #Protected areas
    ProArea_Size = PA[which(
      PA$ID == ID_),]$N
    ProArea_Count = PA[which(
      PA$ID == ID_),]$ProtectedCells
    ProArea = (ProArea_Count/ProArea_Size)*100
    
    
    DataFrame = data.frame(
      Row = a,
      Start = StudyStart,
      End = StudyEnd,
      PDC = PDChange,
      PD = PDEnd,
      PriC = PriC,
      Pri = PrimEnd,
      NatC = NatC,
      AgC = AgC,
      HumC = HumC,
      ExHeatC = ExHeat,
      DroughtC = DroughtChange, 
      Drought = StudySpei_fl_mean,
      HDI = HDI, 
      HDI_var = HDI_var, 
      HDI_c = HDI_c,
      Gov = Gov, 
      Gov_var = Gov_var, 
      Gov_c = Gov_c,
      Conf = Conf, 
      ProArea = ProArea,
      MaxLon = MaxLon, 
      MaxLon_var = MaxLon_var, 
      BodyMass = BodyMass, 
      BodyMass_var = BodyMass_var, 
      Reprod = Reprod, 
      Reprod_var = Reprod_var,
      Gen = Gen, 
      Gen_var = Gen_var,
      Gen2 = Gen2)
    DataFrameComb = rbind(DataFrameComb, DataFrame)
    rm(StudyStart, 
       StudyEnd,
       PDChange,
       PDEnd,
       PriC,
       PrimEnd,
       NatC,
       AgC,
       HumC,
       ExheatC,
       ExHeat, 
       DroughtC,
       Drought,
       HDI, 
       HDI_var, 
       HDI_c,
       Gov, 
       Gov_var, 
       Gov_c,
       Conf, 
       ProArea,
       MaxLon, 
       MaxLon_var, 
       BodyMass, 
       BodyMass_var, 
       Reprod, 
       Reprod_var,
       Gen, 
       Gen_var,
       Gen2)
  }
  TrendsJoin = cbind(Trends, DataFrameComb) 
  TrendsJoin[TrendsJoin == "NaN"] = NA
  TrendsList[[b]] = TrendsJoin
}

saveRDS(TrendsList, "Documents/ModellingTrends/Data/Analysis/DataToModel2.rds")
