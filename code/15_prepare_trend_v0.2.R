#Load CaPTrends
CaPTrendsRaw = read.csv("Documents/ModellingTrends/Data/Trends/CaPTrends.csv")
CaPTrendsRaw$UniqueID = paste("cpt", CaPTrendsRaw$DataTableID, sep = "_")
CaPTrends = subset(CaPTrendsRaw, select = c(
  UniqueID,
  DataTableID,
  Citation_key,
  Species,
  Singular_country,
  Study_year_start,
  Study_year_end,
  N_observations,
  Quantitative_trend,
  Quantitative_method,
  Qualitative,
  Field_method,
  Modelling_method,
  Genetic_data,
  Harvest_data,
  Invasive_species,
  Record_labelled_inaccurate,
  Asymptotic_growth,
  Metric_unusual,
  Latitude,
  Longitude
))
rm(CaPTrendsRaw)

#Load time-series
ManualTrend = read.csv("Documents/ModellingTrends/Data/Trends/ManualTrendEstimate.csv")

CombAbundance = NULL
CombLambda = NULL
ManTrendTableID = as.character(unique(ManualTrend$DataTableID))
for(a in ManTrendTableID){
  Temp = subset(ManualTrend, DataTableID == a)
  if((nrow(Temp)-sum(Temp$Value == 0)) < 2){
    ZeroN = sum(Temp$Value == 0)
    ExtinctRecol = T
    } else {
    ExtinctRecol = F
    ZeroN = sum(Temp$Value == 0)
  }
  if(Temp$Type_of_measure[1] == "Abundance/Density"){
    Temp$ScaleX = NA
    Temp$ScaleX[1] = 0
    
    for (b in seq(1, (length(Temp[,1])- 1),1)){
      Temp$ScaleX[b + 1] = sum(Temp[1:(b+1),4], na.rm = T)
    }
    Temp$Value = Temp$Value + 10
    Temp$ValueScale = log(Temp$Value)
    Temp = subset(Temp, !is.infinite(ValueScale))
    #ggplot(Temp) +
    #  geom_point(aes(x = ScaleX, y = ValueScale))+
    #  geom_smooth(aes(x = ScaleX, y = ValueScale), method = "lm")
    #ggsave(paste0("Documents/ModellingTrends/Results/slope_fits/_cpt_",a,"_.png"), width = 3, height = 3, units = "in")
    
    if(is.nan(Temp$ValueScale[1])){
      print(paste("skip:",a))
    } else {
      TempOutput = summary(lm(ValueScale ~ ScaleX, data = Temp))
      Est = TempOutput$coefficients[2,1]
      N_observations = b + 1
      Method = "Abundance/Density"
      DataTableID = a
      TempDF = data.frame(DataTableID, Est, N_observations, Method, ExtinctRecol, ZeroN)
      CombAbundance = rbind(CombAbundance, TempDF)
    }
  } else if (Temp$Type_of_measure[1] == "Lambda") {
    ValueScale = NULL
    YearScale = NULL
    ValueScale[1] = 100
    YearScale[1] = 0
    for(b in 1:nrow(Temp)){
      ValueScale[b+1] = ValueScale[b] * Temp$Value[b]
      YearScale[b+1] = YearScale[b] + Temp$Length_time_period[b]
    }
    ValueScale = log(ValueScale)
    TempOutput = summary(lm(ValueScale ~ YearScale, data = Temp))
    Est = TempOutput$coefficients[2,1]
    N_observations = b + 1
    Method = "Lambda"
    DataTableID = a
    TempDF = data.frame(DataTableID, Est, N_observations, Method, ExtinctRecol, ZeroN)
    CombLambda = rbind(CombLambda, TempDF)
  } else{
    #Percent change
    Row1 = Temp[1,]
    Row1$Time_period = 0
    Row1$Value = 100
    Temp = rbind(Row1, Temp)
    Temp$ScaleX = NA
    Temp$ValueAdj[1] = 100
    Temp$ScaleX[1] = 0
    for (b in seq(1, (length(Temp[,1])- 1),1)){
      Temp$ScaleX[b + 1] = sum(Temp[1:(b),4], na.rm = T)
      Temp$ValueAdj[b + 1] = (Temp$Value[b+1]*Temp$ValueAdj[b])/100
    }
    Temp$ValueScale = log(Temp$ValueAdj + 1)
    if(is.nan(Temp$ValueScale[1])){
      print(paste("skip:",a))
    } else {
      TempOutput = summary(lm(ValueScale ~ ScaleX, data = Temp))
      Est = TempOutput$coefficients[2,1]
      Extinct = NA
      N_observations = b + 1
      Method = "Percentage change"
      DataTableID = a
      TempDF = data.frame(DataTableID, Est, N_observations, Method, ExtinctRecol, ZeroN)
      CombAbundance = rbind(CombAbundance, TempDF)
    }
  }
}


CombManual = rbind(CombAbundance, CombLambda)


CaPTrends$DataTableID = as.character(CaPTrends$DataTableID)
CaPTrends = left_join(CaPTrends, CombManual, by = "DataTableID")
CaPTrends$N_observations = ifelse(CaPTrends$Quantitative_method != "Manual calculation required", CaPTrends$N_observations.x, CaPTrends$N_observations.y)
CaPTrends$N_observations = ifelse(CaPTrends$N_observations < 2,
                                  1,
                                  CaPTrends$N_observations)
CaPTrends$N_observations.x = NULL
CaPTrends$N_observations.y = NULL


CaPTrends$PopulationTrend = NA
for(a in 1:nrow(CaPTrends)){
  Meth = CaPTrends$Quantitative_method[a]
  Trend = CaPTrends$Quantitative_trend[a]
  N = CaPTrends$N_observations[a]
  Ys = CaPTrends$Study_year_start[a]
  Ye = CaPTrends$Study_year_end[a]
  Pt = NA
  Va = NA
  if(grepl("Qualitative only", Meth)){
    Trend_t = NA
    Va = NA
  } else if (grepl("Manual calculation required", Meth)){
    Pt = CaPTrends$Est[a]
  } else {
    if(grepl("Lambda", Meth)){
      Trend_t = log(Trend)
    } else if(grepl("Percentage change", Meth)){
      if(Trend == 0){
        CaPTrends$ExtinctRecol[a] = T
      } else {
        ab = NULL
        ab[1] = 100
        ab[2] = ab[1]*(Trend/100)
        tim = c(Ys, Ye)
        Trend_t = coef(lm(log(ab) ~ tim))[2]
      }
    } else if(grepl("change", Meth)){
      if(Trend == 0){
        CaPTrends$ExtinctRecol[a] = T
      } else {
      ab = NULL
      ab[1] = 100
      ab[2] = ab[1]*Trend
      tim = c(Ys, Ye)
      Trend_t = coef(lm(log(ab) ~ tim))[2]
      }
    } else if (grepl("R trend", Meth)){
      Trend_t = Trend
    } else {
      message("skip")
    }
    Pt = Trend_t
  }
  CaPTrends$PopulationTrend[a] = Pt
}


#Load living planet
LPI = read.csv("Documents/ModellingTrends/Data/Trends/LPICombinedEdited.csv", row.names = NULL)
LPI = subset(LPI, is.na(Tag))
LPIManualTrend = LPI[,c(2,31:75)]
A = reshape(LPIManualTrend, 
        direction = "long",
        varying = list(names(LPIManualTrend)[2:46]),
        v.names = "Value",
        idvar = "id",
        timevar = "Year",
        times = 1970:2014)
A = subset(A, !is.na(Value))

CombAbundance = NULL
for(a in unique(A$id)){
  Temp = subset(A, id == a)
  if(nrow(Temp)>1){
    if((nrow(Temp)-sum(Temp$Value == 0)) < 2){
      ZeroN = sum(Temp$Value == 0)
      ExtinctRecol = T
    } else {
      ExtinctRecol = F
      ZeroN = sum(Temp$Value == 0)
    }
  Temp = Temp[order(Temp$Year),]
  Temp$Value = Temp$Value + 10
  Temp$ValueScale = log(Temp$Value)
  TempOutput = summary(lm(ValueScale ~ Year, data = Temp))
  Est = TempOutput$coefficients[2,1]
  N_observations = nrow(Temp)
  Method = "Abundance/Density"
  LPIID = a
  Study_year_start = min(Temp$Year)
  Study_year_end = max(Temp$Year)
  TempDF = data.frame(LPIID, Est, N_observations, Method, ExtinctRecol, ZeroN, Study_year_start, Study_year_end)
  CombAbundance = rbind(CombAbundance, TempDF)
  } else {
  }
}


LPI = LPI[,c(1,2,8,9,17,19,20,25)]
LPI$Species = paste(LPI$Genus, LPI$Species, sep = " ")
LPI$Genus = NULL
LPI = subset(LPI, 
             CountryList != "Serbia, Montenegro" &
               CountryList != "Poland, Slovakia" &
               CountryList != "Namibia, Botswana, South Africa, Tanzania, United Republic Of, Kenya" &
               CountryList != "Italy, France" &
               CountryList != "Argentina, Brazil")
LPI$Species[LPI$Species == "Hyaena brunnea"] <- "Parahyaena brunnea"
LPI$Species[LPI$Species == "Leopardus Pardalis"] <- "Leopardus pardalis"
LPI$Species[LPI$Species == "Uncia uncia"] <- "Panthera uncia"

colnames(LPI) = c(
  "Citation_key",
  "LPIID",
  "Species",
  "Singular_country",
  "Latitude",
  "Longitude",
  "Invasive_species")
LPI$Invasive_species = ifelse(LPI$Invasive_species == F, 1, NA)
LPI = left_join(LPI, CombAbundance)
LPI$UniqueID = paste("lpi", LPI$LPIID, sep = "_")

Trends = rbind.fill(CaPTrends, LPI)
Trends$PopulationTrend = ifelse(!is.na(Trends$LPIID), 
                                Trends$Est,
                                Trends$PopulationTrend)
Trends$ZeroN[is.na(Trends$ZeroN)] = 0
Trends$N_observations[is.na(Trends$N_observations)] = 1
Trends$N_observations[Trends$N_observations < 2] = 1

Trends$LowQualityRecord = ifelse(Trends$N_observations - Trends$ZeroN < 2 |
                                      !is.na(Trends$Genetic_data) |
                                      !is.na(Trends$Harvest_data) |
                                      !is.na(Trends$Record_labelled_inaccurate) |
                                      !is.na(Trends$Asymptotic_growth) |
                                      !is.na(Trends$Metric_unusual), 
                                    T, 
                                    F)
Trends$ExtinctRecol[is.na(Trends$ExtinctRecol)] <- F
Trends$PopulationTrend[is.infinite(Trends$PopulationTrend)] <- NA
Trends$QualitativeIncrease = ifelse((is.na(Trends$PopulationTrend) | Trends$ExtinctRecol == T) &
                                      Trends$Qualitative == "Increase", 1, 0)
Trends$QualitativeDecrease = ifelse((is.na(Trends$PopulationTrend) | Trends$ExtinctRecol == T) &
                                      Trends$Qualitative == "Decrease", 1, 0)
Trends$QualitativeStable = ifelse((is.na(Trends$PopulationTrend) | Trends$ExtinctRecol == T) &
                                    (Trends$Qualitative == "Stable"  |
                                       Trends$Qualitative == "Varied: both declines and recoveries present"), 1, 0)
Trends = left_join(Trends, Coords[,c("Area", "UniqueID")], by = "UniqueID")

Trends = subset(Trends,
                Study_year_start > 1969 &
                  Study_year_end < 2016 &
                  Singular_country != "GLOBAL" &
                  Singular_country != "" &
                  is.na(Invasive_species), 
                select = c(
                  "UniqueID",
                  "DataTableID",
                  "Species",
                  "Singular_country",
                  "Study_year_start",
                  "Study_year_end",
                  "Latitude",
                  "Longitude",
                  "Area",
                  "N_observations",
                  "PopulationTrend",
                  "Quantitative_method",
                  "QualitativeIncrease",
                  "QualitativeDecrease",
                  "QualitativeStable",
                  "Field_method",
                  "Modelling_method",
                  "ExtinctRecol",
                  "LowQualityRecord"
                ))

CountryAccess = read.csv("Documents/ModellingTrends/Data/CountryData/CountryInAccess.csv")
CountryISO3 = read.csv("Documents/ModellingTrends/Data/CountryData/CountryContinent.csv")
Country = left_join(CountryISO3, CountryAccess, by = c("alpha.2" = "ISO_3166.1"))
Trends$Singular_country = as.character(Trends$Singular_country)
Trends$Singular_country[Trends$Singular_country == "Tanzania, United Republic Of"] = "Tanzania, United Republic of"
Trends$Singular_country[Trends$Singular_country == "Svalbard And Jan Mayen"] = "Norway"
Trends$Singular_country[Trends$Singular_country == "Korea, Republic Of"] = "Korea, Republic of"
Trends$Singular_country[Trends$Singular_country == "Kosovo"] = "Serbia"


Trends = left_join(Trends, Country[,c("Country", "alpha.3", "sub.region", "Conts")], by = c("Singular_country" = "Country"))

Trends$PopulationTrend[is.infinite(Trends$PopulationTrend)] = NA



ID_df = read.csv("Documents/ModellingTrends/Data/Trends/ID(save).csv")
Trends = left_join(Trends, ID_df)

