
 #Load, tidy and merge all governance data which is to be imputed

 CountryISO3 = read.csv("Documents/ModellingTrends/Data/CountryData/CountryContinent.csv")
 AllGovernance = NULL
 TempAllGovernance = data.frame(CountryCode = CountryISO3$alpha.3)
 for(a in seq(1960,2018,1)){
   TempAllGovernance$Year = a
   AllGovernance = rbind(AllGovernance, TempAllGovernance)
 }
 AllGovernance$Year = paste("X",AllGovernance$Year, sep = "")

 GDP = read.csv("Documents/ModellingTrends/Data/WG/GlobalGDPerCapita.csv")
 GDP <- GDP %>% gather(Year, GDP, c(X1960:X2018))
 GDP$GDP = as.numeric(GDP$GDP)
 colnames(GDP)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, GDP)

 HDI = read.csv("Documents/ModellingTrends/Data/HDI/HDI.csv") #Merge Country name with ISO 3 and link
 HDI = left_join(HDI, CountryISO3[,c(1,3)], by = c("Country" = "name"))
 HDI <- HDI %>% gather(Year, HDI, X1990:X2017)
 HDI = HDI[,3:5]
 HDI$HDI = as.numeric(HDI$HDI)
 colnames(HDI)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, HDI)

 Corruption = read.csv("Documents/ModellingTrends/Data/WG/Corruption.csv")
 Corruption <- Corruption %>% gather(Year, Corruption, c(X1996,X1998,X2000,X2002:X2018))
 Corruption = Corruption[,2:4]
 Corruption$Corruption = as.numeric(Corruption$Corruption)
 colnames(Corruption)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, Corruption)


 GovEffect = read.csv("Documents/ModellingTrends/Data/WG/GovernmentEffectiveness.csv")
 GovEffect <- GovEffect %>% gather(Year, GovEffect, c(X1996,X1998,X2000,X2002:X2018))
 GovEffect = GovEffect[,2:4]
 GovEffect$GovEffect = as.numeric(GovEffect$GovEffect)
 colnames(GovEffect)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, GovEffect)

 PolStab = read.csv("Documents/ModellingTrends/Data/WG/PoliticalStabilityAndAbsenceOfViolence.csv")
 PolStab <- PolStab %>% gather(Year, PolStab, c(X1996,X1998,X2000,X2002:X2018))
 PolStab = PolStab[,2:4]
 PolStab$PolStab = as.numeric(PolStab$PolStab)
 colnames(PolStab)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, PolStab)

 RegQual = read.csv("Documents/ModellingTrends/Data/WG/RegulatoryQuality.csv")
 RegQual <- RegQual %>% gather(Year, RegQual, c(X1996,X1998,X2000,X2002:X2018))
 RegQual = RegQual[,2:4]
 RegQual$RegQual = as.numeric(RegQual$RegQual)
 colnames(RegQual)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, RegQual)

 RuleOfLaw = read.csv("Documents/ModellingTrends/Data/WG/RuleOfLaw.csv")
 RuleOfLaw <- RuleOfLaw %>% gather(Year, RuleOfLaw, c(X1996,X1998,X2000,X2002:X2018))
 RuleOfLaw = RuleOfLaw[,2:4]
 RuleOfLaw$RuleOfLaw = as.numeric(RuleOfLaw$RuleOfLaw)
 colnames(RuleOfLaw)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, RuleOfLaw)

 VoiceAndAcc = read.csv("Documents/ModellingTrends/Data/WG/VoiceAndAccountability.csv")
 VoiceAndAcc <- VoiceAndAcc %>% gather(Year, VoiceAndAcc, c(X1996,X1998,X2000,X2002:X2018))
 VoiceAndAcc = VoiceAndAcc[,2:4]
 VoiceAndAcc$VoiceAndAcc = as.numeric(VoiceAndAcc$VoiceAndAcc)
 colnames(VoiceAndAcc)[1] = "CountryCode"
 AllGovernance = left_join(AllGovernance, VoiceAndAcc)


Conflicts = read.csv("Documents/ModellingTrends/Data/Conflicts/Conflicts.csv") #Link via ISO 3
Conflicts = Conflicts %>%
   group_by(CountryCode, year) %>%
   dplyr::summarise(Conflicts = n())
Conflicts = as.data.frame(Conflicts)
colnames(Conflicts)[2] = "Year"
Conflicts$Year = paste("X",Conflicts$Year, sep = "")
AllGovernance = left_join(AllGovernance, Conflicts)
AllGovernance$Conflicts[is.na(AllGovernance$Conflicts)] <- 0
AllGovernance$Year = gsub("X", "", AllGovernance$Year)
AllGovernance$Year = as.numeric(AllGovernance$Year)
rm(GDP,HDI,Corruption,GovEffect,PolStab,RegQual,RuleOfLaw,VoiceAndAcc,Conflicts)
(colSums(is.na(AllGovernance))/12201)*100

AllGovernance$GDP_log10 = log10(AllGovernance$GDP)
AllGovernance$GDP = NULL


CountryNum = data.frame(Code = unique(AllGovernance$CountryCode), Num = seq(1, length(unique(AllGovernance$CountryCode)),1))
AllGovernance = left_join(AllGovernance, CountryNum, by = c("CountryCode" = "Code"))
AllGovernance$CountryCode = NULL
AllGovernance$Num = as.integer(AllGovernance$Num)
AllGovernance$Conflicts = ifelse(AllGovernance$Conflicts > 0, "Conflict", "No conflict")
imp0 <- mice(as.matrix(AllGovernance), maxit=0)
predM <- imp0$predictorMatrix
predM[-11,"Num"] = c(1, rep(-2,9))
GovernanceList <- mice(AllGovernance , m = 100, predictorMatrix = predM,
                        method =  "2l.pan", maxit=50, paniter=1000)

saveRDS(GovernanceList, "Documents/ModellingTrends/Data/WG/ImputedGovernance.rds")
GovernanceList = readRDS("Documents/ModellingTrends/Data/WG/ImputedGovernance.rds")
png("Documents/ModellingTrends/Results/GovernanceConvergence_mean.png", width = 1200, height = 1500, res = 200)
plot(GovernanceList, layout = c(2,8), col = rgb(0, 0, 0, 0.05))
dev.off()

CombGovernance = NULL
for(a in 1:100){
  FullDF = complete(GovernanceList,a)
  FullDF$Governance = (
    FullDF$Corruption + 
      FullDF$GovEffect +
      FullDF$PolStab +
      FullDF$RegQual +
      FullDF$RuleOfLaw +
      FullDF$VoiceAndAcc)/6
  FullDF$Model = a
  CombGovernance = rbind(CombGovernance, FullDF)
}

Governance = CombGovernance %>%
  group_by(Num, Year) %>%
  dplyr::summarise(
    Gov_mean = mean(Governance),
    Gov_var = var(Governance),
    Cor_mean = mean(Corruption),
    Cor_var = var(Corruption),
    GE_mean = mean(GovEffect),
    GE_var = var(GovEffect),
    PS_mean = mean(PolStab),
    PS_var = var(PolStab),
    RQ_mean = mean(RegQual),
    RQ_var = var(RegQual),
    RL_mean = mean(RuleOfLaw),
    RL_var = var(RuleOfLaw),
    VA_mean = mean(VoiceAndAcc),
    VA_var = var(VoiceAndAcc),
    HDI_mean = mean(HDI),
    HDI_var = var(HDI),
    GDP_mean = mean(GDP_log10),
    GDP_var = var(GDP_log10))
Governance = left_join(Governance, FullDF[c("Num", "Year", "Conflicts")])
Governance = left_join(Governance, CountryNum)
saveRDS(Governance, "Documents/ModellingTrends/Data/WG/GovernanceFormatted.rds")


