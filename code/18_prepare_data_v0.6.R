m1 = readRDS("ErrorModel.rds")
ihs <- function(x) {
  y <- log(x + sqrt(x ^ 2 + 1))
  return(y)
}


####REMOVE TRENDS WITH NO TEMPORAL COMPONENT. i.e START YEAR MUST DIFFER TO END YEAR


TrendsTrim = Trends[[1]]
TrendsTrim_lag0 = TrendsTrim

TrendsTrim_lag0$N_observations = ifelse(is.na(TrendsTrim_lag0$N_observations), 2, TrendsTrim_lag0$N_observations)

man_wt_fld = ifelse(grepl("Unsyst",TrendsTrim_lag0$Field_method) | grepl("Undef",TrendsTrim_lag0$Field_method), 0.08, NA)
man_wt_fld = ifelse(grepl("Syst",TrendsTrim_lag0$Field_method), 0.04, man_wt_fld)
man_wt_fld = ifelse(grepl("Indiv",TrendsTrim_lag0$Field_method), 0.01, man_wt_fld)
man_wt_fld = ifelse(is.na(TrendsTrim_lag0$Field_method), 0.08, man_wt_fld)

man_wt_mod = ifelse(grepl("Field",TrendsTrim_lag0$Modelling_method) | grepl("Undef",TrendsTrim_lag0$Modelling_method), 0.08, NA)
man_wt_mod = ifelse(grepl("Acc",TrendsTrim_lag0$Modelling_method), 0.04, man_wt_mod)
man_wt_mod = ifelse(grepl("odel",TrendsTrim_lag0$Modelling_method) | grepl("Total",TrendsTrim_lag0$Modelling_method), 0.01, man_wt_mod)
man_wt_mod = ifelse(is.na(TrendsTrim_lag0$Modelling_method), 0.08, man_wt_mod)

man_wt_lq = ifelse(TrendsTrim_lag0$LowQualityRecord == F, 0.04, 0)

sd = rowSums(cbind(man_wt_fld, man_wt_mod, man_wt_lq))

tim_dif = (ifelse((TrendsTrim_lag0$Study_year_end - TrendsTrim_lag0$Study_year_start) == 0, 1,(TrendsTrim_lag0$Study_year_end - TrendsTrim_lag0$Study_year_start))) + 1

pred_err = predict(m1, newdata = data.frame(
  samp_int = (TrendsTrim_lag0$N_observations/tim_dif),
  sd = (sd),
  time_dif = tim_dif))

pred_err2 = exp(pred_err)
TrendsTrim_lag0$abs_weight = (1 -(pred_err2-min(pred_err2))/(max(pred_err2)-min(pred_err2))) + 1e-10
#rm(list=setdiff(ls(), c("Trends", "TrendsTrim_lag0", "ihs","m1")))
TrendsTrim_lag0 = subset(TrendsTrim_lag0, ExtinctRecol == F)
TrendsTrim_lag0 = subset(TrendsTrim_lag0, Area <= 2000000)
TrendsTrim_lag0 = TrendsTrim_lag0[complete.cases(TrendsTrim_lag0[,27:53]),]
TrendsTrim_lag0 = subset(TrendsTrim_lag0, !is.na(PopulationTrend) 
                         | QualitativeIncrease != 0
                         | QualitativeDecrease != 0
                         | QualitativeStable != 0)

TrendsTrim_lag0$PopulationTrend[TrendsTrim_lag0$QualitativeDecrease == 1] = NA
TrendsTrim_lag0$QualitativeDecrease[is.na(TrendsTrim_lag0$QualitativeIncrease) & TrendsTrim_lag0$ExtinctRecol == T] = 0
TrendsTrim_lag0$PopulationTrend[TrendsTrim_lag0$QualitativeStable == 1] = NA
TrendsTrim_lag0$QualitativeStable[is.na(TrendsTrim_lag0$QualitativeIncrease) & TrendsTrim_lag0$ExtinctRecol == T] = 0
TrendsTrim_lag0$PopulationTrend[TrendsTrim_lag0$QualitativeIncrease == 1] = NA
TrendsTrim_lag0$QualitativeIncrease[is.na(TrendsTrim_lag0$QualitativeIncrease) & TrendsTrim_lag0$ExtinctRecol == T] = 1

TrendsTrim_lag0$PopulationTrend[TrendsTrim_lag0$QualitativeIncrease == 1 |
                                  TrendsTrim_lag0$QualitativeDecrease == 1 |
                                  TrendsTrim_lag0$QualitativeStable == 1] = NA

TrendsTrim_lag0 = TrendsTrim_lag0[order(TrendsTrim_lag0$PopulationTrend, TrendsTrim_lag0$QualitativeIncrease, TrendsTrim_lag0$QualitativeDecrease, TrendsTrim_lag0$QualitativeStable),]
rownames(TrendsTrim_lag0) = seq(1,nrow(TrendsTrim_lag0),1)

SpeciesMerge = read.csv("data/SpeciesListRAW.csv")
SpeciesMerge$Species = paste(SpeciesMerge$Genus, SpeciesMerge$Species, sep = " ")
TrendsTrim_lag0 = left_join(TrendsTrim_lag0, SpeciesMerge[,c("Family", "Genus", "Species")])


HDI_u = TrendsTrim_lag0$HDI + (sqrt(TrendsTrim_lag0$HDI_var)/sqrt(100))*1.96
HDI_l = TrendsTrim_lag0$HDI - (sqrt(TrendsTrim_lag0$HDI_var)/sqrt(100))*1.96
HDI_m = mean(TrendsTrim_lag0$HDI)
HDI_sd = sd(TrendsTrim_lag0$HDI)
HDI_u = (HDI_u - HDI_m)/HDI_sd
HDI_l = (HDI_l - HDI_m)/HDI_sd
TrendsTrim_lag0$HDI_s = (TrendsTrim_lag0$HDI - HDI_m)/HDI_sd
TrendsTrim_lag0$HDI_sv = (sqrt(100) * (HDI_u - HDI_l)/3.92)^2

Gov_u = TrendsTrim_lag0$Gov + (sqrt(TrendsTrim_lag0$Gov_var)/sqrt(100))*1.96
Gov_l = TrendsTrim_lag0$Gov - (sqrt(TrendsTrim_lag0$Gov_var)/sqrt(100))*1.96
Gov_m = mean(TrendsTrim_lag0$Gov)
Gov_sd = sd(TrendsTrim_lag0$Gov)
Gov_u = (Gov_u - Gov_m)/Gov_sd
Gov_l = (Gov_l - Gov_m)/Gov_sd
TrendsTrim_lag0$Gov_s = (TrendsTrim_lag0$Gov - Gov_m)/Gov_sd
TrendsTrim_lag0$Gov_sv = (sqrt(100) * (Gov_u - Gov_l)/3.92)^2

Reprod_u = TrendsTrim_lag0$Reprod + (sqrt(TrendsTrim_lag0$Reprod_var)/sqrt(100))*1.96
Reprod_l = TrendsTrim_lag0$Reprod - (sqrt(TrendsTrim_lag0$Reprod_var)/sqrt(100))*1.96
Reprod_m = mean(TrendsTrim_lag0$Reprod)
Reprod_sd = sd(TrendsTrim_lag0$Reprod)
Reprod_u = (Reprod_u - Reprod_m)/Reprod_sd
Reprod_l = (Reprod_l - Reprod_m)/Reprod_sd
TrendsTrim_lag0$Reprod_s = (TrendsTrim_lag0$Reprod - Reprod_m)/Reprod_sd
TrendsTrim_lag0$Reprod_sv = (sqrt(100) * (Reprod_u - Reprod_l)/3.92)^2

Gen_u = TrendsTrim_lag0$Gen + (sqrt(TrendsTrim_lag0$Gen_var)/sqrt(100))*1.96
Gen_l = TrendsTrim_lag0$Gen - (sqrt(TrendsTrim_lag0$Gen_var)/sqrt(100))*1.96
Gen_m = mean(TrendsTrim_lag0$Gen)
Gen_sd = sd(TrendsTrim_lag0$Gen)
Gen_u = (Gen_u - Gen_m)/Gen_sd
Gen_l = (Gen_l - Gen_m)/Gen_sd
TrendsTrim_lag0$Gen_s = (TrendsTrim_lag0$Gen - Gen_m)/Gen_sd
TrendsTrim_lag0$Gen_sv = (sqrt(100) * (Gen_u - Gen_l)/3.92)^2


lwr = ifelse(TrendsTrim_lag0$QualitativeIncrease == 1, ihs(0), NA)
lwr = ifelse(TrendsTrim_lag0$QualitativeStable == 1, ihs(-5), lwr)
lwr = ifelse(TrendsTrim_lag0$QualitativeDecrease == 1, ihs(-50), lwr)
upr = ifelse(TrendsTrim_lag0$QualitativeIncrease == 1, ihs(50), NA)
upr = ifelse(TrendsTrim_lag0$QualitativeStable == 1, ihs(5), upr)
upr = ifelse(TrendsTrim_lag0$QualitativeDecrease == 1, ihs(0), upr)


jagsdata_lag0 = with(TrendsTrim_lag0,  list(
  pt = ihs((exp(PopulationTrend)-1)*100),
  stab_min = which(TrendsTrim_lag0$QualitativeStable == 1)[1],
  inc_max = which(TrendsTrim_lag0$QualitativeIncrease == 1)[length(which(TrendsTrim_lag0$QualitativeIncrease == 1))],
  cens.pred = ifelse(is.na(TrendsTrim_lag0$PopulationTrend), 1, 0),
  cens = ifelse(is.na(TrendsTrim_lag0$PopulationTrend), 1, 0),
  lim = cbind(lwr, upr),
  #Var param
  nobs = N_observations,
  wg_a = abs_weight,
  #Add environmental parameters
  pdc = as.vector(scale(PDC)),
  prc = as.vector(scale(abs(PriC))),
  ntc = as.vector(scale(NatC)),
  #Add climate parameters
  exh = as.vector(scale(ExHeatC)),
  drc = as.vector(scale(DroughtC)),
  #Add governance parameters
  hdi = as.vector(HDI_s),
  hdi_v = (ifelse(is.infinite(1/HDI_sv), 1/min(TrendsTrim_lag0$HDI_sv[which(TrendsTrim_lag0$HDI_sv != 0)]), 1/HDI_sv)),
  gov = as.vector(Gov_s),
  gov_v = (ifelse(is.infinite(1/Gov_sv), 1/min(TrendsTrim_lag0$Gov_sv[which(TrendsTrim_lag0$Gov_sv != 0)]), 1/Gov_sv)),
  hdc = as.vector(scale(HDI_c)),
  gvc = as.vector(scale(Gov_c)),
  conf = as.integer(as.factor(Conf)),
  pr = as.vector(scale(ProArea)),
  #Add trait parameters
  ml = as.vector(scale(MaxLon)),
  bm = as.vector(scale(BodyMass)),
  re = as.vector(Reprod_s),
  re_v = as.vector(1/Reprod_sv),
  gn = as.vector(Gen_s),
  gn_v = as.vector(1/Gen_sv),
  gc = as.vector(scale(Gen2)),
  #Add controls
  ar = as.vector(scale(log10(Area))),
  #Add heirarchy
  reg = as.numeric(droplevels(as.factor(Conts))),
  cou = as.numeric(droplevels(as.factor(Singular_country))),
  reg_n = length(unique(Conts)),
  cou_n = length(unique(Singular_country)),
  gen = as.numeric(droplevels(as.factor(Genus))),
  spec = as.numeric(as.factor(Species)),
  gen_n = length(unique(Genus)),
  spec_n = length(unique(Species))
))



TrendsTrim = Trends[[2]]
TrendsTrim_lag5 = TrendsTrim

TrendsTrim_lag5$N_observations = ifelse(is.na(TrendsTrim_lag5$N_observations), 1, TrendsTrim_lag5$N_observations)

man_wt_fld = ifelse(grepl("Unsyst",TrendsTrim_lag5$Field_method) | grepl("Undef",TrendsTrim_lag5$Field_method), 0.08, NA)
man_wt_fld = ifelse(grepl("Syst",TrendsTrim_lag5$Field_method), 0.04, man_wt_fld)
man_wt_fld = ifelse(grepl("Indiv",TrendsTrim_lag5$Field_method), 0.01, man_wt_fld)
man_wt_fld = ifelse(is.na(TrendsTrim_lag5$Field_method), 0.08, man_wt_fld)

man_wt_mod = ifelse(grepl("Field",TrendsTrim_lag5$Modelling_method) | grepl("Undef",TrendsTrim_lag5$Modelling_method), 0.08, NA)
man_wt_mod = ifelse(grepl("Acc",TrendsTrim_lag5$Modelling_method), 0.04, man_wt_mod)
man_wt_mod = ifelse(grepl("odel",TrendsTrim_lag5$Modelling_method) | grepl("Total",TrendsTrim_lag5$Modelling_method), 0.01, man_wt_mod)
man_wt_mod = ifelse(is.na(TrendsTrim_lag5$Modelling_method), 0.08, man_wt_mod)

man_wt_lq = ifelse(TrendsTrim_lag5$LowQualityRecord == F, 0.04, 0)

sd = rowSums(cbind(man_wt_fld, man_wt_mod, man_wt_lq))

tim_dif = (ifelse((TrendsTrim_lag5$Study_year_end - TrendsTrim_lag5$Study_year_start) == 0, 1,(TrendsTrim_lag5$Study_year_end - TrendsTrim_lag5$Study_year_start)))  + 1

pred_err = predict(m1, newdata = data.frame(
  samp_int = (TrendsTrim_lag5$N_observations/tim_dif),
  sd = (sd),
  time_dif = tim_dif))

pred_err2 = exp(pred_err)
TrendsTrim_lag5$abs_weight = (1 -(pred_err2-min(pred_err2))/(max(pred_err2)-min(pred_err2))) + 1e-10
#rm(list=setdiff(ls(), c("Trends", "TrendsTrim_lag5", "ihs","m1")))

TrendsTrim_lag5 = subset(TrendsTrim_lag5, ExtinctRecol == F)
TrendsTrim_lag5 = subset(TrendsTrim_lag5, Area <= 2000000)
TrendsTrim_lag5 = TrendsTrim_lag5[complete.cases(TrendsTrim_lag5[,27:53]),]
TrendsTrim_lag5 = subset(TrendsTrim_lag5, !is.na(PopulationTrend) 
                    | QualitativeIncrease != 0
                    | QualitativeDecrease != 0
                    | QualitativeStable != 0)

TrendsTrim_lag5$PopulationTrend[TrendsTrim_lag5$QualitativeDecrease == 1] = NA
TrendsTrim_lag5$QualitativeDecrease[is.na(TrendsTrim_lag5$QualitativeIncrease) & TrendsTrim_lag5$ExtinctRecol == T] = 0
TrendsTrim_lag5$PopulationTrend[TrendsTrim_lag5$QualitativeStable == 1] = NA
TrendsTrim_lag5$QualitativeStable[is.na(TrendsTrim_lag5$QualitativeIncrease) & TrendsTrim_lag5$ExtinctRecol == T] = 0
TrendsTrim_lag5$PopulationTrend[TrendsTrim_lag5$QualitativeIncrease == 1] = NA
TrendsTrim_lag5$QualitativeIncrease[is.na(TrendsTrim_lag5$QualitativeIncrease) & TrendsTrim_lag5$ExtinctRecol == T] = 1

TrendsTrim_lag5$PopulationTrend[TrendsTrim_lag5$QualitativeIncrease == 1 |
                             TrendsTrim_lag5$QualitativeDecrease == 1 |
                             TrendsTrim_lag5$QualitativeStable == 1] = NA

TrendsTrim_lag5 = TrendsTrim_lag5[order(TrendsTrim_lag5$PopulationTrend, TrendsTrim_lag5$QualitativeIncrease, TrendsTrim_lag5$QualitativeDecrease, TrendsTrim_lag5$QualitativeStable),]
rownames(TrendsTrim_lag5) = seq(1,nrow(TrendsTrim_lag5),1)

SpeciesMerge = read.csv("data/SpeciesListRAW.csv")
SpeciesMerge$Species = paste(SpeciesMerge$Genus, SpeciesMerge$Species, sep = " ")
TrendsTrim_lag5 = left_join(TrendsTrim_lag5, SpeciesMerge[,c("Family", "Genus", "Species")])


HDI_u = TrendsTrim_lag5$HDI + (sqrt(TrendsTrim_lag5$HDI_var)/sqrt(100))*1.96
HDI_l = TrendsTrim_lag5$HDI - (sqrt(TrendsTrim_lag5$HDI_var)/sqrt(100))*1.96
HDI_m = mean(TrendsTrim_lag5$HDI)
HDI_sd = sd(TrendsTrim_lag5$HDI)
HDI_u = (HDI_u - HDI_m)/HDI_sd
HDI_l = (HDI_l - HDI_m)/HDI_sd
TrendsTrim_lag5$HDI_s = (TrendsTrim_lag5$HDI - HDI_m)/HDI_sd
TrendsTrim_lag5$HDI_sv = (sqrt(100) * (HDI_u - HDI_l)/3.92)^2

Gov_u = TrendsTrim_lag5$Gov + (sqrt(TrendsTrim_lag5$Gov_var)/sqrt(100))*1.96
Gov_l = TrendsTrim_lag5$Gov - (sqrt(TrendsTrim_lag5$Gov_var)/sqrt(100))*1.96
Gov_m = mean(TrendsTrim_lag5$Gov)
Gov_sd = sd(TrendsTrim_lag5$Gov)
Gov_u = (Gov_u - Gov_m)/Gov_sd
Gov_l = (Gov_l - Gov_m)/Gov_sd
TrendsTrim_lag5$Gov_s = (TrendsTrim_lag5$Gov - Gov_m)/Gov_sd
TrendsTrim_lag5$Gov_sv = (sqrt(100) * (Gov_u - Gov_l)/3.92)^2

Reprod_u = TrendsTrim_lag5$Reprod + (sqrt(TrendsTrim_lag5$Reprod_var)/sqrt(100))*1.96
Reprod_l = TrendsTrim_lag5$Reprod - (sqrt(TrendsTrim_lag5$Reprod_var)/sqrt(100))*1.96
Reprod_m = mean(TrendsTrim_lag5$Reprod)
Reprod_sd = sd(TrendsTrim_lag5$Reprod)
Reprod_u = (Reprod_u - Reprod_m)/Reprod_sd
Reprod_l = (Reprod_l - Reprod_m)/Reprod_sd
TrendsTrim_lag5$Reprod_s = (TrendsTrim_lag5$Reprod - Reprod_m)/Reprod_sd
TrendsTrim_lag5$Reprod_sv = (sqrt(100) * (Reprod_u - Reprod_l)/3.92)^2

Gen_u = TrendsTrim_lag5$Gen + (sqrt(TrendsTrim_lag5$Gen_var)/sqrt(100))*1.96
Gen_l = TrendsTrim_lag5$Gen - (sqrt(TrendsTrim_lag5$Gen_var)/sqrt(100))*1.96
Gen_m = mean(TrendsTrim_lag5$Gen)
Gen_sd = sd(TrendsTrim_lag5$Gen)
Gen_u = (Gen_u - Gen_m)/Gen_sd
Gen_l = (Gen_l - Gen_m)/Gen_sd
TrendsTrim_lag5$Gen_s = (TrendsTrim_lag5$Gen - Gen_m)/Gen_sd
TrendsTrim_lag5$Gen_sv = (sqrt(100) * (Gen_u - Gen_l)/3.92)^2


lwr = ifelse(TrendsTrim_lag5$QualitativeIncrease == 1, ihs(0), NA)
lwr = ifelse(TrendsTrim_lag5$QualitativeStable == 1, ihs(-5), lwr)
lwr = ifelse(TrendsTrim_lag5$QualitativeDecrease == 1, ihs(-50), lwr)
upr = ifelse(TrendsTrim_lag5$QualitativeIncrease == 1, ihs(50), NA)
upr = ifelse(TrendsTrim_lag5$QualitativeStable == 1, ihs(5), upr)
upr = ifelse(TrendsTrim_lag5$QualitativeDecrease == 1, ihs(0), upr)


jagsdata_lag5 = with(TrendsTrim_lag5,  list(
  pt = ihs((exp(PopulationTrend)-1)*100),
  stab_min = which(TrendsTrim_lag5$QualitativeStable == 1)[1],
  inc_max = which(TrendsTrim_lag5$QualitativeIncrease == 1)[length(which(TrendsTrim_lag5$QualitativeIncrease == 1))],
  cens.pred = ifelse(is.na(TrendsTrim_lag5$PopulationTrend), 1, 0),
  cens = ifelse(is.na(TrendsTrim_lag5$PopulationTrend), 1, 0),
  lim = cbind(lwr, upr),
  #Var param
  nobs = N_observations,
  wg_a = abs_weight,
  #Add environmental parameters
  pdc = as.vector(scale(PDC)),
  prc = as.vector(scale(abs(PriC))),
  ntc = as.vector(scale(NatC)),
  #Add climate parameters
  exh = as.vector(scale(ExHeatC)),
  drc = as.vector(scale(DroughtC)),
  #Add governance parameters
  hdi = as.vector(HDI_s),
  hdi_v = (ifelse(is.infinite(1/HDI_sv), 1/min(TrendsTrim_lag5$HDI_sv[which(TrendsTrim_lag5$HDI_sv != 0)]), 1/HDI_sv)),
  gov = as.vector(Gov_s),
  gov_v = (ifelse(is.infinite(1/Gov_sv), 1/min(TrendsTrim_lag5$Gov_sv[which(TrendsTrim_lag5$Gov_sv != 0)]), 1/Gov_sv)),
  hdc = as.vector(scale(HDI_c)),
  gvc = as.vector(scale(Gov_c)),
  conf = as.integer(as.factor(Conf)),
  pr = as.vector(scale(ProArea)),
  #Add trait parameters
  ml = as.vector(scale(MaxLon)),
  bm = as.vector(scale(BodyMass)),
  re = as.vector(Reprod_s),
  re_v = as.vector(1/Reprod_sv),
  gn = as.vector(Gen_s),
  gn_v = as.vector(1/Gen_sv),
  gc = as.vector(scale(Gen2)),
  #Add controls
  ar = as.vector(scale(log10(Area))),
  #Add heirarchy
  reg = as.numeric(droplevels(as.factor(Conts))),
  cou = as.numeric(droplevels(as.factor(Singular_country))),
  reg_n = length(unique(Conts)),
  cou_n = length(unique(Singular_country)),
  gen = as.numeric(droplevels(as.factor(Genus))),
  spec = as.numeric(as.factor(Species)),
  gen_n = length(unique(Genus)),
  spec_n = length(unique(Species))
))


TrendsTrim = Trends[[3]]
TrendsTrim_lag10 = TrendsTrim

TrendsTrim_lag10$N_observations = ifelse(is.na(TrendsTrim_lag10$N_observations), 1, TrendsTrim_lag10$N_observations)

man_wt_fld = ifelse(grepl("Unsyst",TrendsTrim_lag10$Field_method) | grepl("Undef",TrendsTrim_lag10$Field_method), 0.08, NA)
man_wt_fld = ifelse(grepl("Syst",TrendsTrim_lag10$Field_method), 0.04, man_wt_fld)
man_wt_fld = ifelse(grepl("Indiv",TrendsTrim_lag10$Field_method), 0.01, man_wt_fld)
man_wt_fld = ifelse(is.na(TrendsTrim_lag10$Field_method), 0.08, man_wt_fld)

man_wt_mod = ifelse(grepl("Field",TrendsTrim_lag10$Modelling_method) | grepl("Undef",TrendsTrim_lag10$Modelling_method), 0.08, NA)
man_wt_mod = ifelse(grepl("Acc",TrendsTrim_lag10$Modelling_method), 0.04, man_wt_mod)
man_wt_mod = ifelse(grepl("odel",TrendsTrim_lag10$Modelling_method) | grepl("Total",TrendsTrim_lag10$Modelling_method), 0.01, man_wt_mod)
man_wt_mod = ifelse(is.na(TrendsTrim_lag10$Modelling_method), 0.08, man_wt_mod)

man_wt_lq = ifelse(TrendsTrim_lag10$LowQualityRecord == F, 0.04, 0)

sd = rowSums(cbind(man_wt_fld, man_wt_mod, man_wt_lq))

tim_dif = (ifelse((TrendsTrim_lag10$Study_year_end - TrendsTrim_lag10$Study_year_start) == 0, 1,(TrendsTrim_lag10$Study_year_end - TrendsTrim_lag10$Study_year_start)))  + 1

pred_err = predict(m1, newdata = data.frame(
  samp_int = (TrendsTrim_lag10$N_observations/tim_dif),
  sd = (sd),
  time_dif = tim_dif))

pred_err2 = exp(pred_err)
TrendsTrim_lag10$abs_weight = (1 -(pred_err2-min(pred_err2))/(max(pred_err2)-min(pred_err2))) + 1e-10
#rm(list=setdiff(ls(), c("Trends", "TrendsTrim_lag10", "ihs","m1")))

TrendsTrim_lag10 = subset(TrendsTrim_lag10, ExtinctRecol == F)
TrendsTrim_lag10 = subset(TrendsTrim_lag10, Area <= 2000000)
TrendsTrim_lag10 = TrendsTrim_lag10[complete.cases(TrendsTrim_lag10[,27:53]),]
TrendsTrim_lag10 = subset(TrendsTrim_lag10, !is.na(PopulationTrend) 
                         | QualitativeIncrease != 0
                         | QualitativeDecrease != 0
                         | QualitativeStable != 0)

TrendsTrim_lag10$PopulationTrend[TrendsTrim_lag10$QualitativeDecrease == 1] = NA
TrendsTrim_lag10$QualitativeDecrease[is.na(TrendsTrim_lag10$QualitativeIncrease) & TrendsTrim_lag10$ExtinctRecol == T] = 0
TrendsTrim_lag10$PopulationTrend[TrendsTrim_lag10$QualitativeStable == 1] = NA
TrendsTrim_lag10$QualitativeStable[is.na(TrendsTrim_lag10$QualitativeIncrease) & TrendsTrim_lag10$ExtinctRecol == T] = 0
TrendsTrim_lag10$PopulationTrend[TrendsTrim_lag10$QualitativeIncrease == 1] = NA
TrendsTrim_lag10$QualitativeIncrease[is.na(TrendsTrim_lag10$QualitativeIncrease) & TrendsTrim_lag10$ExtinctRecol == T] = 1

TrendsTrim_lag10$PopulationTrend[TrendsTrim_lag10$QualitativeIncrease == 1 |
                                  TrendsTrim_lag10$QualitativeDecrease == 1 |
                                  TrendsTrim_lag10$QualitativeStable == 1] = NA

TrendsTrim_lag10 = TrendsTrim_lag10[order(TrendsTrim_lag10$PopulationTrend, TrendsTrim_lag10$QualitativeIncrease, TrendsTrim_lag10$QualitativeDecrease, TrendsTrim_lag10$QualitativeStable),]
rownames(TrendsTrim_lag10) = seq(1,nrow(TrendsTrim_lag10),1)

SpeciesMerge = read.csv("data/SpeciesListRAW.csv")
SpeciesMerge$Species = paste(SpeciesMerge$Genus, SpeciesMerge$Species, sep = " ")
TrendsTrim_lag10 = left_join(TrendsTrim_lag10, SpeciesMerge[,c("Family", "Genus", "Species")])


HDI_u = TrendsTrim_lag10$HDI + (sqrt(TrendsTrim_lag10$HDI_var)/sqrt(100))*1.96
HDI_l = TrendsTrim_lag10$HDI - (sqrt(TrendsTrim_lag10$HDI_var)/sqrt(100))*1.96
HDI_m = mean(TrendsTrim_lag10$HDI)
HDI_sd = sd(TrendsTrim_lag10$HDI)
HDI_u = (HDI_u - HDI_m)/HDI_sd
HDI_l = (HDI_l - HDI_m)/HDI_sd
TrendsTrim_lag10$HDI_s = (TrendsTrim_lag10$HDI - HDI_m)/HDI_sd
TrendsTrim_lag10$HDI_sv = (sqrt(100) * (HDI_u - HDI_l)/3.92)^2

Gov_u = TrendsTrim_lag10$Gov + (sqrt(TrendsTrim_lag10$Gov_var)/sqrt(100))*1.96
Gov_l = TrendsTrim_lag10$Gov - (sqrt(TrendsTrim_lag10$Gov_var)/sqrt(100))*1.96
Gov_m = mean(TrendsTrim_lag10$Gov)
Gov_sd = sd(TrendsTrim_lag10$Gov)
Gov_u = (Gov_u - Gov_m)/Gov_sd
Gov_l = (Gov_l - Gov_m)/Gov_sd
TrendsTrim_lag10$Gov_s = (TrendsTrim_lag10$Gov - Gov_m)/Gov_sd
TrendsTrim_lag10$Gov_sv = (sqrt(100) * (Gov_u - Gov_l)/3.92)^2

Reprod_u = TrendsTrim_lag10$Reprod + (sqrt(TrendsTrim_lag10$Reprod_var)/sqrt(100))*1.96
Reprod_l = TrendsTrim_lag10$Reprod - (sqrt(TrendsTrim_lag10$Reprod_var)/sqrt(100))*1.96
Reprod_m = mean(TrendsTrim_lag10$Reprod)
Reprod_sd = sd(TrendsTrim_lag10$Reprod)
Reprod_u = (Reprod_u - Reprod_m)/Reprod_sd
Reprod_l = (Reprod_l - Reprod_m)/Reprod_sd
TrendsTrim_lag10$Reprod_s = (TrendsTrim_lag10$Reprod - Reprod_m)/Reprod_sd
TrendsTrim_lag10$Reprod_sv = (sqrt(100) * (Reprod_u - Reprod_l)/3.92)^2

Gen_u = TrendsTrim_lag10$Gen + (sqrt(TrendsTrim_lag10$Gen_var)/sqrt(100))*1.96
Gen_l = TrendsTrim_lag10$Gen - (sqrt(TrendsTrim_lag10$Gen_var)/sqrt(100))*1.96
Gen_m = mean(TrendsTrim_lag10$Gen)
Gen_sd = sd(TrendsTrim_lag10$Gen)
Gen_u = (Gen_u - Gen_m)/Gen_sd
Gen_l = (Gen_l - Gen_m)/Gen_sd
TrendsTrim_lag10$Gen_s = (TrendsTrim_lag10$Gen - Gen_m)/Gen_sd
TrendsTrim_lag10$Gen_sv = (sqrt(100) * (Gen_u - Gen_l)/3.92)^2


lwr = ifelse(TrendsTrim_lag10$QualitativeIncrease == 1, ihs(0), NA)
lwr = ifelse(TrendsTrim_lag10$QualitativeStable == 1, ihs(-5), lwr)
lwr = ifelse(TrendsTrim_lag10$QualitativeDecrease == 1, ihs(-50), lwr)
upr = ifelse(TrendsTrim_lag10$QualitativeIncrease == 1, ihs(50), NA)
upr = ifelse(TrendsTrim_lag10$QualitativeStable == 1, ihs(5), upr)
upr = ifelse(TrendsTrim_lag10$QualitativeDecrease == 1, ihs(0), upr)


library(rnaturalearth)
library(rnaturalearthdata)
library(viridis)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## [1] "sf"  
## [1] "data.frame"

TrendsTrim_lag10$PopTrend_perc = (exp(TrendsTrim_lag10$PopulationTrend) - 1)*100
TrendsTrim_lag10$PopTrend_bin = ifelse(TrendsTrim_lag10$PopTrend_perc > 5, "Increase", NA)
TrendsTrim_lag10$PopTrend_bin = ifelse(TrendsTrim_lag10$PopTrend_perc < -5, "Decrease", TrendsTrim_lag10$PopTrend_bin)
TrendsTrim_lag10$PopTrend_bin = ifelse(TrendsTrim_lag10$PopTrend_perc > -5 & TrendsTrim_lag10$PopTrend_perc < 5, "Stable", TrendsTrim_lag10$PopTrend_bin)
TrendsTrim_lag10$PopTrend_bin = factor(TrendsTrim_lag10$PopTrend_bin, levels = c("Increase", "Stable", "Decrease"))

a = ggplot() +
  geom_sf(data = world) +
  geom_point(data = TrendsTrim_lag10[which(TrendsTrim_lag10$PopTrend_bin == "Increase"),], aes(x = Longitude, y = Latitude), alpha = 0.5, colour = "purple") +
  geom_point(data = TrendsTrim_lag10[which(is.na(TrendsTrim_lag10$PopulationTrend) & TrendsTrim_lag10$QualitativeIncrease == 1),], aes(x = Longitude, y = Latitude), alpha = 0.5, colour = "orange") +
  theme_classic() +
  labs(x = "Longitude", y = "Latitude")

b = ggplot() +
  geom_sf(data = world) +
  geom_point(data = TrendsTrim_lag10[which(TrendsTrim_lag10$PopTrend_bin == "Stable"),], aes(x = Longitude, y = Latitude), alpha = 0.5, colour = "purple") +
  geom_point(data = TrendsTrim_lag10[which(is.na(TrendsTrim_lag10$PopulationTrend) & TrendsTrim_lag10$QualitativeStable == 1),], aes(x = Longitude, y = Latitude), alpha = 0.5, colour = "orange") +
  theme_classic() +
  labs(x = "Longitude", y = "Latitude")

c = ggplot() +
  geom_sf(data = world) +
  geom_point(data = TrendsTrim_lag10[which(TrendsTrim_lag10$PopTrend_bin == "Decrease"),], aes(x = Longitude, y = Latitude), alpha = 0.5, colour = "purple") +
  geom_point(data = TrendsTrim_lag10[which(is.na(TrendsTrim_lag10$PopulationTrend) & TrendsTrim_lag10$QualitativeStable == 1),], aes(x = Longitude, y = Latitude), alpha = 0.5, colour = "orange") +
  theme_classic() +
  scale_x_continuous(breaks = c(-90,0,90)) +
  scale_y_continuous(breaks = c(-45,0,45)) +
  labs(x = "Longitude", y = "Latitude")

d = ggplot() +
  geom_density(data = TrendsTrim_lag10, aes(x = PopTrend_perc), fill = "grey", alpha = 0.5) +
  coord_cartesian(xlim = c(-30,30)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(x = "Annual rate of change (%)", y = "Density")


ggarrange(d,a,b,c,ncol = 2, nrow = 2, labels = c("a,", "b,", "c,", "d,"))
ggsave("qual_quan_space.png", width = 9, height = 5, dpi = 500)




jagsdata_lag10 = with(TrendsTrim_lag10,  list(
  pt = ihs((exp(PopulationTrend)-1)*100),
  stab_min = which(TrendsTrim_lag10$QualitativeStable == 1)[1],
  inc_max = which(TrendsTrim_lag10$QualitativeIncrease == 1)[length(which(TrendsTrim_lag10$QualitativeIncrease == 1))],
  cens.pred = ifelse(is.na(TrendsTrim_lag10$PopulationTrend), 1, 0),
  cens = ifelse(is.na(TrendsTrim_lag10$PopulationTrend), 1, 0),
  lim = cbind(lwr, upr),
  #Var param
  nobs = N_observations,
  wg_a = abs_weight,
  #Add environmental parameters
  pdc = as.vector(scale(PDC)),
  prc = as.vector(scale(abs(PriC))),
  ntc = as.vector(scale(NatC)),
  #Add climate parameters
  exh = as.vector(scale(ExHeatC)),
  drc = as.vector(scale(DroughtC)),
  #Add governance parameters
  hdi = as.vector(HDI_s),
  hdi_v = (ifelse(is.infinite(1/HDI_sv), 1/min(TrendsTrim_lag10$HDI_sv[which(TrendsTrim_lag10$HDI_sv != 0)]), 1/HDI_sv)),
  gov = as.vector(Gov_s),
  gov_v = (ifelse(is.infinite(1/Gov_sv), 1/min(TrendsTrim_lag10$Gov_sv[which(TrendsTrim_lag10$Gov_sv != 0)]), 1/Gov_sv)),
  hdc = as.vector(scale(HDI_c)),
  gvc = as.vector(scale(Gov_c)),
  conf = as.integer(as.factor(Conf)),
  pr = as.vector(scale(ProArea)),
  #Add trait parameters
  ml = as.vector(scale(MaxLon)),
  bm = as.vector(scale(BodyMass)),
  re = as.vector(Reprod_s),
  re_v = as.vector(1/Reprod_sv),
  gn = as.vector(Gen_s),
  gn_v = as.vector(1/Gen_sv),
  gc = as.vector(scale(Gen2)),
  #Add controls
  ar = as.vector(scale(log10(Area))),
  #Add heirarchy
  reg = as.numeric(droplevels(as.factor(Conts))),
  cou = as.numeric(droplevels(as.factor(Singular_country))),
  reg_n = length(unique(Conts)),
  cou_n = length(unique(Singular_country)),
  gen = as.numeric(droplevels(as.factor(Genus))),
  spec = as.numeric(as.factor(Species)),
  gen_n = length(unique(Genus)),
  spec_n = length(unique(Species))
))


TrendsTrim_quan = subset(TrendsTrim_lag5, !is.na(PopulationTrend))
jagsdata_quan = with(TrendsTrim_quan,  list(
  pt = ihs((exp(PopulationTrend)-1)*100),
  len = nrow(TrendsTrim_quan),
  #Var param
  nobs = N_observations,
  wg_a = abs_weight,
  #Add environmental parameters
  pdc = as.vector(scale(PDC)),
  prc = as.vector(scale(abs(PriC))),
  ntc = as.vector(scale(NatC)),
  #Add climate parameters
  exh = as.vector(scale(ExHeatC)),
  drc = as.vector(scale(DroughtC)),
  #Add governance parameters
  hdi = as.vector(HDI_s),
  hdi_v = (ifelse(is.infinite(1/HDI_sv), 1/min(TrendsTrim_lag10$HDI_sv[which(TrendsTrim_lag10$HDI_sv != 0)]), 1/HDI_sv)),
  gov = as.vector(Gov_s),
  gov_v = (ifelse(is.infinite(1/Gov_sv), 1/min(TrendsTrim_lag10$Gov_sv[which(TrendsTrim_lag10$Gov_sv != 0)]), 1/Gov_sv)),
  hdc = as.vector(scale(HDI_c)),
  gvc = as.vector(scale(Gov_c)),
  conf = as.integer(as.factor(Conf)),
  pr = as.vector(scale(ProArea)),
  #Add trait parameters
  ml = as.vector(scale(MaxLon)),
  bm = as.vector(scale(BodyMass)),
  re = as.vector(Reprod_s),
  re_v = as.vector(1/Reprod_sv),
  gn = as.vector(Gen_s),
  gn_v = as.vector(1/Gen_sv),
  gc = as.vector(scale(Gen2)),
  #Add controls
  ar = as.vector(scale(log10(Area))),
  #Add heirarchy
  reg = as.numeric(droplevels(as.factor(Conts))),
  cou = as.numeric(droplevels(as.factor(Singular_country))),
  reg_n = length(unique(Conts)),
  cou_n = length(unique(Singular_country)),
  gen = as.numeric(droplevels(as.factor(Genus))),
  spec = as.numeric(as.factor(Species)),
  gen_n = length(unique(Genus)),
  spec_n = length(unique(Species))
))

TrendsTrim_man = subset(TrendsTrim_lag5, Quantitative_method == "Manual calculation required")
jagsdata_man = with(TrendsTrim_man,  list(
  pt = ihs((exp(PopulationTrend)-1)*100),
  len = nrow(TrendsTrim_man),
  #Var param
  nobs = N_observations,
  wg_a = abs_weight,
  #Add environmental parameters
  pdc = as.vector(scale(PDC)),
  prc = as.vector(scale(abs(PriC))),
  ntc = as.vector(scale(NatC)),
  #Add climate parameters
  exh = as.vector(scale(ExHeatC)),
  drc = as.vector(scale(DroughtC)),
  #Add governance parameters
  hdi = as.vector(HDI_s),
  hdi_v = (ifelse(is.infinite(1/HDI_sv), 1/min(TrendsTrim_lag10$HDI_sv[which(TrendsTrim_lag10$HDI_sv != 0)]), 1/HDI_sv)),
  gov = as.vector(Gov_s),
  gov_v = (ifelse(is.infinite(1/Gov_sv), 1/min(TrendsTrim_lag10$Gov_sv[which(TrendsTrim_lag10$Gov_sv != 0)]), 1/Gov_sv)),
  hdc = as.vector(scale(HDI_c)),
  gvc = as.vector(scale(Gov_c)),
  conf = as.integer(as.factor(Conf)),
  pr = as.vector(scale(ProArea)),
  #Add trait parameters
  ml = as.vector(scale(MaxLon)),
  bm = as.vector(scale(BodyMass)),
  re = as.vector(Reprod_s),
  re_v = as.vector(1/Reprod_sv),
  gn = as.vector(Gen_s),
  gn_v = as.vector(1/Gen_sv),
  gc = as.vector(scale(Gen2)),
  #Add controls
  ar = as.vector(scale(log10(Area))),
  #Add heirarchy
  reg = as.numeric(droplevels(as.factor(Conts))),
  cou = as.numeric(droplevels(as.factor(Singular_country))),
  reg_n = length(unique(Conts)),
  cou_n = length(unique(Singular_country)),
  gen = as.numeric(droplevels(as.factor(Genus))),
  spec = as.numeric(as.factor(Species)),
  gen_n = length(unique(Genus)),
  spec_n = length(unique(Species))
))


a = ggplot(data = TrendsTrim_lag10) +
  geom_density(aes(x = abs_weight)) +
  labs(x = "Observation weights\n", y = "Density") +
  theme_classic()
a


