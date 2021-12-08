Traits = read.delim("Documents/ModellingTrends/Data/Traits/Traits.txt")
colnames(Traits)[1:5] = c("Order",
                          "Family",
                          "Genus",
                          "Species",
                          "Binomial")
Traits2 = read.delim("Documents/ModellingTrends/Data/Traits/Traits2.txt") 
colnames(Traits2)[1:5] = c("Order",
                           "Family",
                           "Genus",
                           "Species",
                           "Binomial")
Traits = rbind.fill(Traits, Traits2)
rm(Traits2)
Traits[Traits == -999] = NA
Traits = subset(Traits, Order == "Carnivora" & 
                  (Family == "Canidae" | 
                     Family == "Felidae" | 
                     Family == "Ursidae" | 
                     Family == "Hyaenidae"), 
                select = c(
                  Family,
                  Binomial,
                  X26.1_GR_Area_km2, 
                  X26.2_GR_MaxLat_dd, 
                  X26.3_GR_MinLat_dd, 
                  X5.1_AdultBodyMass_g,
                  X17.1_MaxLongevity_m, 
                  X23.1_SexualMaturityAge_d, #reproductive output
                  X15.1_LitterSize, #reproductive output
                  X9.1_GestationLen_d, #reproductive output
                  X25.1_WeaningAge_d, 
                  X14.1_InterBirthInterval_d,
                  X5.3_NeonateBodyMass_g))
colnames(Traits)[2] = "Species"

#Ensure trait species match our species
Traits$Species = as.character(Traits$Species)
Traits$Species[Traits$Species == "Uncia uncia"] <- "Panthera uncia"




library(ape)
library(phytools)
SpeciesMerge = read.csv("Documents/ModellingTrends/Data/Phylogeny/SpeciesListRAW.csv")
SpeciesMerge = data.frame(Species = SpeciesMerge$JoinScience, ID1 = "Traits")
Species = left_join(SpeciesMerge, Traits)
Species$ID1 = NULL
Species = Species[!duplicated(Species),]
colnames(Species) = c("Species",
                      "Family",
                      "Area_km2",
                      "MaxLat_dd",
                      "MinLat_dd",
                      "BodyMass_g",
                      "Longevity_m",
                      "SexMat_d",
                      "LittSize",
                      "GestLen_d",
                      "WeanAge_d",
                      "InterBirthInt_d",
                      "NeonateMass_g"
                      )

anage = read.delim("Documents/ModellingTrends/Data/Traits/anage_data.txt") 
anage$Species = paste(anage$Genus, anage$Species, sep = " ")
anage$SexMat_d = rowMeans(anage[,10:11], na.rm = T)
anage = subset(anage, Order == "Carnivora",
               select = c(
                 "Species",
                 "Family",
                 "Adult.weight..g.",
                 "Maximum.longevity..yrs.",
                 "SexMat_d",
                 "Litter.Clutch.size",
                 "Gestation.Incubation..days.",
                 "Inter.litter.Interbirth.interval"
               ))
anage$SexMat_d[anage$SexMat_d == "NaN"] = NA

library(foreign)
mgs = read.dbf("Documents/ModellingTrends/Data/Traits/DatabaseMG/Updated database 2014_v1/quantitativedatatable_corrected.dbf")
mgs_spec = read.dbf("Documents/ModellingTrends/Data/Traits/DatabaseMG/Updated database 2014_v1/speciestable.dbf")
mgs = left_join(mgs, mgs_spec[,1:4])
rm(mgs_spec)

mgs_diet = read.dbf("Documents/ModellingTrends/Data/Traits/DatabaseMG/Updated database 2014_v1/diettable_corrected.dbf")
mgs_spec = read.dbf("Documents/ModellingTrends/Data/Traits/DatabaseMG/Updated database 2014_v1/speciestable.dbf")
mgs_diet = left_join(mgs_diet, mgs_spec[,1:4])
rm(mgs_spec)

CombDiet = NULL
for(a in unique(mgs_diet$SPECIES_ID)){
  tmp = subset(mgs_diet, SPECIES_ID == a)
  diet_breadth = sum(colSums(tmp[,c(12:22,24)]) != 0)
  spec = paste(tmp$GENUS[1],tmp$SPECIES[1], sep = " ")
  tmp = data.frame(Species = spec, DietBreadth = diet_breadth)
  CombDiet = rbind(CombDiet, tmp)
}

mgs = subset(mgs, AGE == "adult" | AGE == "unspecified")
mgs = subset(mgs, 
             DATA_TYPE == "body mass" |
               DATA_TYPE == "birth interval" |
               DATA_TYPE == "gestation length" |
               DATA_TYPE == "litter size" |
               DATA_TYPE == "longevity" |
               DATA_TYPE == "minimum breed age" |
               DATA_TYPE == "neonatal body mass" |
               DATA_TYPE == "wean age")
mgs = subset(mgs,
             MEASURE_UN != "ind per group" &
               MEASURE_UN != "ind per den" &
               MEASURE_UN != "ind per litter")
mgs$DATA_VALUE = ifelse(mgs$MEASURE_UN == "kg", mgs$DATA_VALUE*1000, mgs$DATA_VALUE)
mgs$DATA_VALUE = ifelse(mgs$MEASURE_UN == "pounds",mgs$DATA_VALUE*453.592, mgs$DATA_VALUE)
mgs$DATA_VALUE = ifelse(mgs$MEASURE_UN == "years", mgs$DATA_VALUE*365.25, mgs$DATA_VALUE)
mgs$DATA_VALUE = ifelse(mgs$MEASURE_UN == "months", mgs$DATA_VALUE*(365.25/12), mgs$DATA_VALUE)
mgs$DATA_VALUE = ifelse(mgs$MEASURE_UN == "weeks", mgs$DATA_VALUE*(365.25/52), mgs$DATA_VALUE)
mgs$Species = paste(mgs$GENUS, mgs$SPECIES, sep = " ")

CombDF = NULL
for(a in 1:nrow(Species)){
  Spec = Species$Species[a]
  Pant_bm = Species$BodyMass_g[a]
  Anage_bm = anage[which(anage$Species == Spec),3]
  mgs_bm = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "body mass")[,15]
  Pant_lon = Species$Longevity_m[a]
  Anage_lon = anage[which(anage$Species == Spec),4]
  mgs_lon = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "longevity")[,15]
  Pant_sm = Species$SexMat_d[a]
  Anage_sm = anage[which(anage$Species == Spec),5]
  mgs_sm = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "minimum breed age")[,15]
  Pant_lit = Species$LittSize[a]
  Anage_lit = anage[which(anage$Species == Spec),6]
  mgs_lit = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "litter size")[,15]
  Pant_gest = Species$BodyMass_g[a]
  Anage_gest = anage[which(anage$Species == Spec),7]
  mgs_gest = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "gestation length")[,15]
  Pant_int = Species$BodyMass_g[a]
  Anage_int = anage[which(anage$Species == Spec),8]
  mgs_int = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "birth interval")[,15]
  Pant_wa = Species$WeanAge_d[a]
  mgs_wa = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "wean age")[,15]
  Pant_nm = Species$NeonateMass_g[a]
  mgs_nm = subset(mgs, 
                  Species == Spec &
                    DATA_TYPE == "neonatal body mass")[,15]
  Nrow = max(length(mgs_bm),
             length(mgs_lon),
             length(mgs_sm),
             length(mgs_lit),
             length(mgs_gest),
             length(mgs_wa),
             length(mgs_int),
             length(mgs_nm))
  
  if(length(Anage_bm) == 0){
    Anage_bm = NA
  } else {
  }
  if(length(Anage_lon) == 0){
    Anage_lon = NA
  } else {
  }
  if(length(Anage_sm) == 0){
    Anage_sm = NA
  } else {
  }
  if(length(Anage_lit) == 0){
    Anage_lit = NA
  } else {
  }
  if(length(Anage_gest) == 0){
    Anage_gest = NA
  } else {
  }
  if(length(Anage_int) == 0){
    Anage_int = NA
  } else {
  }

  
  if(length(mgs_bm) == 0){
    mgs_bm = NA
  } else {
  }
  if(length(mgs_lon) == 0){
    mgs_lon = NA
  } else {
  }
  if(length(mgs_sm) == 0){
    mgs_sm = NA
  } else {
  }
  if(length(mgs_lit) == 0){
    mgs_lit = NA
  } else {
  }
  if(length(mgs_gest) == 0){
    mgs_gest = NA
  } else {
  }
  if(length(mgs_wa) == 0){
    mgs_wa = NA
  } else {
  }
  if(length(mgs_int) == 0){
    mgs_int = NA
  } else {
  }
  if(length(mgs_nm) == 0){
    mgs_nm = NA
  } else {
  }
  
  if(Nrow > 0){
    mgs_bm = data.frame(id = 1:Nrow, trait = c(mgs_bm, rep(NA, Nrow - length(mgs_bm))))
    mgs_lon = data.frame(id = 1:Nrow, trait = c(mgs_lon, rep(NA, Nrow - length(mgs_lon))))
    mgs_sm = data.frame(id = 1:Nrow, trait = c(mgs_sm, rep(NA, Nrow - length(mgs_sm))))
    mgs_lit = data.frame(id = 1:Nrow, trait = c(mgs_lit, rep(NA, Nrow - length(mgs_lit))))
    mgs_gest = data.frame(id = 1:Nrow, trait = c(mgs_gest, rep(NA, Nrow - length(mgs_gest))))
    mgs_wa = data.frame(id = 1:Nrow, trait = c(mgs_wa, rep(NA, Nrow - length(mgs_wa))))
    mgs_int = data.frame(id = 1:Nrow, trait = c(mgs_int, rep(NA, Nrow - length(mgs_int))))
    mgs_nm = data.frame(id = 1:Nrow, trait = c(mgs_nm, rep(NA, Nrow - length(mgs_nm))))
  } else {
    message("No records in mgs")
    mgs_bm = data.frame(id = 1, trait = NA)
    mgs_lon = data.frame(id = 1, trait = NA)
    mgs_sm = data.frame(id = 1, trait = NA)
    mgs_lit = data.frame(id = 1, trait = NA)
    mgs_gest = data.frame(id = 1, trait = NA)
    mgs_wa = data.frame(id = 1, trait = NA)
    mgs_int = data.frame(id = 1, trait = NA)
    mgs_nm = data.frame(id = 1, trait = NA)
  }

  Nrow = ifelse(Nrow == 0, 1, Nrow)

  
  df = data.frame(Species = Spec,
                       Family = Species$Family[a],
                       BodyMass_g = c(Pant_bm, Anage_bm, mgs_bm[,2]),
                       Longevity_d = c(Pant_lon*(365.25/12), (Anage_lon*12*(365.25/12)), mgs_lon[,2]),
                       SexMat_d = c(Pant_sm, Anage_sm, mgs_sm[,2]),
                       LittSize = c(Pant_lit, Anage_lit, mgs_lit[,2]),
                       GestLen_d = c(Pant_gest, Anage_gest, mgs_gest[,2]),
                       WeanAge_d = c(Pant_wa, NA, mgs_wa[,2]),
                       InterBirthInt_d = c(Pant_int, Anage_int, mgs_int[,2]),
                       NeonateMass_g = c(Pant_nm, NA, mgs_nm[,2]),
                  Source = c("Pan", "AnAge", rep("MGS", Nrow))
                  )
  df$max_lat_dd = Species$MaxLat_dd[a]
  df$min_lat_dd = Species$MinLat_dd[a]
  
  if(is.na(Species$MaxLat_dd[a])){
  } else {
    if(Species$MaxLat_dd[a] > 0 & Species$MinLat_dd[a] >= 0){
      #Both positive
      LatExtent = Species$MaxLat_dd[a] - Species$MinLat_dd[a]
    } else if (Species$MaxLat_dd[a] <= 0 & Species$MinLat_dd[a] < 0){
      #Both negative  
      LatExtent = abs(Species$MinLat_dd[a]) - abs(Species$MaxLat_dd[a] > 0)
    } else {
      LatExtent = Species$MaxLat_dd[a] - Species$MinLat_dd[a]
    }
  }
  df$lat_extent_dd = LatExtent
  
  CombDF = rbind(CombDF, df)
}
CombDF = CombDF[!duplicated(CombDF), ]
CombCli = readRDS("Documents/ModellingTrends/Data/Traits/climate_gen.rds")[,c(1,6,7,8)]
CombDF = left_join(CombDF, CombCli, by = c("Species" = "species"))

mgs_diet = read.dbf("Documents/ModellingTrends/Data/Traits/DatabaseMG/Updated database 2014_v1/diettable_corrected.dbf")
mgs_spec = read.dbf("Documents/ModellingTrends/Data/Traits/DatabaseMG/Updated database 2014_v1/speciestable.dbf")
mgs_diet = left_join(mgs_diet, mgs_spec[,1:4])
rm(mgs_spec)

TraitList = list()
for(n in 1:3){
Tree = read.nexus("Documents/ModellingTrends/Data/Phylogeny/Nyakatura_&_Bininda-Edmonds_2012_CarnivoraSupertree.nex")
Tree = Tree[[n]]
Tree$tip.label = gsub("_", " ", Tree$tip.label)
SpeciesMerge2 = data.frame(Species = Tree$tip.label, ID2 = "Tree")
NonMathcedSpecies = anti_join(SpeciesMerge, SpeciesMerge2) 


Tree$tip.label[Tree$tip.label == "Puma yagouaroundi"] <- "Herpailurus yagouaroundi"
Tree$tip.label[Tree$tip.label == "Leopardus jacobitus"] <- "Leopardus jacobita"
Tree$tip.label[Tree$tip.label == "Felis manul"] <- "Otocolobus manul"
Tree$tip.label[Tree$tip.label == "Uncia uncia"] <- "Panthera uncia"
Tree$tip.label[Tree$tip.label == "Hyaena brunnea"] <- "Parahyaena brunnea"


Tree = bind.tip(Tree, 
                tip.label = "Caracal aurata", 
                edge.length=0.0001, 
                where = nodepath(
                  Tree, 
                  grep("Caracal caracal", Tree$tip.label), 
                  grep("Caracal caracal", Tree$tip.label) + 1)[2], 
                Position = 0)

Tree$tip.label = gsub("_", " ", Tree$tip.label)

Tree = bind.tip(Tree, 
                tip.label = "Dusicyon avus", 
                edge.length=0.0001, 
                where = nodepath(
                  Tree, 
                  grep("Dusicyon australis", Tree$tip.label), 
                  grep("Dusicyon australis", Tree$tip.label) + 1)[2], 
                Position = 0)

Tree$tip.label = gsub("_", " ", Tree$tip.label)


Tree = bind.tip(Tree, 
                tip.label = "Canis rufus", 
                edge.length=0.0001, 
                where = nodepath(
                  Tree, 
                  grep("Canis lupus", Tree$tip.label), 
                  grep("Canis lupus", Tree$tip.label) + 1)[2], 
                Position = 0)

Tree$tip.label = gsub("_", " ", Tree$tip.label)
Tree = bind.tip(Tree, 
                tip.label = "Leopardus guttulus", 
                edge.length=0.0001, 
                where = nodepath(
                  Tree, 
                  grep("Leopardus geoffroyi", Tree$tip.label), 
                  grep("Leopardus geoffroyi", Tree$tip.label) + 1)[2], 
                Position = 0)

Tree$tip.label = gsub("_", " ", Tree$tip.label)
Tree = bind.tip(Tree, 
                tip.label = "Neofelis diardi", 
                edge.length=0.0001, 
                where = nodepath(
                  Tree, 
                  grep("Neofelis nebulosa", Tree$tip.label), 
                  grep("Neofelis nebulosa", Tree$tip.label) + 1)[2], 
                Position = 0)
Tree$tip.label = gsub("_", " ", Tree$tip.label)
PrunedTree = drop.tip(Tree,
                      setdiff(Tree$tip.label,
                              SpeciesMerge$Species))
if(n == 1){
  saveRDS(PrunedTree, "PrunedTree.rds")
} else {
}




CombDF$BodyMass_log10 = log10(CombDF$BodyMass_g)
CombDF$Longevity_log10 = log10(CombDF$Longevity_d)
CombDF$SexMat_log10 = log10(CombDF$SexMat_d)
CombDF$LitSize_log10 = log10(CombDF$LittSize)
CombDF$GestLen_log10 = log10(CombDF$GestLen_d)
CombDF$WeanAge_log10 = log10(CombDF$WeanAge_d)
CombDF$IntBir_log10 = log10(CombDF$InterBirthInt_d)
CombDF$NeoMass_log10 = log10(CombDF$NeonateMass_g)

CombDiet = NULL
for(m in unique(mgs_diet$SPECIES_ID)){
  tmp = subset(mgs_diet, SPECIES_ID == m)
  diet_breadth = sum(colSums(tmp[,c(12:22,24)]) != 0)
  spec = paste(tmp$GENUS[1],tmp$SPECIES[1], sep = " ")
  tmp = data.frame(Species = spec, DietBreadth = diet_breadth)
  CombDiet = rbind(CombDiet, tmp)
}
CombHab = readRDS("Documents/ModellingTrends/Data/Traits/Habitat.rds")
CombDF = left_join(CombDF, CombDiet)
CombDF = left_join(CombDF, CombHab)


Species = CombDF[,c(1,12:27)]
colnames(Species)[1] = "species"
Species$DietBreadth = as.numeric(Species$DietBreadth)
Species$HabBreadth = as.numeric(Species$HabBreadth)

tmp = Species %>%
  group_by(species) %>% 
  summarise(
    clsd = mean(clim_mn_sd, na.rm = T),
    bm = mean(BodyMass_log10, na.rm = T),
    ln = mean(Longevity_log10, na.rm = T),
    hb = mean(HabBreadth, na.rm = T),
    di = mean(DietBreadth, na.rm = T),
    sm = mean(SexMat_log10, na.rm = T),
    ls = mean(LitSize_log10, na.rm = T),
    gl = mean(GestLen_log10, na.rm = T),
    wa = mean(WeanAge_log10, na.rm = T),
    ib = mean(IntBir_log10, na.rm = T),
    nm = mean(NeoMass_log10, na.rm = T)
  )
tmp2 = colSums(is.na(tmp[,c(2:12)])/87)*100 
View(tmp2)

PPE = phylopars(trait_data = Species, 
                tree = PrunedTree,
                pheno_error = F,
                pheno_correlated = F,
                phylo_correlated = T)
Traits = as.data.frame(PPE$anc_recon[1:87,])
TraitsVar = as.data.frame(PPE$anc_var[1:87,])
colnames(TraitsVar) = paste(colnames(TraitsVar),"_Var", sep = "")
Traits = cbind(Traits, TraitsVar)
Traits$Species = rownames(Traits)


Join = NULL
for(a in 1:100){
  SexMatComb = NULL
  LitSizeComb = NULL
  GestLenComb = NULL
  WeanAgeComb = NULL
  IntBirthComb = NULL
  NeoMassComb = NULL
  HabBreadthComb = NULL
  DietBreadthComb = NULL
  for(b in 1:nrow(Traits)){
    SexMatTemp = rnorm(1, Traits$SexMat_log10[b], sqrt(Traits$SexMat_log10_Var[b]))
    SexMatComb = rbind(SexMatComb, SexMatTemp)
    LitSizeTemp = rnorm(1, Traits$LitSize_log10[b], sqrt(Traits$LitSize_log10_Var[b]))
    LitSizeComb = rbind(LitSizeComb, LitSizeTemp)
    GestLenTemp = rnorm(1, Traits$GestLen_log10[b], sqrt(Traits$GestLen_log10_Var[b]))
    GestLenComb = rbind(GestLenComb, GestLenTemp)
    WeanAgeTemp = rnorm(1, Traits$WeanAge_log10[b], sqrt(Traits$WeanAge_log10_Var[b]))
    WeanAgeComb = rbind(WeanAgeComb, WeanAgeTemp)
    IntBirthTemp = rnorm(1, Traits$IntBir_log10[b], sqrt(Traits$IntBir_log10_Var[b]))
    IntBirthComb = rbind(IntBirthComb, IntBirthTemp)
    NeoMassTemp = rnorm(1, Traits$NeoMass_log10[b], sqrt(Traits$NeoMass_log10_Var[b]))
    NeoMassComb = rbind(NeoMassComb, NeoMassTemp)
    HabBreadthTemp = rnorm(1, Traits$HabBreadth[b], sqrt(Traits$HabBreadth_Var[b]))
    HabBreadthComb = rbind(HabBreadthComb, HabBreadthTemp)
    DietBreadthTemp = rnorm(1, Traits$DietBreadth[b], sqrt(Traits$DietBreadth_Var[b]))
    DietBreadthComb = rbind(DietBreadthComb, DietBreadthTemp)
  }
  Comb = data.frame(SexMat = SexMatComb,
                    LitSize = LitSizeComb,
                    GestLen = GestLenComb,
                    WeanAge = WeanAgeComb,
                    IntBirth = IntBirthComb,
                    NeoMass = NeoMassComb,
                    HabBreadth = HabBreadthComb,
                    DietBreadth = DietBreadthComb,
                    Model = a,
                    Species = Traits$Species)
  PCA_reprod = prcomp(Comb[,c("SexMat",
                              "LitSize",
                              "GestLen",
                              "WeanAge",
                              "IntBirth",
                              "NeoMass")], center = T, scale. = T)
  Comb$ReprodRateOutput = PCA_reprod$x[,1]
  PCA_gen = prcomp(Comb[,c("HabBreadth",
                           "DietBreadth")], center = T, scale. = T)
  Comb$GenOutput = PCA_gen$x[,1]
  
  Join = rbind(Join, Comb)
}

TraitPCA = Join %>%
  group_by(Species) %>%
  dplyr::summarise(ReprodRate_mean = mean(ReprodRateOutput, na.rm = T),
                   ReprodRate_var = var(ReprodRateOutput, na.rm = T),
                   Gen_mean = mean(GenOutput, na.rm = T),
                   Gen_var = var(GenOutput, na.rm = T)
                   )
  


Traits = Traits[,c(33,1:8,15:16,23:24)]

Traits = left_join(Traits, TraitPCA)
TraitList[[n]] = Traits
}
saveRDS(TraitList, "Documents/ModellingTrends/Data/Traits/TraitsFormatted.rds")
