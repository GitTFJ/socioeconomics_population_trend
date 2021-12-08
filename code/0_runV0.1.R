#Load packages ----
library(raster)
library(sp)
library(rgdal)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape)
library(data.table)
library(mice)
library(lattice)
library(lme4)
library(Rphylopars)
library(phytools)
library(norm)
library(ggplot2)
library(ggstance)
library(ggeffects)
library(ggpubr)
library(ggridges)
library(ggnewscale)
library(ggtree)
library(ggtreeExtra)
library(viridis)
library(directlabels)
library(grid)
library(ncdf4) 
library(sf)
library(xml2)
library(rvest)
library(tidyverse)
library(spdep)
library(compositions)
library(rgeos)
library(R2jags)
library(loo)
library(MetricsWeighted)
library(SPEI)
library(car)
library(segmented)
library(geoR)
library(phytools)
library(reshape2)
library(RColorBrewer)
library(stringr)
library(rworldmap)
library(betareg)
setwd("ENTER OWN DIRECTORY")


#Spatially represent populations ----
#source(file = "code/1_spatial_representation_v0.1.R")
#source(file = "code/2_create_polygonsV0.1.R")

#Land-use  ----
#source(file = "code/3_extract_environment_v0.1.R")
#source(file = "code/4_assign_environment_v0.1.R")
#source(file = "code/5_model_environment_v0.1.R")
#rm(list=setdiff(ls(), c("CoordJoin","Coords")))

#Climate ----
#source(file = "code/6_download_chelsa_v0.1.R")
#source(file = "code/7_extract_climate_v0.1.R")
#source(file = "code/8_model_climate_v0.1.R")
#rm(list=setdiff(ls(), c("CoordJoin","Coords")))

#Governance ----
#source(file = "code/9_impute_governance_v0.1.R")
#source(file = "code/10_plot_governance_imputation_v0.1.R")
#rm(list=setdiff(ls(), c("CoordJoin","Coords")))

#Protected areas ----
#source(file = "code/11_download_pas_v0.1.R")
#source(file = "code/12_protected_areas_v0.1.R")

#Traits ----
#source(file = "code/13_impute_traits_v0.1.R")
#source(file = "code/14_plot_trait_imputation_v0.1.R")
#rm(list=setdiff(ls(), c("CoordJoin","Coords")))

#Trend ----
#source(file = "code/15_prepare_trend_v0.2.R")
#rm(list=setdiff(ls(), c("Trends", "Coords")))

#Add covariates ----
#source(file = "code/16_add_covariates_v0.2.R")
#rm(list=setdiff(ls(), c("")))
Trends = readRDS("data/DataToModel.rds")

#Select model ----
source("code/17_simulate_trend_error_v0.1.R")
source("code/18_prepare_data_v0.6.R")
source("code/19_weight_models_v0.3.R")
source("code/20_level_models_v0.2.R")
source("code/21_lag_models_v0.2.R")
source("code/22_selected_model_v0.2.R")

#Describe data ----

#Assess model ----
source("code/23_check_assumptions_v0.1.R")
source("code/24_scrutinise_model_v0.4.R")
source("code/25_prepare_region_intercept_v0.1.R")
source("code/26_prepare_country_intercept_v0.1.R")
source("code/27_prepare_genus_intercept_v0.1.R")
source("code/28_prepare_species_intercept_v0.1.R")
source("code/29_scenario_plots_v0.3.R")


