species_int = readRDS("SpeciesIntercept.rds")[,c(2,3)]
colnames(species_int) = c("Species_intercept", "Species")
genus_int = readRDS("GenusIntercept.rds")[,c(2,3)]
colnames(genus_int) = c("Genus_intercept", "Genus")
region_int = readRDS("regionIntercept.rds") [,c(4,1)]
colnames(region_int) = c("Region_intercept", "Conts")
country_int = readRDS("countryIntercept.rds")[,c(4,6)]
colnames(country_int) = c("Country_intercept", "alpha.3")


preds_comb = NULL
for(a in 1:4){
  scenario_df = TrendsTrim_lag10
  scenario_df = left_join(scenario_df, species_int)
  scenario_df = left_join(scenario_df, genus_int)
  scenario_df = left_join(scenario_df, region_int)
  scenario_df = left_join(scenario_df, country_int)
  if(a == 1){
    print("Observed scenario")
    scenario_df$PriC = scale(abs(scenario_df$PriC))
    scenario_df$ExHeatC = scale(scenario_df$ExHeatC)
    scenario_df$DroughtC = scale(scenario_df$DroughtC)
    scenario_df$HDI_c = scale(scenario_df$HDI_c)
  } else if (a == 2) {
    print("Scenario 1 - No loss of primary vegetation")
    scenario_df$PriC = min(scale(abs(scenario_df$PriC)))
    scenario_df$ExHeatC = scale(scenario_df$ExHeatC)
    scenario_df$DroughtC = scale(scenario_df$DroughtC)
    scenario_df$HDI_c = scale(scenario_df$HDI_c)
  } else if (a == 3) {
    print("Scenario 2 - No climate change")
    scenario_df$ExHeatC = -0.362
    scenario_df$DroughtC = -0.251
    scenario_df$PriC = scale(abs(scenario_df$PriC))
    scenario_df$HDI_c = scale(scenario_df$HDI_c)
  } else {
    print("Scenario 3 - No change in human development")
    scenario_df$HDI_c = -2.03
    scenario_df$PriC = scale(abs(scenario_df$PriC))
    scenario_df$ExHeatC = scale(scenario_df$ExHeatC)
    scenario_df$DroughtC = scale(scenario_df$DroughtC)
  }
  alpha  = merged.chains[ , grep("alpha", colnames(merged.chains), fixed = T)]
  alpha  = median(alpha [,-c(grep("sd", colnames(alpha)))])
  
  betas = c()
  for(b in 1:nrow(Dictionary)){
    betas = c(betas,median(merged.chains[ , grep(Dictionary[b,]$variable, colnames(merged.chains), fixed = T)]))
  }
  
  preds = sinh(
    alpha +
    scenario_df$Species_intercept +
    scenario_df$Genus_intercept +
    scenario_df$Country_intercept +
    scenario_df$Region_intercept +
    scale((log10(scenario_df[,Dictionary$TrendsTrimName[1]])))*betas[1] +
    ((scenario_df[,Dictionary$TrendsTrimName[2]]))*betas[2] +
    scale((scenario_df[,Dictionary$TrendsTrimName[3]]))*betas[3] +
    ((scenario_df[,Dictionary$TrendsTrimName[4]]))*betas[4] +
    scale((scenario_df[,Dictionary$TrendsTrimName[5]]))*betas[5] +
    scale((scenario_df[,Dictionary$TrendsTrimName[6]]))*betas[6] +
    scale((scenario_df[,Dictionary$TrendsTrimName[7]]))*betas[7] +
    ((scenario_df[,Dictionary$TrendsTrimName[8]]))*betas[8] +
    scale((scenario_df[,Dictionary$TrendsTrimName[9]]))*betas[9] +
    scale((scenario_df[,Dictionary$TrendsTrimName[10]]))*betas[10] +
    ((scenario_df[,Dictionary$TrendsTrimName[11]]))*betas[11] +
    (as.integer(as.factor(scenario_df[,Dictionary$TrendsTrimName[12]]))-1)*betas[12] +
    scale((scenario_df[,Dictionary$TrendsTrimName[13]]))*betas[13] +
    scale((scenario_df[,Dictionary$TrendsTrimName[14]]))*betas[14] +
    scale((scenario_df[,Dictionary$TrendsTrimName[15]]))*betas[15] +
    scale((scenario_df[,Dictionary$TrendsTrimName[16]]))*betas[16] +
    ((scenario_df[,Dictionary$TrendsTrimName[2]])) * ((scenario_df[,Dictionary$TrendsTrimName[8]])) * betas[17] +
    ((scenario_df[,Dictionary$TrendsTrimName[2]])) * scale((scenario_df[,Dictionary$TrendsTrimName[16]])) * betas[18] +
    ((scenario_df[,Dictionary$TrendsTrimName[2]])) * scale((scenario_df[,Dictionary$TrendsTrimName[5]])) * betas[19] +
    ((scenario_df[,Dictionary$TrendsTrimName[2]])) * scale((scenario_df[,Dictionary$TrendsTrimName[7]])) * betas[20] +
    ((scenario_df[,Dictionary$TrendsTrimName[4]])) * scale((scenario_df[,Dictionary$TrendsTrimName[9]])) * betas[21] +
    ((scenario_df[,Dictionary$TrendsTrimName[4]])) * scale((scenario_df[,Dictionary$TrendsTrimName[3]])) * betas[22] +
    ((scenario_df[,Dictionary$TrendsTrimName[4]])) * scale((scenario_df[,Dictionary$TrendsTrimName[15]])) * betas[23] )
  preds_comb = cbind(preds_comb, preds)
}
preds_comb = as.data.frame(preds_comb)
colnames(preds_comb) = c("sc1", "sc2", "sc3", "sc4")
preds_comb$diff2 = (preds_comb$sc2 - preds_comb$sc1)
preds_comb$diff3 = (preds_comb$sc3 - preds_comb$sc1)
preds_comb$diff4 = (preds_comb$sc4 - preds_comb$sc1)
scenario_df = cbind(scenario_df, preds_comb)


sum_cmb = scenario_df %>%
  group_by(Conts) %>%
  dplyr::summarise(
    mn2 = median(diff2, na.rm = T),
    mn3 = median(diff3, na.rm = T),
    mn4 = median(diff4, na.rm = T),
    ql2 = quantile(diff2, probs = 0.25, na.rm = T),
    ql3 = quantile(diff3, probs = 0.25, na.rm = T),
    ql4 = quantile(diff4, probs = 0.25, na.rm = T),
    qu2 = quantile(diff2, probs = 0.75, na.rm = T),
    qu3 = quantile(diff3, probs = 0.75, na.rm = T),
    qu4 = quantile(diff4, probs = 0.75, na.rm = T),
    cl2 = quantile(diff2, probs = 0.025, na.rm = T),
    cl3 = quantile(diff3, probs = 0.025, na.rm = T),
    cl4 = quantile(diff4, probs = 0.025, na.rm = T),
    cu2 = quantile(diff2, probs = 0.975, na.rm = T),
    cu3 = quantile(diff3, probs = 0.975, na.rm = T),
    cu4 = quantile(diff4, probs = 0.975, na.rm = T),
    n = n()
    )
sum_cmb = subset(sum_cmb, n > 4)


library(ggridges)

scen1 = ggplot() +
  geom_pointrange(data = sum_cmb, aes(x = mn2, xmin = ql2, xmax = qu2, y = Conts), size = 1, alpha = 0.6) +
  geom_linerange(data = sum_cmb, aes(xmin = cl2, xmax = cu2, y = Conts), size = 0.5, alpha = 0.6) +
  geom_rect(aes(xmin = -5, xmax = 0, ymin = 0, ymax = 6.5), fill = "#e38100", alpha = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 5, ymin = 0, ymax = 6.5), fill = "#4B2991", alpha = 0.3) +
  annotate("text", x=-1, y=6, label= "Decreases", size = 4) + 
  annotate("text", x= 1, y=6, label= "Increases", size = 4) + 
  coord_cartesian(xlim = c(-4.1, 4.1)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  labs(y = " ", x = " ", title = "No loss in primary habitat") +
  theme_classic()


scen2 = ggplot() +
  geom_pointrange(data = sum_cmb, aes(x = mn3, xmin = ql3, xmax = qu3, y = Conts), size = 1, alpha = 0.6) +
  geom_linerange(data = sum_cmb, aes(xmin = cl3, xmax = cu3, y = Conts), size = 0.5, alpha = 0.6) +
  geom_rect(aes(xmin = -5, xmax = 0, ymin = 0, ymax = 6.5), fill = "#e38100", alpha = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 5, ymin = 0, ymax = 6.5), fill = "#4B2991", alpha = 0.3) +
  annotate("text", x=-1, y=6, label= "Decreases", size = 4) + 
  annotate("text", x= 1, y=6, label= "Increases", size = 4) + 
  coord_cartesian(xlim = c(-4.1, 4.1)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  labs(y = " ", x = " ", title = "No climate change") +
  scale_colour_discrete(guide = F) +
  theme_classic()

scen3 = ggplot() +
  geom_pointrange(data = sum_cmb, aes(x = mn4, xmin = ql4, xmax = qu4, y = Conts), size = 1, alpha = 0.6) +
  geom_linerange(data = sum_cmb, aes(xmin = cl4, xmax = cu4, y = Conts), size = 0.5, alpha = 0.6) +
  geom_rect(aes(xmin = -5, xmax = 0, ymin = 0, ymax = 6.5), fill = "#e38100", alpha = 0.3) +
  geom_rect(aes(xmin = 0, xmax = 5, ymin = 0, ymax = 6.5), fill = "#4B2991", alpha = 0.3) +
  annotate("text", x=-1, y=6, label= "Decreases", size = 4) + 
  annotate("text", x= 1, y=6, label= "Increases", size = 4) + 
  coord_cartesian(xlim = c(-4.1, 4.1)) +
  geom_vline(aes(xintercept = 0), linetype = "dashed") +
  labs(y = " ", x = "Difference in\nannual rate of change (%)", title = "No growth in human development") +
  scale_colour_discrete(guide = F) +
  theme_classic()

jpeg("scen_plot.jpeg", width = 5, height = 8, units = "in", res = 300)
ggarrange(scen1, scen2, scen3, ncol = 1, nrow = 3, labels = c("a,", "b,", "c,"))
dev.off()
