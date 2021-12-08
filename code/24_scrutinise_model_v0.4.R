mod.mcmc = as.mcmc(mod)
merged.chains = do.call(rbind, mod.mcmc)

deltas = merged.chains[,grep("delta", colnames(merged.chains))]
model.combos = NULL
id = gsub("[^0-9A-Za-z///' ]","", colnames(deltas))
id = as.numeric(gsub("delta","", id))
for(i in 1:nrow(deltas)){
  model.combos[i] = paste(id[deltas[i,]==1],collapse=",")
}
deltas = as.data.frame(deltas)
deltas$model.combos = model.combos
model.combos = as.data.frame(table(model.combos)/nrow(deltas))
model.combos = model.combos[order(model.combos$Freq, decreasing = T),]
deltas = left_join(deltas, model.combos)

mod = as.data.frame(merged.chains[,grepl("mod", colnames(merged.chains))])
mod = mod[,-c(grep("sd", colnames(mod)))]
v_fixed = apply(mod, 1, function(x) sd(x)^2)
v_resid = merged.chains[,grepl("sd.mod", colnames(merged.chains))]^2
v_resid = 1/v_resid
v_resid = as.matrix(jagsdata_lag10$wg_a) %*% v_resid
v_resid = 1/v_resid
v_resid = apply(v_resid, 2, function(x) median(x))
v_random_reg = (merged.chains[,grepl("sd.reg", colnames(merged.chains))])^2
v_random_cou = (merged.chains[,grepl("sd.cou", colnames(merged.chains))])^2
v_random_gen = (merged.chains[,grepl("sd.gen", colnames(merged.chains))])^2
v_random_spec = (merged.chains[,grepl("sd.spec", colnames(merged.chains))])^2


pred = as.data.frame(merged.chains[,grepl("mod", colnames(merged.chains))])
pred = pred[,-c(grep("sd.mod", colnames(pred)))]
true = as.data.frame(merged.chains[,grepl("pt", colnames(merged.chains))])
true = true[,-c(grep("pt.pred", colnames(true)))]
wg_df = data.frame(
  id = colnames(pred)
)
wg_df2 = data.frame(
  id = paste0("mod[", 1:length(jagsdata_lag10$pt), "]"),
  weig = jagsdata_lag10$wg_a
)
wg_df = left_join(wg_df, wg_df2)

diff_val = sinh(true) - sinh(pred)
diff_val = diff_val^2
wrmse_vec = sqrt(apply(diff_val, 1, function(x) weighted_mean(x, wg_df$weig)))

rmse_plt = ggplot() +
  geom_density(aes(x = wrmse_vec), colour = "grey", fill = "light grey", alpha = 0.4) +
  labs(x = "Weighted RMSE") +
  scale_x_continuous(limits = c(8.5, 10)) +
  theme_classic()

r2_df = data.frame(
  fix = v_fixed,
  res = v_resid,
  reg = v_random_reg,
  cou = v_random_cou,
  gen = v_random_gen,
  spe = v_random_spec
)
r2_df$marg = r2_df$fix/(r2_df$fix + r2_df$res + r2_df$reg + r2_df$cou + r2_df$gen + r2_df$spe) 
r2_df$cond = (r2_df$fix + r2_df$reg + r2_df$cou + r2_df$gen + r2_df$spe)/(r2_df$fix + r2_df$res + r2_df$reg + r2_df$cou + r2_df$gen + r2_df$spe) 


r2_m = ggplot(r2_df) +
  geom_density(aes(x = marg), colour = "grey", fill = "light grey", alpha = 0.4) +
  #scale_x_continuous(limits = c(0,0.25)) +
  labs(x = expression(Marginal~R^{2})) +
  theme_classic()

r2_c = ggplot(r2_df) +
  geom_density(aes(x = cond), colour = "grey", fill = "light grey", alpha = 0.4) +
  #scale_x_continuous(limits = c(0,0.25)) +
  labs(x = expression(Conditional~R^{2})) +
  theme_classic()





jpeg("model_fit.jpeg", width = 9, height = 3, units = "in", res = 300)

ggarrange(
  rmse_plt,
  r2_m,
  r2_c,
  ncol = 3,
  labels = c("a", "b", "c"),
  align = "h")
dev.off()



betas = cbind(deltas, merged.chains[,grep("beta", colnames(merged.chains))])
for(b in 8:23){
  delta.position = grep(paste("delta[",b,"]",sep = ""),colnames(betas), fixed =T)
  beta.position = grep(paste("beta[",b,"]",sep = ""),colnames(betas), fixed =T)
  betas[,beta.position] = ifelse(betas[,delta.position] == 0, 
                                 NA,
                                 betas[,beta.position])
}
colnames(betas)
betas = betas[,c(19:41)]
betas$id = rownames(betas)
betas.melt = melt(betas, id = "id")
mp = function(vals){sinh(vals)}
betas.sum = betas.melt %>%
  group_by(variable) %>%
  dplyr::summarise(
    n = sum(!is.na(value)),
    rvi = sum(!is.na(value))/nrow(betas),
    coef = mp(median(value, na.rm = T)),
    lci_50 = mp(quantile(value, probs = c(0.25), na.rm = T)),
    uci_50 = mp(quantile(value, probs = c(0.75), na.rm = T)),
    lci_80 = mp(quantile(value, probs = c(0.1), na.rm = T)),
    uci_80 = mp(quantile(value, probs = c(0.9), na.rm = T)),
    lci_90 = mp(quantile(value, probs = c(0.05), na.rm = T)),
    uci_90 = mp(quantile(value, probs = c(0.95), na.rm = T)),
    lci_95 = mp(quantile(value, probs = c(0.025), na.rm = T)),
    uci_95 = mp(quantile(value, probs = c(0.975), na.rm = T)),
    lci_99 = mp(quantile(value, probs = c(0.01), na.rm = T)),
    uci_99 = mp(quantile(value, probs = c(0.99), na.rm = T)))
betas.sum[which(betas.sum$variable == "beta[17]"),]$rvi =  betas.sum[which(betas.sum$variable == "beta[17]"),]$n/sum(deltas[,c("delta[8]")])
betas.sum[which(betas.sum$variable == "beta[18]"),]$rvi =  betas.sum[which(betas.sum$variable == "beta[18]"),]$n/sum(deltas[,c("delta[16]")])
betas.sum[which(betas.sum$variable == "beta[21]"),]$rvi =  betas.sum[which(betas.sum$variable == "beta[21]"),]$n/sum(deltas[,c("delta[9]")])
betas.sum[which(betas.sum$variable == "beta[23]"),]$rvi =  betas.sum[which(betas.sum$variable == "beta[23]"),]$n/sum(deltas[,c("delta[15]")])




Dictionary = data.frame(
  variable = c(paste("beta[", 1:23, "]", sep = "")),
  Friendly = c(
    "Population area",
    "Change in extreme heat",
    "Change in human density",
    "Primary habitat loss",
    "Body mass",
    "Governance",
    "Protected area coverage",
    "Change in drought",
    "Change in natural land",
    "Human development",
    "Change in human development",
    "War present",
    "Reproductive output",
    "Maximum longevity",
    "Ecological niche breadth",
    "Climatic niche breadth",
    "Change in extreme heat * Change in drought",
    "Change in extreme heat * Climatic niche breadth",
    "Change in extreme heat * Body mass",
    "Change in extreme heat * Protected area coverage",
    "Primary habitat loss * Change in natural land",
    "Primary habitat loss * Change in human density",
    "Primary habitat loss * Ecological niche breadth"
  ),
  Category = c(
    rep("Traits",1),
    rep("Climate", 1),
    rep("Land-use", 2),
    rep("Traits", 1),
    rep("Governance", 2),
    rep("Climate", 1),
    rep("Land-use", 1),
    rep("Governance", 3),
    rep("Traits", 4),
    rep("Climate", 4),
    rep("Land-use", 3)
  ),
  Type = c(
    rep("Core", 7),
    rep("Optional", 16)
  ),
  Transformation = c(
    "scale log10",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale",
    "cat",
    "scale",
    "scale",
    "scale",
    "scale",
    "scale inter scale",
    "scale inter scale",
    "scale inter scale",
    "scale inter scale",
    "scale inter scale",
    "scale inter scale",
    "scale inter scale"
  ),
  JagsTerm = c(
    "ar",
    "exh",
    "pdc",
    "prc",
    "bm",
    "gov_e",
    "pr",
    "drc",
    "ntc",
    "hdi_e",
    "hdc",
    "conf",
    "re_e",
    "ml",
    "gn_e",
    "gc",
    "exh inter drc",
    "exh inter gc",
    "exh inter bm",
    "exh inter pr",
    "prc inter ntc",
    "prc inter pdc",
    "prc inter gn_e"
  ),
  TrendsTrimName = c(
    "Area",
    "ExHeatC",
    "PDC",
    "PriC",
    "BodyMass",
    "Gov",
    "ProArea",
    "DroughtC",
    "NatC",
    "HDI",
    "HDI_c",
    "Conf",
    "Reprod",
    "MaxLon",
    "Gen",
    "Gen2",
    NA,
    NA,
    NA,
    NA,
    NA,
    NA,
    NA
  )
)

Dictionary$Category = factor(Dictionary$Category,
                             levels = c(
                               "Land-use",
                               "Traits",
                               "Climate",
                               "Governance"
                               
                             ))
Dictionary$Type = factor(Dictionary$Type,
                         levels = c(
                           "Core",
                           "Optional"
                         ))
betas.sum = left_join(betas.sum, Dictionary)
betas.sum$Sig = NA
betas.sum$Sig = ifelse(is.na(betas.sum$Sig) &
                         ((betas.sum$lci_99 >= 0 & betas.sum$uci_99 >= 0) |
                            (betas.sum$lci_99 <= 0 & betas.sum$uci_99 <= 0)),
                       "99% CI",
                       betas.sum$Sig)
betas.sum$Sig = ifelse(is.na(betas.sum$Sig) &
                         ((betas.sum$lci_95 >= 0 & betas.sum$uci_95 >= 0) |
                            (betas.sum$lci_95 <= 0 & betas.sum$uci_95 <= 0)),
                       "95% CI",
                       betas.sum$Sig)
betas.sum$Sig = ifelse(is.na(betas.sum$Sig) &
                         ((betas.sum$lci_90 >= 0 & betas.sum$uci_90 >= 0) |
                            (betas.sum$lci_90 <= 0 & betas.sum$uci_90 <= 0)),
                       "90% CI",
                       betas.sum$Sig)
betas.sum$Sig = ifelse(is.na(betas.sum$Sig), 
                       "High overlap",
                       betas.sum$Sig)
betas.sum$Sig = as.factor(betas.sum$Sig)
betas.sum$Sig = factor(betas.sum$Sig,
                       levels = c(
                         "99% CI",
                         "95% CI",
                         "90% CI",
                         "High overlap"
                       ))

betas.sum$Friendly = factor(betas.sum$Friendly, levels = rev(betas.sum$Friendly[order(
  betas.sum$Category,
  betas.sum$Type,
  1 - betas.sum$rvi
)]))

g = ggplot(betas.sum) +
  geom_col(aes(x = Friendly, y = rvi, fill = Type), alpha = 0.6) +
  #geom_hline(aes(yintercept = 0.15), linetype = "dotted", size = 1) +
  coord_flip() +
  labs(x = " ", y = "Proportion of iterations where parameter is present") +
  scale_fill_manual(values = c("#4b2991", "#CA3C97", "light grey")) +
  facet_grid(Category~., scales = "free_y") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank()
  ) 
g

gt = ggplot_gtable(ggplot_build(g))
grid.draw(gt)
gt$heights[7] = gt$heights[7]*(6/1)
gt$heights[9] = gt$heights[9]*(6/1)
gt$heights[11] = gt$heights[11]*(6/1)
gt$heights[13] = gt$heights[13]*(5/1)
grid.draw(gt)
rvi = gt


betas.sum$string99 = ifelse(betas.sum$Sig == "High overlap",
                            "#c7c7c7",
                            "#872CA2")
betas.sum$string95 = ifelse(betas.sum$Sig == "High overlap",
                            "#bab6b6",
                            "#782B9D")
betas.sum$string90 = ifelse(betas.sum$Sig == "High overlap",
                            "#a8b1b5",
                            "#692A99")
betas.sum$string80 = ifelse(betas.sum$Sig == "High overlap",
                            "#98a2a6",
                            "#5A2995")
betas.sum$string50 = ifelse(betas.sum$Sig == "High overlap",
                            "#8a979c",
                            "#4B2991")


g = ggplot(data = betas.sum) +
  geom_linerange(aes(x = Friendly, ymin = lci_95, ymax = uci_90), size = 3, colour = betas.sum$string95, alpha = 0.3) +
  geom_linerange(aes(x = Friendly, ymin = lci_90, ymax = lci_80), size = 3, colour = betas.sum$string90, alpha = 0.4) +
  geom_linerange(aes(x = Friendly, ymin = lci_80, ymax = lci_50), size = 3, colour = betas.sum$string80, alpha = 0.55) +
  geom_linerange(aes(x = Friendly, ymin = lci_50, ymax = uci_50), size = 3, colour = betas.sum$string50, alpha = 0.7) +
  geom_linerange(aes(x = Friendly, ymin = uci_50, ymax = uci_80), size = 3, colour = betas.sum$string80, alpha = 0.55) +
  geom_linerange(aes(x = Friendly, ymin = uci_80, ymax = uci_90), size = 3, colour = betas.sum$string90, alpha = 0.4) +
  geom_linerange(aes(x = Friendly, ymin = uci_90, ymax = uci_95), size = 3, colour = betas.sum$string95, alpha = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  coord_flip() +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-0.8,0.8)) +
  facet_grid(Category~., scales = "free_y") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.text.y = element_blank(),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(y = "Annual rate of change (%)", x = " ")
g


gt = ggplot_gtable(ggplot_build(g))
grid.draw(gt)
gt$heights[7] = gt$heights[7]*(6/1)
gt$heights[9] = gt$heights[9]*(6/1)
gt$heights[11] = gt$heights[11]*(6/1)
gt$heights[13] = gt$heights[13]*(5/1)
grid.draw(gt)
full_coefs = gt

ggarrange(rvi, full_coefs, labels = c("a", "b"), widths = c(1,0.7))
ggsave("rvi_coefs.png", width = 14, height = 7.5)



betas.sum$Friendly = factor(betas.sum$Friendly, levels = rev(c(
  "Primary habitat loss",
  "Change in human density",
  "Change in natural land",
  "Primary habitat loss * Change in human density",
  "Primary habitat loss * Change in natural land",
  "Primary habitat loss * Ecological niche breadth",
  "Population area",
  "Body mass",
  "Reproductive output",
  "Maximum longevity",
  "Climatic niche breadth",
  "Ecological niche breadth",
  "Change in extreme heat * Body mass",
  "Change in extreme heat * Change in drought",
  "Change in extreme heat * Protected area coverage",
  "Change in extreme heat",
  "Change in extreme heat * Climatic niche breadth",
  "Change in drought",
  "Protected area coverage",
  "Change in human development",
  "Human development",
  "Governance",
  "War present"
)))


g = ggplot(data = betas.sum) +
  geom_linerange(aes(x = Friendly, ymin = lci_95, ymax = uci_80), size = 3, colour = betas.sum$string95, alpha = 0.3) +
  geom_linerange(aes(x = Friendly, ymin = lci_80, ymax = lci_50), size = 3, colour = betas.sum$string80, alpha = 0.55) +
  geom_linerange(aes(x = Friendly, ymin = lci_50, ymax = uci_50), size = 3, colour = betas.sum$string50, alpha = 0.7) +
  geom_linerange(aes(x = Friendly, ymin = uci_50, ymax = uci_80), size = 3, colour = betas.sum$string80, alpha = 0.55) +
  geom_linerange(aes(x = Friendly, ymin = uci_80, ymax = uci_95), size = 3, colour = betas.sum$string95, alpha = 0.3) +
  geom_hline(aes(yintercept = 0)) +
  coord_flip() +
  scale_y_continuous(breaks = c(-1, -0.5, 0, 0.5, 1), limits = c(-0.8,0.8)) +
  facet_grid(Category~., scales = "free_y") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  ) +
  labs(y = "Annual rate of change (%)", x = " ")


gt = ggplot_gtable(ggplot_build(g))
grid.draw(gt)
gt$heights[7] = gt$heights[7]*(6/1)
gt$heights[9] = gt$heights[9]*(6/1)
gt$heights[11] = gt$heights[11]*(6/1)
gt$heights[13] = gt$heights[13]*(5/1)
coefs = gt


alpha_raw  = merged.chains[ , grep("alpha", colnames(merged.chains), fixed = T)]
alpha_raw  = alpha_raw [,-c(grep("sd", colnames(alpha_raw )))]


alpha = alpha_raw
var.beta = merged.chains[ , grep(Dictionary[which(Dictionary$JagsTerm == "prc"),]$variable, colnames(merged.chains), fixed = T)]
var = seq(-10, 10, length.out = 100)
comb.resp = NULL
for(b in 1:length(var)){
  temp.resp = data.frame(resp = alpha + var[b]*var.beta, scaled = var[b])
  comb.resp = rbind(comb.resp, temp.resp)
}

sum.ribbons = comb.resp %>%
  group_by(scaled) %>%
  dplyr::summarise(lci95 = mp(quantile(resp, probs = c(0.025), na.rm = T)),
                   lci80 = mp(quantile(resp, probs = c(0.1), na.rm = T)),
                   lci50 = mp(quantile(resp, probs = c(0.25), na.rm = T)),
                   median = mp(median(resp, na.rm = T)),
                   uci50 = mp(quantile(resp, probs = c(0.75), na.rm = T)),
                   uci80 = mp(quantile(resp, probs = c(0.9), na.rm = T)),
                   uci95 = mp(quantile(resp, probs = c(0.975), na.rm = T))
  )

trans.rib = ((sum.ribbons$scaled*sd(abs(TrendsTrim_lag10$PriC), na.rm = T)) + mean(abs(TrendsTrim_lag10$PriC), na.rm = T))
sum.ribbons$trans = as.vector(trans.rib)


tmp.r = sum.ribbons
prc_plt = ggplot() +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci95, ymax = lci80), fill = "#782B9D", alpha = 0.3) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci80, ymax = lci50), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci50, ymax = uci50), fill = "#4B2991", alpha = 0.7) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci50, ymax = uci80), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci80, ymax = uci95), fill = "#782B9D", alpha = 0.3) +
  geom_line(data = tmp.r, aes(x = trans, y = median), linetype = "dashed", size = 1.2) +
  labs(x = "Primary habitat loss (%)",
       y = "Annual rate\nof change (%)") +
  scale_x_continuous(breaks = c(0,5,10,15), expand=c(0,0)) +
  coord_cartesian(ylim = c(-2, 2), xlim = c(0,16)) +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
prc_plt


alpha = alpha_raw
var.beta = merged.chains[ , grep(Dictionary[which(Dictionary$JagsTerm == "ar"),]$variable, colnames(merged.chains), fixed = T)]
var = seq(-5, 5, length.out = 100)
comb.resp = NULL
for(b in 1:length(var)){
  temp.resp = data.frame(resp = alpha + var[b]*var.beta, scaled = var[b])
  comb.resp = rbind(comb.resp, temp.resp)
}

sum.ribbons = comb.resp %>%
  group_by(scaled) %>%
  dplyr::summarise(lci95 = mp(quantile(resp, probs = c(0.025), na.rm = T)),
                   lci80 = mp(quantile(resp, probs = c(0.1), na.rm = T)),
                   lci50 = mp(quantile(resp, probs = c(0.25), na.rm = T)),
                   median = mp(median(resp, na.rm = T)),
                   uci50 = mp(quantile(resp, probs = c(0.75), na.rm = T)),
                   uci80 = mp(quantile(resp, probs = c(0.9), na.rm = T)),
                   uci95 = mp(quantile(resp, probs = c(0.975), na.rm = T))
  )

sum.ribbons = subset(sum.ribbons, scaled > min(jagsdata_lag10[["ar"]], na.rm = T))
trans.rib = 10^((sum.ribbons$scaled*sd(log10(TrendsTrim_lag10$Area), na.rm = T)) + mean(log10(TrendsTrim_lag10$Area), na.rm = T))
sum.ribbons$trans = as.vector(trans.rib)

tmp.r = sum.ribbons
ar_plt = ggplot() +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci95, ymax = lci80), fill = "#782B9D", alpha = 0.3) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci80, ymax = lci50), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci50, ymax = uci50), fill = "#4B2991", alpha = 0.7) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci50, ymax = uci80), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci80, ymax = uci95), fill = "#782B9D", alpha = 0.3) +
  geom_line(data = tmp.r, aes(x = trans, y = median), linetype = "dashed", size = 1.2) +
  labs(x = "Population area (km2)",
       y = " ") +
  scale_x_log10(breaks = c(100, 10000, 1000000), labels = c("100", "10,000", "1,000,000"), limits = c(20,20000000), expand=c(0,0)) +
  coord_cartesian(ylim = c(-1, 3)) +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
ar_plt


alpha = alpha_raw
var.beta = merged.chains[ , grep(Dictionary[which(Dictionary$JagsTerm == "exh inter bm"),]$variable, colnames(merged.chains), fixed = T)]
var.delta = merged.chains[ , grep(gsub("beta", "delta", Dictionary[which(Dictionary$JagsTerm == "exh inter bm"),]$variable), colnames(merged.chains), fixed = T)]
alpha = alpha_raw[which(var.delta == 1)]
var.beta = var.beta[which(var.delta == 1)]
var1 = rep(seq(-5, 5, length.out = 100),2)
var2 = c(rep(0.81,100), rep(-1.46,100))
comb.resp = NULL
for(b in 1:length(var1)){
  temp.resp = data.frame(resp = alpha + var1[b]*var2[b]*var.beta, scaled = var1[b], group = var2[b])
  comb.resp = rbind(comb.resp, temp.resp)
}


sum.ribbons = comb.resp %>%
  group_by(scaled, group) %>%
  dplyr::summarise(lci95 = mp(quantile(resp, probs = c(0.025), na.rm = T)),
                   lci80 = mp(quantile(resp, probs = c(0.1), na.rm = T)),
                   lci50 = mp(quantile(resp, probs = c(0.25), na.rm = T)),
                   median = mp(median(resp, na.rm = T)),
                   uci50 = mp(quantile(resp, probs = c(0.75), na.rm = T)),
                   uci80 = mp(quantile(resp, probs = c(0.9), na.rm = T)),
                   uci95 = mp(quantile(resp, probs = c(0.975), na.rm = T))
  )


sum.ribbons = subset(sum.ribbons, scaled > min(jagsdata_lag10[["exh"]], na.rm = T))
trans.rib = sum.ribbons$scaled*sd(TrendsTrim_lag10$ExHeat, na.rm = T) + mean(TrendsTrim_lag10$ExHeat, na.rm = T)
sum.ribbons$trans = as.vector(trans.rib)

col_key = data.frame(
  x = c(0,0),
  y = c(0,0),
  group = c("5kg", "100kg")
)
col_key$group = factor(col_key$group, levels = c("5kg", "100kg"))

tmp.r = sum.ribbons
ec_bm_plt = ggplot() +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 0.81),], aes(x = trans, ymin = lci95, ymax = lci80), fill = "#782B9D", alpha = 0.3) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 0.81),], aes(x = trans, ymin = lci80, ymax = lci50), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 0.81),], aes(x = trans, ymin = lci50, ymax = uci50), fill = "#4B2991", alpha = 0.7) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 0.81),], aes(x = trans, ymin = uci50, ymax = uci80), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 0.81),], aes(x = trans, ymin = uci80, ymax = uci95), fill = "#782B9D", alpha = 0.3) +
  geom_line(data = tmp.r[which(tmp.r$group == 0.81),], aes(x = trans, y = median), linetype = "dashed", size = 1.2) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -1.46),], aes(x = trans, ymin = lci95, ymax = lci80), fill = "#ffaa3b", alpha = 0.4) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -1.46),], aes(x = trans, ymin = lci80, ymax = lci50), fill = "#ff9912", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -1.46),], aes(x = trans, ymin = lci50, ymax = uci50), fill = "#e38100", alpha = 0.7) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -1.46),], aes(x = trans, ymin = uci50, ymax = uci80), fill = "#ff9912", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -1.46),], aes(x = trans, ymin = uci80, ymax = uci95), fill = "#ffaa3b", alpha = 0.4) +
  geom_line(data = tmp.r[which(tmp.r$group == -1.46),], aes(x = trans, y = median), linetype = "dotted", size = 1.2) +
  geom_line(data = col_key, aes(x = x, y = y, colour = group), size = 2.5) +
  scale_colour_manual(values = c("#e38100", "#4B2991"), name = "Body mass:") +
  labs(x = "Change in extreme heat",
       y = "Annual rate\nof change (%)") +
  scale_x_continuous(breaks = c(0,1.5,3), expand=c(0,0)) +
  coord_cartesian(ylim = c(-1, 3), xlim = c(-0.1, 3.1)) +
  theme_classic() +
  theme(legend.position = c(0.25,0.85),
        legend.background = element_blank(),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8), face = "italic"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
ec_bm_plt


alpha = alpha_raw
var.beta = merged.chains[ , grep(Dictionary[which(Dictionary$JagsTerm == "exh inter pr"),]$variable, colnames(merged.chains), fixed = T)]
var.delta = merged.chains[ , grep(gsub("beta", "delta", Dictionary[which(Dictionary$JagsTerm == "exh inter pr"),]$variable), colnames(merged.chains), fixed = T)]
alpha = alpha_raw[which(var.delta == 1)]
var.beta = var.beta[which(var.delta == 1)]
var1 = rep(seq(-5, 5, length.out = 100),2)
var2 = c(rep(1.75,100), rep(-0.71,100))
comb.resp = NULL
for(b in 1:length(var1)){
  temp.resp = data.frame(resp = alpha + var1[b]*var2[b]*var.beta, scaled = var1[b], group = var2[b])
  comb.resp = rbind(comb.resp, temp.resp)
}


sum.ribbons = comb.resp %>%
  group_by(scaled, group) %>%
  dplyr::summarise(lci95 = mp(quantile(resp, probs = c(0.025), na.rm = T)),
                   lci80 = mp(quantile(resp, probs = c(0.1), na.rm = T)),
                   lci50 = mp(quantile(resp, probs = c(0.25), na.rm = T)),
                   median = mp(median(resp, na.rm = T)),
                   uci50 = mp(quantile(resp, probs = c(0.75), na.rm = T)),
                   uci80 = mp(quantile(resp, probs = c(0.9), na.rm = T)),
                   uci95 = mp(quantile(resp, probs = c(0.975), na.rm = T))
  )


sum.ribbons = subset(sum.ribbons, scaled > min(jagsdata_lag10[["exh"]], na.rm = T))
trans.rib = sum.ribbons$scaled*sd(TrendsTrim_lag10$ExHeat, na.rm = T) + mean(TrendsTrim_lag10$ExHeat, na.rm = T)
sum.ribbons$trans = as.vector(trans.rib)

col_key = data.frame(
  x = c(0,0),
  y = c(0,0),
  group = c("0%", "50%")
)
col_key$group = factor(col_key$group, levels = c("0%", "50%"))

tmp.r = sum.ribbons
ec_pr_plt = ggplot() +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 1.75),], aes(x = trans, ymin = lci95, ymax = lci80), fill = "#782B9D", alpha = 0.3) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 1.75),], aes(x = trans, ymin = lci80, ymax = lci50), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 1.75),], aes(x = trans, ymin = lci50, ymax = uci50), fill = "#4B2991", alpha = 0.7) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 1.75),], aes(x = trans, ymin = uci50, ymax = uci80), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == 1.75),], aes(x = trans, ymin = uci80, ymax = uci95), fill = "#782B9D", alpha = 0.3) +
  geom_line(data = tmp.r[which(tmp.r$group == 1.75),], aes(x = trans, y = median), linetype = "dashed", size = 1.2) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -0.71),], aes(x = trans, ymin = lci95, ymax = lci80), fill = "#ffaa3b", alpha = 0.4) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -0.71),], aes(x = trans, ymin = lci80, ymax = lci50), fill = "#ff9912", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -0.71),], aes(x = trans, ymin = lci50, ymax = uci50), fill = "#e38100", alpha = 0.7) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -0.71),], aes(x = trans, ymin = uci50, ymax = uci80), fill = "#ff9912", alpha = 0.5) +
  geom_ribbon(data = tmp.r[which(tmp.r$group == -0.71),], aes(x = trans, ymin = uci80, ymax = uci95), fill = "#ffaa3b", alpha = 0.4) +
  geom_line(data = tmp.r[which(tmp.r$group == -0.71),], aes(x = trans, y = median), linetype = "dotted", size = 1.2) +
  geom_line(data = col_key, aes(x = x, y = y, colour = group), size = 2.5) +
  scale_colour_manual(values = c("#e38100", "#4B2991"), name = "Protected area coverage:") +
  labs(x = "Change in extreme heat",
       y = " ") +
  scale_x_continuous(breaks = c(0,1.5,3), expand=c(0,0)) +
  coord_cartesian(ylim = c(-1, 5), xlim = c(-0.1, 3)) +
  theme_classic() +
  theme(legend.position = c(0.4,0.85),
        legend.background = element_blank(),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8), face = "italic"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
ec_pr_plt



alpha = alpha_raw
var.beta = merged.chains[ , grep(Dictionary[which(Dictionary$JagsTerm == "hdc"),]$variable, colnames(merged.chains), fixed = T)]
var.delta = merged.chains[ , grep(gsub("beta", "delta", Dictionary[which(Dictionary$JagsTerm == "hdc"),]$variable), colnames(merged.chains), fixed = T)]
alpha = alpha_raw[which(var.delta == 1)]
var.beta = var.beta[which(var.delta == 1)]
var = seq(-10, 10, length.out = 100)
comb.resp = NULL
for(b in 1:length(var)){
  temp.resp = data.frame(resp = alpha + var[b]*var.beta, scaled = var[b])
  
  comb.resp = rbind(comb.resp, temp.resp)
}

sum.ribbons = comb.resp %>%
  group_by(scaled) %>%
  dplyr::summarise(lci95 = mp(quantile(resp, probs = c(0.025), na.rm = T)),
                   lci80 = mp(quantile(resp, probs = c(0.1), na.rm = T)),
                   lci50 = mp(quantile(resp, probs = c(0.25), na.rm = T)),
                   median = mp(median(resp, na.rm = T)),
                   uci50 = mp(quantile(resp, probs = c(0.75), na.rm = T)),
                   uci80 = mp(quantile(resp, probs = c(0.9), na.rm = T)),
                   uci95 = mp(quantile(resp, probs = c(0.975), na.rm = T))
  )



trans.rib = ((sum.ribbons$scaled*sd((TrendsTrim_lag10$HDI_c), na.rm = T)) + mean((TrendsTrim_lag10$HDI_c), na.rm = T))
sum.ribbons$trans = as.vector(trans.rib)

tmp.r = sum.ribbons
hdc_plt = ggplot() +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci95, ymax = lci80), fill = "#782B9D", alpha = 0.3) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci80, ymax = lci50), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci50, ymax = uci50), fill = "#4B2991", alpha = 0.7) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci50, ymax = uci80), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci80, ymax = uci95), fill = "#782B9D", alpha = 0.3) +
  geom_line(data = tmp.r, aes(x = trans, y = median), linetype = "dashed", size = 1.2) +
  labs(x = "Change in Human development (%)",
       y = "Annual rate\nof change (%)") +
  scale_x_continuous(breaks = c(0,1,2), expand=c(0,0)) +
  coord_cartesian(ylim = c(-1,3), xlim = c(0,2)) +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
hdc_plt

alpha = alpha_raw
var.beta = merged.chains[ , grep(Dictionary[which(Dictionary$JagsTerm == "hdi_e"),]$variable, colnames(merged.chains), fixed = T)]
var.delta = merged.chains[ , grep(gsub("beta", "delta", Dictionary[which(Dictionary$JagsTerm == "hdi_e"),]$variable), colnames(merged.chains), fixed = T)]
alpha = alpha_raw[which(var.delta == 1)]
var.beta = var.beta[which(var.delta == 1)]
var = seq(-10, 10, length.out = 100)
comb.resp = NULL
for(b in 1:length(var)){
  temp.resp = data.frame(resp = alpha + var[b]*var.beta, scaled = var[b])
  
  comb.resp = rbind(comb.resp, temp.resp)
}

sum.ribbons = comb.resp %>%
  group_by(scaled) %>%
  dplyr::summarise(lci95 = mp(quantile(resp, probs = c(0.025), na.rm = T)),
                   lci80 = mp(quantile(resp, probs = c(0.1), na.rm = T)),
                   lci50 = mp(quantile(resp, probs = c(0.25), na.rm = T)),
                   median = mp(median(resp, na.rm = T)),
                   uci50 = mp(quantile(resp, probs = c(0.75), na.rm = T)),
                   uci80 = mp(quantile(resp, probs = c(0.9), na.rm = T)),
                   uci95 = mp(quantile(resp, probs = c(0.975), na.rm = T))
  )



trans.rib = ((sum.ribbons$scaled*sd((TrendsTrim_lag10$HDI), na.rm = T)) + mean((TrendsTrim_lag10$HDI), na.rm = T))
sum.ribbons$trans = as.vector(trans.rib)

tmp.r = sum.ribbons
hdi_plt = ggplot() +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci95, ymax = lci80), fill = "#782B9D", alpha = 0.3) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci80, ymax = lci50), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = lci50, ymax = uci50), fill = "#4B2991", alpha = 0.7) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci50, ymax = uci80), fill = "#5A2995", alpha = 0.5) +
  geom_ribbon(data = tmp.r, aes(x = trans, ymin = uci80, ymax = uci95), fill = "#782B9D", alpha = 0.3) +
  geom_line(data = tmp.r, aes(x = trans, y = median), linetype = "dashed", size = 1.2) +
  labs(x = " Human development index",
       y = " ") +
  scale_x_continuous(breaks = c(0,0.5,1), expand=c(0,0)) +
  coord_cartesian(ylim = c(-3, 3), xlim = c(0,1.05)) +
  theme_classic() +
  theme(axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
hdi_plt







jpeg("coef_plot.jpeg", width = 12.5, height = 9, units = "in", res = 300)

  ggarrange(
    coefs,
    NA,
  ggarrange(
    prc_plt,
    ar_plt,
    ec_bm_plt,
    ec_pr_plt,
    hdc_plt,
    hdi_plt,
    labels = paste0(letters[seq(from = 2, to = 7)],","), 
    ncol = 2, 
    nrow = 3), 
  ncol = 3,
  labels = c("a,", " ", " "),
  widths = c(0.7,0.01,0.7),
  align = "h")
dev.off()

