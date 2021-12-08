burn = 5000
iter = 10000

source("background/full_unw_v1.7.R")
source("background/full_nobs_v1.7.R")
source("background/full_wg_a_v1.7.R")

full_unw = readRDS("full_unw.rds")
full_nobs = readRDS("full_nobs.rds")
full_wg_a = readRDS("full_wg_a.rds")

mod_list = list(
  full_unw, full_nobs, full_wg_a
)

for(a in 1:length(mod_list)){
  #Build chains
  mod.mcmc = as.mcmc(mod_list[[a]])
  converge = mod.mcmc
  
  for(a in 1:nchain(converge)){
    trim = converge[[a]]
    trim = trim[,c(grep(
      paste(  "sd.gamma",
              "sd.alpha",
              "sd.reg",
              "sd.cou",
              "sd.gen",
              "sd.spec",
              "sd.mod",
              "p.select",
              "p.int", sep = "|"), 
      colnames(trim)))]
    converge[[a]] = trim
  }
  print(gelman.diag(converge))
}

beta_df = NULL
df = NULL
for(a in 1:length(mod_list)){
  if(a == 1){
    wt = 1
    mod_name = "Unweighted \n "
  } else if(a == 2){
    wt = jagsdata_lag5$nobs
    mod_name = "Weighted by \n trend sample size"
  } else {
    wt = jagsdata_lag5$wg_a
    mod_name = "Weighted by \n simulated error"
  }
  mod.mcmc = as.mcmc(mod_list[[a]])
  
  merged.chains = do.call(rbind, mod.mcmc)
  deltas = merged.chains[,grep("delta", colnames(merged.chains))]
  model.combos = NULL
  id = gsub("[^0-9A-Za-z///' ]","", colnames(deltas))
  id = as.numeric(gsub("delta","", id))
  for(i in 1:nrow(deltas)){
    model.combos[i] = paste(id[deltas[i,]==1],collapse=",")
  }
  deltas = as.data.frame(deltas)
  
  
  
  betas = cbind(deltas, merged.chains[,grep("beta", colnames(merged.chains))])
  for(b in 8:23){
    delta.position = grep(paste("delta[",b,"]",sep = ""),colnames(betas), fixed =T)
    beta.position = grep(paste("beta[",b,"]",sep = ""),colnames(betas), fixed =T)
    betas[,beta.position] = ifelse(betas[,delta.position] == 0, 
                                   NA,
                                   betas[,beta.position])
  }
  colnames(betas)
  betas = betas[,c(17:39)]
  betas$id = rownames(betas)
  betas.melt = melt(betas, id = "id")
  mp = function(vals){sinh(vals)}
  betas.sum = betas.melt %>%
    group_by(variable) %>%
    dplyr::summarise(
      inc_p = sum(!is.na(value))/nrow(betas),
      coef = median(value, na.rm = T),
      lci50 = quantile(value, probs = c(0.25), na.rm = T),
      lci80 = quantile(value, probs = c(0.1), na.rm = T),
      lci95 = quantile(value, probs = c(0.05), na.rm = T),
      lci975 = quantile(value, probs = c(0.025), na.rm = T),
      uci975 = quantile(value, probs = c(0.975), na.rm = T),
      uci95 = quantile(value, probs = c(0.95), na.rm = T),
      uci80 = quantile(value, probs = c(0.9), na.rm = T),
      uci50 = quantile(value, probs = c(0.75), na.rm = T))
  
  betas.sum$model = mod_name
  beta_df = rbind(beta_df, betas.sum)
  
  pred = as.data.frame(merged.chains[,grepl("mod", colnames(merged.chains))])
  pred = pred[,-c(grep("sd.mod", colnames(pred)))]
  true = as.data.frame(merged.chains[,grepl("pt", colnames(merged.chains))])
  
  pred_df = data.frame(
    p = apply(pred, 2, function(x) quantile(x,probs = 0.5)),
    t = apply(true, 2, function(x) quantile(x,probs = 0.5))
  )
  pred_df$id = rownames(pred_df)
  
  weig_df = data.frame(
    id = paste0("mod[", 1:length(jagsdata_lag5$pt), "]"),
    weig = wt
  )
  pred_df = left_join(pred_df, weig_df)
  
  
  pred_df$p = sinh(pred_df$p)
  pred_df$t = sinh(pred_df$t)
  rmse_un = rmse(pred_df$t, pred_df$p)
  rmse_weig = rmse(pred_df$t, pred_df$p, pred_df$weig)
  
  
  mod = as.data.frame(merged.chains[,grepl("mod", colnames(merged.chains))])
  mod = mod[,-c(grep("sd", colnames(mod)))]
  mod = apply(mod, 2, function(x) quantile(x,probs = 0.5))
  v_fixed = (sd(mod))^2
  v_resid = median(merged.chains[,grepl("sd.mod", colnames(merged.chains))])^2
  v_resid  = median(1/((1/v_resid) * pred_df$weig)) #Precision * weight
  v_random_reg = median(merged.chains[,grepl("sd.reg", colnames(merged.chains))])^2
  v_random_cou = median(merged.chains[,grepl("sd.cou", colnames(merged.chains))])^2
  v_random_gen = median(merged.chains[,grepl("sd.gen", colnames(merged.chains))])^2
  v_random_spec = median(merged.chains[,grepl("sd.spec", colnames(merged.chains))])^2
  r2m = v_fixed/(v_fixed + v_resid + v_random_reg + v_random_cou + v_random_gen + v_random_spec) 
  r2c =(v_fixed + v_random_reg + v_random_cou + v_random_gen + v_random_spec)/(v_fixed + v_resid + v_random_reg + v_random_cou + v_random_gen + v_random_spec)
  
  pred_df = pred_df[order(-pred_df$weig),] 
  wt10 =(sum(pred_df$weig[1:round(nrow(pred_df)*0.1)])/sum(pred_df$weig))*100
  
  
  tmp_df = data.frame(Model = a,
                      r2m = r2m,
                      r2c = r2c,
                      rmse_un = rmse_un,
                      rmse_weig = rmse_weig,
                      wt10 = wt10)
  df = rbind(df, tmp_df)
}

df = round(df,3)
write.csv(df, "weight_fit.csv")

key = data.frame(
  variable = paste0("beta[",1:23,"]"),
  term = c(
    "Population area",
    "Change in extreme heat",
    "Change in population density",
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
    "Change in extreme heat * BodyMass",
    "Change in extreme heat * Protected area coverage",
    "Primary habitat loss * Change in natural land",
    "Primary habitat loss * Change in population density",
    "Primary habitat loss * Ecological niche breadth"
  )
)

beta_df = left_join(beta_df, key)
beta_df$model = factor(beta_df$model, levels = c("Unweighted \n ", "Weighted by \n trend sample size", "Weighted by \n simulated error"))

ggplot(beta_df) +
  geom_linerange(aes(y = term, xmin = lci50, xmax = uci50), size = 4, colour = "grey65") +
  geom_linerange(aes(y = term, xmin = lci80, xmax = uci80), size = 2.5, colour = "grey40") +
  geom_linerange(aes(y = term, xmin = lci95, xmax = uci95), size = 1, colour = "grey30") +
  geom_linerange(aes(y = term, xmin = lci975, xmax = uci975), size = 0.2, colour = "grey20") +
  geom_vline(aes(xintercept = 0), linetype = "dotted") +
  labs(x = "Coefficient", y = " ") +
  theme_classic() +
  scale_x_continuous(breaks = c(-0.3,0,0.3)) +
  coord_cartesian(xlim = c(-0.5,0.5)) +
  facet_grid(.~model, scales = "free")

ggsave("weight_fit.png", width = 12, height = 7)

