
regs = as.data.frame(merged.chains[,grepl("obs.site", colnames(merged.chains))])
regs_med = data.frame(reg_intercept = apply(regs, 2, function(x) quantile(x,probs = 0.5)))
regs_med$id = rownames(regs_med)
regs_dic = data.frame(
  regs_c = unique(droplevels(as.factor(TrendsTrim_lag10$Singular_country))),
  regs_n = unique(as.numeric(droplevels(as.factor(TrendsTrim_lag10$Singular_country))))
)
regs_dic$id = paste0("obs.site[",regs_dic$regs_n,"]")
regs_dic = left_join(regs_dic, regs_med)

country = read.csv("data/CountryContinent.csv")
regs_dic = left_join(regs_dic, country, by = c("regs_c" = "name"))
regs_dic$alpha.3[2] = "USA"
regs_dic$alpha.3[3] = "CIV"
regs_dic$alpha.3[29] = "KOR"
regs_dic$alpha.3[32] = "GBR"
regs_dic$alpha.3[71] = "BOL"
regs_dic$alpha.3[73] = "VEN"

saveRDS(regs_dic, "countryIntercept.rds")


world = read_sf("data/TM_WORLD_BORDERS-0.3.shp")
world = left_join(world, regs_dic, by = c("ISO3" = "alpha.3"))
world$reg_intercept = ifelse(is.na(world$reg_intercept), 0, world$reg_intercept)
world_rst = rasterize(world, raster(nrows=180, ncols=360), field = "reg_intercept")
plot(world_rst)
saveRDS(world_rst, "country_rast.rds")

