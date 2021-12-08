

regs = as.data.frame(merged.chains[,grepl("mu.reg", colnames(merged.chains))])
regs_med = data.frame(reg_intercept = apply(regs, 2, function(x) quantile(x,probs = 0.5)))
regs_med$id = rownames(regs_med)
regs_dic = data.frame(
  regs_c = unique(droplevels(as.factor(TrendsTrim_lag10$Conts))),
  regs_n = unique(as.numeric(droplevels(as.factor(TrendsTrim_lag10$Conts))))
)
regs_dic$id = paste0("mu.reg[",regs_dic$regs_n,"]")

regs_dic = left_join(regs_dic, regs_med)
saveRDS(regs_dic, "RegionIntercept.rds")

world = read_sf("data/TM_WORLD_BORDERS-0.3.shp")
world = left_join(world, regs_dic)
world$reg_intercept = ifelse(is.na(world$reg_intercept), 0, world$reg_intercept)
world_rst = rasterize(world, raster(nrows=180, ncols=360), field = "reg_intercept")
saveRDS(world_rst, "region_rast.rds")
