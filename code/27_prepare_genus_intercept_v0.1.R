spec = as.data.frame(merged.chains[,grep("mu.gen", colnames(merged.chains))])
colnames(spec)
spec$id = rownames(spec)
spec.melt = melt(spec, id = "id")
spec.sum = spec.melt %>%
  group_by(variable) %>%
  dplyr::summarise(
    coef = median(value, na.rm = T))
spec.melt = left_join(spec.melt, spec.sum)

Dictionary_spec = data.frame(
  variable = paste("mu.gen[", 1:25, "]", sep = ""),
  Friendly = levels(as.factor(TrendsTrim_lag10$Genus))
)

spec_fam = unique(TrendsTrim_lag10[,c("Genus", "Family")])
Dictionary_spec = left_join(Dictionary_spec, spec_fam, by = c("Friendly" = "Genus"))
spec.sum = left_join(spec.sum, Dictionary_spec)
saveRDS(spec.sum, "GenusIntercept.rds")
