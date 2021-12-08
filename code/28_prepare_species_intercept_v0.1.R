spec = as.data.frame(merged.chains[,grep("obs.spec", colnames(merged.chains))])
colnames(spec)
spec$id = rownames(spec)
spec.melt = melt(spec, id = "id")
spec.sum = spec.melt %>%
  group_by(variable) %>%
  dplyr::summarise(
    coef = median(value, na.rm = T))
spec.melt = left_join(spec.melt, spec.sum)

Dictionary_spec = data.frame(
  variable = paste("obs.spec[", 1:50, "]", sep = ""),
  Friendly = levels(as.factor(TrendsTrim_lag10$Species))
)

spec_fam = unique(TrendsTrim_lag10[,c("Species", "Genus", "Family")])
Dictionary_spec = left_join(Dictionary_spec, spec_fam, by = c("Friendly" = "Species"))
spec.sum = left_join(spec.sum, Dictionary_spec)
saveRDS(spec.sum, "SpeciesIntercept.rds")



