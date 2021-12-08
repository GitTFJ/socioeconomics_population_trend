mod = readRDS("full_mod.rds")
mod.mcmc = as.mcmc(mod)

converge = mod.mcmc
for(a in 1:nchain(converge)){
  trim = converge[[a]]
  trim = trim[,c(grep(
    paste("alpha",
          "beta",
          "sd.alpha",
          "sd.gamma",
          "sd.reg",
          "sd.cou",
          "sd.gen",
          "sd.spec",
          "sd.mod",
          "p.select",
          "p.int"
          , sep = "|"), 
    colnames(trim)))]
  converge[[a]] = trim
}
pdf("convergence.pdf")
plot(converge)
dev.off()
gelman.diag(converge)



merged.chains = do.call(rbind, mod.mcmc)
pred = as.data.frame(merged.chains[,grepl("mod", colnames(merged.chains))])
pred = pred[,-c(grep("sd.mod", colnames(pred)))]
true = as.data.frame(merged.chains[,grepl("pt", colnames(merged.chains))])
true = true[,-c(grep("pt.pred", colnames(true)))]
sim = as.data.frame(merged.chains[,grepl("pt.pred", colnames(merged.chains))])
res = true - pred

df = data.frame(
  p.l = apply(pred, 2, function(x) quantile(x,probs = 0.025)),
  p.m = apply(pred, 2, function(x) quantile(x,probs = 0.5)),
  p.u = apply(pred, 2, function(x) quantile(x,probs = 0.975)),
  t.l = apply(true, 2, function(x) quantile(x,probs = 0.025)),
  t.m = apply(true, 2, function(x) quantile(x,probs = 0.5)),
  t.u = apply(true, 2, function(x) quantile(x,probs = 0.975)),
  s.l = apply(sim, 2, function(x) quantile(x,probs = 0.025)),
  s.m = apply(sim, 2, function(x) quantile(x,probs = 0.5)),
  s.u = apply(sim, 2, function(x) quantile(x,probs = 0.975)),
  r.l = apply(res, 2, function(x) quantile(x,probs = 0.025)),
  r.m = apply(res, 2, function(x) quantile(x,probs = 0.5)),
  r.u = apply(res, 2, function(x) quantile(x,probs = 0.975))
)
df$id = rownames(df)

Values = data.frame(id = paste("mod[", 1:length(jagsdata_lag10$pt), "]", sep = ""))
Values$Code = c(rep("Quantitative", (min(which(TrendsTrim_lag10$QualitativeStable == 1)) - 1)),
                rep("Qualitative: Stable", (max(which(TrendsTrim_lag10$QualitativeStable == 1)) - (min(which(TrendsTrim_lag10$QualitativeStable == 1)) - 1))),
                rep("Qualitative: Decrease", (max(which(TrendsTrim_lag10$QualitativeDecrease == 1)) - (min(which(TrendsTrim_lag10$QualitativeDecrease == 1)) - 1))),
                rep("Qualitative: Increase", (max(which(TrendsTrim_lag10$QualitativeIncrease == 1)) - (min(which(TrendsTrim_lag10$QualitativeIncrease == 1)) - 1))))
Values$wt = TrendsTrim_lag10$abs_weight
df = left_join(df, Values)


a = ggplot(df[which(df$Code == "Quantitative"),]) +
  geom_jitter(aes(x = t.m-p.m, y = p.m), alpha = 0.2, width = 0.1, height = 0.1) +
  theme_classic() +
  labs(x = "Residual annual rate\nof change (%) ihs transformed", y = "Predicted annual rate\nof change (%) ihs transformedd")
a



ac = cbind(data.frame(id = paste("mod[", 1:length(jagsdata_lag5$pt), "]", sep = "")),
           TrendsTrim_lag5[,c("Longitude", "Latitude", "Species")])
ac = left_join(ac, df[,c("id", "t.m", "p.m")])
geo = as.matrix(dist(cbind(ac$Longitude, ac$Latitude)))
geo = 1/geo
diag(geo) = 0
geo[is.infinite(geo)] <- 0
vg = variog(coords = ac[,2:3], data = ac$t.m - ac$p.m)
vg = data.frame(distance = vg$u, vari = vg$v)
b = ggplot() +
  geom_smooth(data = vg, aes(x = distance, y = vari)) +
  geom_point(data = vg, aes(x = distance, y = vari)) +
  scale_y_continuous(limits = c(0,4)) +
  labs(x = "Distance\n (decimal degrees)", y = "Semivariance", title = paste("     Moran's autocorrelation p-value:", round(ape::Moran.I(ac$t.m - ac$p.m, geo)$p.value,2))) +
  theme_classic()
b
PrunedTree = readRDS("PrunedTree.rds")
p.ac = as.data.frame(ac %>%
                       group_by(Species) %>%
                       dplyr::summarise(r = mean(t.m-p.m)))
p.x = as.matrix(p.ac$r)
rownames(p.x) = p.ac$Species

psig = phylosig(tree = PrunedTree, 
                x = p.x,
                method = "lambda",
                test = T)

p.x = data.frame(id = p.ac$Species, res = p.ac$r,  stringsAsFactors = F)
PrunedTree = drop.tip(PrunedTree,PrunedTree$tip.label[-match(p.x$id, PrunedTree$tip.label)])

p = ggtree(PrunedTree)+ 
  theme_tree2()
c = facet_plot(p, 
               panel = "Trend",
               data = p.x, 
               geom=geom_barh, 
               mapping = aes(x = res),
               stat = "identity") +
  labs(x = "                  Millions of years                                         Residual annual rate of change (%) ihs transformed") 
c = facet_labeller(c, c(Tree = "Phylogeny"))
c
jpeg("assumption_plot.jpeg", width = 8, height = 6, units = "in", res = 300)
ggarrange(ggarrange(a,b, ncol = 2, labels = c("a", "b")), c, nrow = 2, labels = c(" ", "c\n\n\n"))
dev.off()

a = ggplot(df[which(df$Code == "Quantitative"),]) +
  geom_point(aes(x = t.m, y = p.m), alpha = 0.2) +
  coord_cartesian(ylim = c(-5,5), xlim = c(-5,5)) +
  theme_classic() +
  labs(x = "Observed annual rate of change (%)\nInverse hyperbolic sine transformed", y = "Predicted annual rate of change (%)\nInverse hyperbolic sine transformed") 
a

df2 = df
df2$Code = factor(df2$Code, levels = c("Qualitative: Decrease", "Qualitative: Stable", "Qualitative: Increase"))
b = ggplot(df2[which(df2$Code != "Quantitative"),]) +
  geom_pointrange(aes(x = t.m, y = p.m, ymin = p.l, ymax = p.u), alpha = 0.3, colour = "grey") +
  geom_hline(aes(yintercept = 0)) +
  coord_cartesian(ylim = c(-5, 5)) +
  
  theme_classic() +
  facet_grid(~Code, scales = "free_x") +
  labs(x = "Quasi-observed annual rate of change (%)\nInverse hyperbolic sine transformed",
       y = " ")
b

bpval <- mean(df[which(df$Code == "Quantitative"),]$s.m > df[which(df$Code == "Quantitative"),]$t.m)

c = ggplot(df[which(df$Code == "Quantitative"),]) +
  geom_density(aes(x = t.m), fill = "grey", alpha = 0.4) +
  geom_density(aes(x = s.m), fill = "blue", alpha = 0.2, linetype = "dashed") +
  theme_classic() +
  labs(x = "Annual rate of change (%)\nInverse hyperbolic sine transformed",
       y = "Density") +
  xlim(-10,10)
c
bpval <- mean(df[which(grepl("Qualitative", df$Code)),]$s.m > df[which(grepl("Qualitative", df$Code)),]$t.m)

d = ggplot(df2[which(df2$Code != "Quantitative"),]) +
  geom_density(aes(x = t.m), fill = "grey", alpha = 0.4) +
  geom_density(aes(x = s.m), fill = "blue", alpha = 0.2, linetype = "dashed") +
  theme_classic() +
  facet_grid(~Code, scales = "free_x") +
  labs(x = "Annual rate of change (%)\nInverse hyperbolic sine transformed",
       y = " ")
d 

jpeg("posterior_check_plot.jpeg", width = 11, height = 8, units = "in", res = 300)
ggarrange(a,b,c,d, nrow = 2, ncol = 2, labels = c("a", "b", "c", "d"), widths = c(1,1.5))
dev.off()

