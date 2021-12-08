TraitList = readRDS("Documents/ModellingTrends/Data/Traits/TraitsFormatted.rds")

A = TraitList[[1]]
A$Code = "Best estimate"
B = TraitList[[2]]
B$Code = "Lower"
C = TraitList[[3]]
C$Code = "Upper"
Traits = rbind(A,B,C)

PrunedTree = readRDS("PrunedTree.rds")

Plot = ggtree(PrunedTree)
BM = Traits[,c(1,8,12,18)]
Plot = facet_plot(Plot, 
                  panel = "Body mass", 
                  data=BM, 
                  geom=geom_pointrangeh, 
                  aes(x=BodyMass_log10, 
                      xmin = BodyMass_log10 - sqrt(BodyMass_log10_Var)*1.96,  
                      xmax = BodyMass_log10 + sqrt(BodyMass_log10_Var)*1.96,
                      colour = Code,
                      shape = Code),
                  position = "jitter",
                  alpha = 0.2) +
  theme_tree2() 



ML = Traits[,c(1,9,13,18)]
Plot = facet_plot(Plot, 
                  panel = "Maximum longevity", 
                  data=ML, 
                  geom=geom_pointrangeh, 
                  mapping = aes(x=Longevity_log10, 
                                xmin = Longevity_log10 - sqrt(Longevity_log10_Var)*1.96,  
                                xmax = Longevity_log10 + sqrt(Longevity_log10_Var)*1.96,
                                colour = Code,
                                shape = Code),
                  position = "jitter",
                  alpha = 0.2) +
  theme_tree2() 

GEN = Traits[,c(1,16:18)]
Plot = facet_plot(Plot, 
                  panel = "Ecological niche breadth", 
                  data=GEN, 
                  geom=geom_pointrangeh, 
                  mapping = aes(x=Gen_mean, 
                                xmin = Gen_mean - sqrt(Gen_var)*1.96,  
                                xmax = Gen_mean + sqrt(Gen_var)*1.96,
                                colour = Code,
                                shape = Code),
                  position = "jitter",
                  alpha = 0.25) +
  theme_tree2()


RRO = Traits[,c(1,14,15,18)]
Plot = facet_plot(Plot, 
                  panel = "Reproductive output", 
                  data=RRO, 
                  geom=geom_pointrangeh, 
                  mapping = aes(x=ReprodRate_mean, 
                                xmin = ReprodRate_mean - sqrt(ReprodRate_var)*1.96,  
                                xmax = ReprodRate_mean + sqrt(ReprodRate_var)*1.96,
                                colour = Code,
                                shape = Code),
                  position = "jitter",
                  alpha = 0.25) +
  theme_tree2()  +
  labs(x = "Millions of years                      log10(grams)                   log10(years)                   Component                   Component") 
Plot = facet_labeller(Plot, c(Tree = "Phylogeny"))
Plot
ggsave("Documents/ModellingTrends/Results/Trait_Imps.png", width = 10.5, height = 7)

GGally::ggpairs(Traits[,c(9,8,5,16,14)],
                columnLabels = c("Body mass", "Maximum longevity", "Climatic niche breadth", "Ecological niche breadth", "Reproductive output"), 
                axisLabels="none") + 
  theme_classic() 
ggsave("Documents/ModellingTrends/Results/Trait_cor.png", width = 8, height = 8)
