gov_imp_plt = ggplot() +
  geom_line(data = Governance[which((Governance$Code == "ARG" |
                                   Governance$Code == "IND" |
                                   Governance$Code == "TZA" |
                                   Governance$Code == "USA") & 
                                     Governance$Gov_var == 0),],
            aes(x = Year, y = Gov_mean, group = Num), alpha = 0.5) +
  geom_pointrange(data = Governance[which((Governance$Code == "ARG" |
                                                   Governance$Code == "IND" |
                                                   Governance$Code == "TZA" |
                                                   Governance$Code == "USA") &
                                                Governance$Gov_var != 0),],
             aes(
               x = Year, 
               y = Gov_mean, 
               ymin = (Gov_mean - sqrt(Gov_var)*1.96), 
               ymax = (Gov_mean + sqrt(Gov_var)*1.96)), 
             colour = "dark red", 
             alpha = 0.25) +
  scale_x_continuous(breaks = c(1970, 1990, 2010), limits = c(1960,2015)) +
  facet_grid(~Code) +
  theme_classic() +
  theme(text = element_text(size = 14)) +
  labs(y = "Governance Index")


hdi_imp_plt = ggplot() +
  geom_line(data = Governance[which((Governance$Code == "ARG" |
                                      Governance$Code == "IND" |
                                      Governance$Code == "TZA" |
                                      Governance$Code == "USA") & 
                                Governance$HDI_var == 0),],
            aes(x = Year, y = HDI_mean, group = Num), alpha = 0.5) +
  geom_pointrange(data = Governance[which((Governance$Code == "ARG" |
                                             Governance$Code == "IND" |
                                             Governance$Code == "TZA" |
                                             Governance$Code == "USA") &
                                            Governance$HDI_var != 0),],
                  aes(
                    x = Year, 
                    y = HDI_mean, 
                    ymin = (HDI_mean - sqrt(HDI_var)*1.96), 
                    ymax = (HDI_mean + sqrt(HDI_var)*1.96)), 
                  colour = "dark red", 
                  alpha = 0.25) +
  scale_x_continuous(breaks = c(1970, 1990, 2010), limits = c(1960,2015)) +
  facet_grid(~Code) +
  theme_classic() +
  theme(text = element_text(size = 14)) +
  labs(y = "Human development Index")

ggarrange(gov_imp_plt, hdi_imp_plt, nrow = 2)

ggsave("Documents/ModellingTrends/Results/Imps.png", height = 6, width = 8)

