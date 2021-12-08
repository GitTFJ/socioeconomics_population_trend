CaPTrendsRaw = read.csv("Documents/ModellingTrends/Data/Trends/CaPTrends.csv")
CaPTrendsRaw$UniqueID = paste("cpt", CaPTrendsRaw$DataTableID, sep = "_")
CaPTrendsRaw$Location = paste(CaPTrendsRaw$Locality_name, CaPTrendsRaw$Singular_country)
CaPTrends = subset(CaPTrendsRaw, 
                   select = c(
                     "Latitude",
                     "Longitude",
                     "Location",
                     "UniqueID"
                   ))


LPI = read.csv("Documents/ModellingTrends/Data/Trends/LPICombinedEdited.csv", 
               row.names = NULL)
LPI$UniqueID = paste("lpi", LPI$id, sep = "_")
LPI$Location = paste(LPI$PopulationLocation, LPI$CountryList)
LPI = subset(LPI, 
             is.na(Tag),
             select = c(
               "DecimalLatitude",
               "DecimalLongitude",
               "Location",
               "UniqueID"
             ))
colnames(LPI) = c("Latitude", 
                  "Longitude", 
                  "Location",
                  "UniqueID")

Coords = rbind(CaPTrends[,c("Latitude", 
                            "Longitude", 
                            "Location",
                            "UniqueID")], 
               LPI[,c("Latitude", 
                      "Longitude", 
                      "Location",
                      "UniqueID")])
Coords$ID = 1:nrow(Coords)
rm(CaPTrendsRaw, CaPTrends, LPI)
Area = read.csv("Documents/ModellingTrends/Data/Trends/Area(save).csv")
Coords = left_join(Coords[,c(5,4,3,1,2)], Area[,c(1,5)])
ID = read.csv("Documents/ModellingTrends/Data/Trends/ID(save).csv")
Coords = left_join(Coords, ID)

Area_Points = data.frame(Area = c(1, 10, 100, 1000, 10000, 100000, 1000000),
                         Points = c(10, 20, 30, 50, 80, 120, 200))
PredPoints = lm(Points ~ poly(log10(Area),2), data = Area_Points)
Coords$Points = as.integer(predict(PredPoints, newdata = Coords))

b = ggplot(Coords) +
  geom_line(aes(x = Area, y = Points)) +
  scale_x_log10(breaks = c(1,100,10000,1000000), labels = c("1", "100", "10,000", "1,000,000")) +
  labs(x = expression(Area~(km^2)), y = "Sampling points (n)") +
  theme_classic()

Area2 = Area
Area2$small = ifelse(Area2$Area == 1000, T, F)
sum(Area2$small)
Area2$medium = ifelse(Area2$Area == 10000, T, F)
sum(Area2$medium)
Area2$large = ifelse(Area2$Area == 100000, T, F)
sum(Area2$large)
Area2$Area = ifelse((Area2$Area == 1000 | Area2$Area == 10000 | Area2$Area == 100000), NA, Area2$Area)

a = ggplot() +
  geom_density(data = Area2, aes(x = Area), fill = "grey", alpha = 0.5) +
  scale_x_log10(
    breaks = c(100,10000,1000000),
    labels = c("100","10,000","1,000,000")
  ) +
  scale_y_continuous(expand = c(0,0)) +
  theme_classic() +
  labs(y = "Density", x = bquote("Population area"~km^2))
ggarrange(a,b,ncol = 2, nrow = 1, labels = c("a,", "b,"))
ggsave("Documents/ModellingTrends/Results/SamplingPoins.png", width = 9, height = 5
       )
rm(Area_Points, PredPoints)
