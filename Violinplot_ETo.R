##################### Violin plot ETo #####################
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ETomethods.R")

#based on observations

#Kathmandu
#hargreaves
Hargreaves <- meteo_hargreaves(Meteo_Kmd_d)
Hargreaves <- Hargreaves[Hargreaves$Timestamps >= "2020-01-01" & Hargreaves$Timestamps < "2020-12-01",]
Hargreaves$EToMethod = "Hargreaves"

#makkink
Makkink <- meteo_makkink(Meteo_Kmd_d)
Makkink <- Makkink[Makkink$Timestamps >= "2020-01-01" & Makkink$Timestamps < "2020-12-01",]
Makkink$EToMethod = "Makkink"

#PM
PenmanMonteith <- meteo_PM(Meteo_Kmd, "2020-01-01", "2020-12-01", station_locations$Altitude[1])
PenmanMonteith$EToMethod = "Penman Monteith"

#combine tables
Hargreaves <- Hargreaves %>% dplyr::select(Timestamps, ETo, EToMethod)
Makkink <- Makkink %>% dplyr::select(Timestamps, ETo, EToMethod)
PenmanMonteith <- PenmanMonteith %>% dplyr::select(Timestamps, ETo, EToMethod)

table <- rbind(Hargreaves, Makkink, PenmanMonteith)
table$station = "Kathmandu"

#Bharatpur
#hargreaves
Hargreaves <- meteo_hargreaves(Meteo_Brp_d)
Hargreaves <- Hargreaves[Hargreaves$Timestamps >= "2020-02-18" & Hargreaves$Timestamps < "2020-09-01",]
Hargreaves$EToMethod = "Hargreaves"

#makkink
Makkink <- meteo_makkink(Meteo_Brp_d)
Makkink <- Makkink[Makkink$Timestamps >= "2020-02-18" & Makkink$Timestamps < "2020-09-01",]
Makkink$EToMethod = "Makkink"

#PM
PenmanMonteith <- meteo_PM(Meteo_Brp, "2020-02-18", "2020-09-01", station_locations$Altitude[3])
PenmanMonteith$EToMethod = "Penman Monteith"

#combine tables
Hargreaves <- Hargreaves %>% dplyr::select(Timestamps, ETo, EToMethod)
Makkink <- Makkink %>% dplyr::select(Timestamps, ETo, EToMethod)
PenmanMonteith <- PenmanMonteith %>% dplyr::select(Timestamps, ETo, EToMethod)

table2 <- rbind(Hargreaves, Makkink, PenmanMonteith)
table2$station = "Bharatpur"

#combine two stations
table <- rbind(table, table2)

#violin plot 2020
ggplot() +
  geom_violin(data = table, aes(EToMethod, ETo, fill = EToMethod, color = EToMethod)) +
  facet_wrap(~station) +
  scale_fill_manual(values=c("darkseagreen1", "darkseagreen3", "darkseagreen4")) +
  scale_color_manual(values=c("darkseagreen1", "darkseagreen3", "darkseagreen4")) +
  labs(y="Evaporation (mm/day)", x="Method") +
  ggtitle("Evaporation distribution using different methods", "Based on meteorological observations in 2020") +
  theme_ipsum() +
  theme(legend.position="none") +
  coord_flip()

# Calculate statistics: mean, min and max
statistics <- na.omit(table) %>% group_by(station, EToMethod) %>% summarize(meanETo = mean(ETo),
                                                                            minETo = min(ETo),
                                                                            maxETo = max(ETo))
print(statistics)