##################### Retrieving ERA5 data #####################
#set workdirectory
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Reading_data_vFinal") #alter to own working directory

# Meteorological stations and corresponding ERA5 grid cells
#ERA1 & ERA5_highr1 = 295; Spg
#ERA2 = 363; 346
#ERA3 = 349; 347; NBw
#ERA4 & ERA5_highr4 = Kmd
#ERA5 & ERA5_highr5 = Brp

# manually reading txt files
ERA5_1 <- read.delim("ERA5/ERA5mintemp_Spg.txt", sep="")
ERA5_1$Timestamps <- strptime(ERA5_1$Timestamps, "%Y%m%d")
ERA5_1$Timestamps <- as.POSIXct(ERA5_1$Timestamps)
ERA5_2 <- read.delim("ERA5/ERA5mintemp_363_2.txt", sep="")
ERA5_2$Timestamps <- strptime(ERA5_2$Timestamps, "%Y%m%d")
ERA5_2$Timestamps <- as.POSIXct(ERA5_2$Timestamps)
ERA5_3 <- read.delim("ERA5/ERA5mintemp_349_2.txt", sep="")
ERA5_3$Timestamps <- strptime(ERA5_3$Timestamps, "%Y%m%d")
ERA5_3$Timestamps <- as.POSIXct(ERA5_3$Timestamps)
ERA5_4 <- read.delim("ERA5/ERA5mintemp_Kmd.txt", sep="")
ERA5_4$Timestamps <- strptime(ERA5_4$Timestamps, "%Y%m%d")
ERA5_4$Timestamps <- as.POSIXct(ERA5_4$Timestamps)
ERA5_5 <- read.delim("ERA5/ERA5mintemp_Brp.txt", sep="")
ERA5_5$Timestamps <- strptime(ERA5_5$Timestamps, "%Y%m%d")
ERA5_5$Timestamps <- as.POSIXct(ERA5_5$Timestamps)

ERA5_maxtemp1 <- read.delim("ERA5/ERA5maxtemp_Spg.txt", sep="")
ERA5_maxtemp2 <- read.delim("ERA5/ERA5maxtemp_363_2.txt", sep="")
ERA5_maxtemp3 <- read.delim("ERA5/ERA5maxtemp_349_2.txt", sep="")
ERA5_maxtemp4 <- read.delim("ERA5/ERA5maxtemp_Kmd.txt", sep="")
ERA5_maxtemp5 <- read.delim("ERA5/ERA5maxtemp_Brp.txt", sep="")

ERA5_prec1 <- read.delim("ERA5/ERA5prec_Spg.txt", sep="")
ERA5_prec2 <- read.delim("ERA5/ERA5prec_346.txt", sep="")
ERA5_prec3 <- read.delim("ERA5/ERA5prec_347.txt", sep="")
ERA5_prec4 <- read.delim("ERA5/ERA5prec_Kmd.txt", sep="")
ERA5_prec5 <- read.delim("ERA5/ERA5prec_Brp.txt", sep="")

#combine max, min, prec, temperature
maxtemp <- ERA5_maxtemp1$maxtemp
precipitation <- ERA5_prec1$prec
ERA5_1 = cbind(ERA5_1, maxtemp, precipitation)

maxtemp <- ERA5_maxtemp2$maxtemp
precipitation <- ERA5_prec2$prec
ERA5_2 = cbind(ERA5_2, maxtemp, precipitation)

maxtemp <- ERA5_maxtemp3$maxtemp
precipitation <- ERA5_prec3$prec
ERA5_3 = cbind(ERA5_3, maxtemp, precipitation)

maxtemp <- ERA5_maxtemp4$maxtemp
precipitation <- ERA5_prec4$precipitation
ERA5_4 = cbind(ERA5_4, maxtemp, precipitation)

maxtemp <- ERA5_maxtemp5$maxtemp
precipitation <- ERA5_prec5$precipitation
ERA5_5 = cbind(ERA5_5, maxtemp, precipitation)

#manually reading high (hourly) resolution csv INCLUDING RADIATION
ERA5_highr1 <- read.csv("ERA5/ERA5radiation_295.csv", sep=",")
ERA5_highr1$Timestamps <- strptime(ERA5_highr1$Timestamps, "%Y-%m-%d %H:%M")
ERA5_highr1$Timestamps <- as.POSIXct(ERA5_highr1$Timestamps)

ERA5_highr4 <- read.csv("ERA5/ERA5radiation_Kmd.csv", sep=",")
ERA5_highr4$Timestamps <- strptime(ERA5_highr4$Timestamps, "%Y-%m-%d %H:%M")
ERA5_highr4$Timestamps <- as.POSIXct(ERA5_highr4$Timestamps)

ERA5_highr5 <- read.csv("ERA5/ERA5radiation_Brp.csv", sep=",")
ERA5_highr5$Timestamps <- strptime(ERA5_highr5$Timestamps, "%Y-%m-%d %H:%M")
ERA5_highr5$Timestamps <- as.POSIXct(ERA5_highr5$Timestamps)

######################################
### high resolution on daily scale ###
######################################
meteo_era5_daily <- function(table){
  table$Timestamps <- strptime(table$Timestamps, "%Y-%m-%d")
  table$Timestamps <- as.POSIXct(table$Timestamps)
  table <- table %>% group_by(Timestamps) %>% summarize(SolRad = mean(SolRad), 
                                                        mintemp = min(Temperature),
                                                        maxtemp = max(Temperature),
                                                        Temperature = mean(Temperature),
                                                        prec = sum(prec))
  return(table)
}

ERA5_highr1_d <- meteo_era5_daily(ERA5_highr1)
ERA5_highr1_d <- ERA5_highr1_d[ERA5_highr1_d$Timestamps > "1979-01-01" & ERA5_highr1_d$Timestamps < "2020-01-01",]
ERA5_highr4_d <- na.omit(meteo_era5_daily(ERA5_highr4))
ERA5_highr4_d <- ERA5_highr4_d[ERA5_highr4_d$Timestamps < "2021-01-01",]
ERA5_highr5_d <- na.omit(meteo_era5_daily(ERA5_highr5))
ERA5_highr5_d <- ERA5_highr5_d[ERA5_highr5_d$Timestamps < "2021-01-01",]