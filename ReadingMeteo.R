##################### Retrieving Meteo data #####################
#set workdirectory
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Reading_data_vFinal") #alter to own working directory

#English locale
Sys.setlocale("LC_TIME", "English")

# Packages used for all projects
library(ggplot2)
library(plotly)
library(scales)
library(lubridate)
library(cowplot)
library(dplyr)
library(data.table)
library(viridis)      # colour blind friendly palette, works in B&W also
library(leaflet)
library(SciViews)
library(hrbrthemes)
library(dygraphs)
library(tidyverse)
library(writexl)
library(xts)          # To make the convertion data-frame / xts format

#Read station locations
station_locations <- read.csv("Locations_Stations.csv", sep = ";")

##################### Reading all data additional stations #####################
Prec_295 = read.csv("Trishuli_Stations_additional/Precipitation/Prec_295.csv")
Prec_295$rainfallsum <- as.numeric(levels(Prec_295$rainfallsum))[Prec_295$rainfallsum]
Prec_346 = read.csv("Trishuli_Stations_additional/Precipitation/Prec_346.csv")
Prec_346$rainfallsum <- as.numeric(levels(Prec_346$rainfallsum))[Prec_346$rainfallsum]
Prec_347 = read.csv("Trishuli_Stations_additional/Precipitation/Prec_347.csv")
Prec_347$rainfallsum <- as.numeric(levels(Prec_347$rainfallsum))[Prec_347$rainfallsum]
Prec_349 = read.csv("Trishuli_Stations_additional/Precipitation/Prec_349.csv")
Prec_349$rainfallsum <- as.numeric(levels(Prec_349$rainfallsum))[Prec_349$rainfallsum]
Prec_363 = read.csv("Trishuli_Stations_additional/Precipitation/Prec_363.csv")
Prec_363$rainfallsum <- as.numeric(levels(Prec_363$rainfallsum))[Prec_363$rainfallsum]

Tmax_295 = read.csv("Trishuli_Stations_additional/Tmax/Tmax_295.csv")
Tmax_347 = read.csv("Trishuli_Stations_additional/Tmax/Tmax_347.csv")
Tmax_349 = read.csv("Trishuli_Stations_additional/Tmax/Tmax_349.csv")
Tmax_363 = read.csv("Trishuli_Stations_additional/Tmax/Tmax_363.csv")

Tmin_295 = read.csv("Trishuli_Stations_additional/Tmin/Tmin_295.csv")
Tmin_347 = read.csv("Trishuli_Stations_additional/Tmin/Tmin_347.csv")
Tmin_349 = read.csv("Trishuli_Stations_additional/Tmin/Tmin_349.csv")
Tmin_363 = read.csv("Trishuli_Stations_additional/Tmin/Tmin_363.csv")

#rename and combine tables
mintemp <- Tmin_295$mintemp
T_295 = cbind(Tmax_295, mintemp)
mintemp <- Tmin_347$mintemp
T_347 = cbind(Tmax_347, mintemp)
mintemp <- Tmin_349$mintemp
T_349 = cbind(Tmax_349, mintemp)
mintemp <- Tmin_363$mintemp
T_363 = cbind(Tmax_363, mintemp)

#generate timestamps
T_295$Timestamps <- with(T_295, paste0(year, "-", month, "-", days))
T_295$Timestamps <- as.POSIXct(T_295$Timestamps)
T_347$Timestamps <- with(T_347, paste0(year, "-", month, "-", days))
T_347$Timestamps <- as.POSIXct(T_347$Timestamps)
T_349$Timestamps <- with(T_349, paste0(year, "-", month, "-", days))
T_349$Timestamps <- as.POSIXct(T_349$Timestamps)
T_363$Timestamps <- with(T_363, paste0(year, "-", month, "-", days))
T_363$Timestamps <- as.POSIXct(T_363$Timestamps)

Prec_295$Timestamps <- with(Prec_295, paste0(year, "-", month, "-", days))
Prec_295$Timestamps <- as.POSIXct(Prec_295$Timestamps)
Prec_346$Timestamps <- with(Prec_346, paste0(year, "-", month, "-", days))
Prec_346$Timestamps <- as.POSIXct(Prec_346$Timestamps)
Prec_347$Timestamps <- with(Prec_347, paste0(year, "-", month, "-", days))
Prec_347$Timestamps <- as.POSIXct(Prec_347$Timestamps)
Prec_349$Timestamps <- with(Prec_349, paste0(year, "-", month, "-", days))
Prec_349$Timestamps <- as.POSIXct(Prec_349$Timestamps)
Prec_363$Timestamps <- with(Prec_363, paste0(year, "-", month, "-", days))
Prec_363$Timestamps <- as.POSIXct(Prec_363$Timestamps)

#identify NAs for temperature dataset
T_295$maxtemp[T_295$maxtemp == 0] = NA
T_295$mintemp[T_295$mintemp == 0] = NA

T_347$maxtemp[T_347$maxtemp == 0] = NA
T_347$mintemp[T_347$mintemp == 0] = NA

T_349$maxtemp[T_349$maxtemp == 0] = NA
T_349$mintemp[T_349$mintemp == 0] = NA

T_363$maxtemp[T_363$maxtemp == 0] = NA
T_363$mintemp[T_363$mintemp == 0] = NA

##################### Reading all data REAMO stations #####################

Meteo_Kmd = read.csv("REAMO_full_timeseries/06-02406_full_timeseries_Kmd3.csv", sep=";")
Meteo_Kmd$Timestamps <- strptime(Meteo_Kmd$Timestamps, "%Y-%m-%d %H:%M:%S")
Meteo_Kmd$Timestamps <- as.POSIXct(Meteo_Kmd$Timestamps)
Meteo_Kmd$WindSpeed <- as.numeric(levels(Meteo_Kmd$WindSpeed))[Meteo_Kmd$WindSpeed]

Meteo_NBw = read.csv("REAMO_full_timeseries/z6-03168_full_timeseries_NBw3.csv", sep=";")
Meteo_NBw$Timestamps <- strptime(Meteo_NBw$Timestamps, "%Y-%m-%d %H:%M:%S")
Meteo_NBw$Timestamps <- as.POSIXct(Meteo_NBw$Timestamps)
Meteo_NBw$WindSpeed <- as.numeric(levels(Meteo_NBw$WindSpeed))[Meteo_NBw$WindSpeed]
Meteo_NBw$SolRad <- as.numeric(levels(Meteo_NBw$SolRad))[Meteo_NBw$SolRad]
Meteo_NBw$AtmosphericPressure <- as.numeric(levels(Meteo_NBw$AtmosphericPressure))[Meteo_NBw$AtmosphericPressure]
Meteo_NBw$Prec <- as.numeric(levels(Meteo_NBw$Prec))[Meteo_NBw$Prec]
Meteo_NBw$Temperature <- as.numeric(levels(Meteo_NBw$Temperature))[Meteo_NBw$Temperature]

Meteo_Brp = read.csv("REAMO_full_timeseries/z6-03157_full_timeseries_Brp3.csv", sep=";")
Meteo_Brp$Timestamps <- strptime(Meteo_Brp$Timestamps, "%Y-%m-%d %H:%M:%S")
Meteo_Brp$Timestamps <- as.POSIXct(Meteo_Brp$Timestamps)
Meteo_Brp$WindSpeed <- as.numeric(levels(Meteo_Brp$WindSpeed))[Meteo_Brp$WindSpeed]
Meteo_Brp$SolRad <- as.numeric(Meteo_Brp$SolRad)

Meteo_Spg = read.csv("REAMO_full_timeseries/z6-03173_full_timeseries_Spg3.csv", sep=";")
Meteo_Spg$Timestamps <- strptime(Meteo_Spg$Timestamps, "%Y-%m-%d %H:%M:%S")
Meteo_Spg$Timestamps <- as.POSIXct(Meteo_Spg$Timestamps)

###############################
# to daily timescale function #
###############################
meteo_daily <- function(table){
  table$Timestamps <- strptime(table$Timestamps, "%Y-%m-%d")
  table$Timestamps <- as.POSIXct(table$Timestamps)
  table <- table %>% group_by(Timestamps) %>% summarize(SolRad = mean(SolRad), 
                                                        Prec = sum(Prec), 
                                                        WindSpeed = mean(WindSpeed), 
                                                        VaporPressure = mean(VaporPressure), 
                                                        AtmosphericPressure = mean(AtmosphericPressure),
                                                        mintemp = min(Temperature),
                                                        maxtemp = max(Temperature),
                                                        Temperature = mean(Temperature))
  table <- table[1:nrow(table)-1,]
  return(table)
}

Meteo_Kmd_d = meteo_daily(Meteo_Kmd)
Meteo_Brp_d = meteo_daily(Meteo_Brp)
Meteo_Spg_d = meteo_daily(Meteo_Spg)
Meteo_NBw_d = meteo_daily(Meteo_NBw)

######################################
#### to hourly timescale function ####
######################################
meteo_hourly <- function(table){
  table$Timestamps <- strptime(table$Timestamps, "%Y-%m-%d %H")
  table$Timestamps <- as.POSIXct(table$Timestamps)
  table <- table %>% group_by(Timestamps) %>% summarize(SolRad = mean(SolRad),
                                                        Prec = sum(Prec),
                                                        WindSpeed = mean(WindSpeed),
                                                        VaporPressure = mean(VaporPressure),
                                                        AtmosphericPressure = mean(AtmosphericPressure),
                                                        mintemp = min(Temperature),
                                                        maxtemp = max(Temperature),
                                                        Temperature = mean(Temperature)) 
}

Meteo_Kmd_h <- meteo_hourly(Meteo_Kmd)
Meteo_Brp_h <- meteo_hourly(Meteo_Brp)
Meteo_Spg_h <- meteo_hourly(Meteo_Spg)