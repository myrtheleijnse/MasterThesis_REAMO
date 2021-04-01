##################### Downscaling ERA5 data QUANTILE MAPPING #####################

# Source Meteo-data Reading_Meteo.R and Reading_ERA5.R
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ReadingMeteo.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ReadingERA5.R")

# Quantile mapping procedure

######################################################### STATION 295 #########################################################
# filter data on calibration period
#calibration period T: 2007-2014
#calibration period P: 2000-2008

ERA5_reftemp <- ERA5_1[ERA5_1$Timestamps >= "2007-01-01" & ERA5_1$Timestamps <= "2014-12-31",]
T_295_reftemp <- T_295[T_295$Timestamps >= "2007-01-01" & T_295$Timestamps <= "2014-12-31",]
ERA5_refprec <- ERA5_1[ERA5_1$Timestamps >= "2000-01-01" & ERA5_1$Timestamps <= "2008-12-31",]
Prec_295_ref <- Prec_295[Prec_295$Timestamps >= "2000-01-01" & Prec_295$Timestamps <= "2008-12-31",]

# construct ecdf functions
ecdf_T_295_mintemp <- ecdf(T_295_reftemp$mintemp)
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_T_295_maxtemp <- ecdf(T_295_reftemp$maxtemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_Prec_295 <- ecdf(Prec_295_ref$rainfallsum)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$precipitation)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_1$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_1$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_1$precipitation)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(T_295_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(T_295_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Prec_295_ref$rainfallsum, prec_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_1_downscaled <- cbind(ERA5_1, mintemp_downscaled, maxtemp_downscaled, prec_downscaled)

#rename table variables
mintemp <- ERA5_1_downscaled$mintemp_downscaled
maxtemp <- ERA5_1_downscaled$maxtemp_downscaled
prec <- ERA5_1_downscaled$prec_downscaled
dwns_295 <- ERA5_1_downscaled %>% dplyr::select(Timestamps)
dwns_295 <- cbind(dwns_295, mintemp, maxtemp, prec)

######################################################### STATION 346 #########################################################
# note: Precipitation only!

# filter data on calibration period
#calibration period 1996-2015
ERA5_refprec <- ERA5_2[ERA5_2$Timestamps >= "1996-01-01" & ERA5_2$Timestamps <= "2015-12-31",]
Prec_346_ref <- Prec_346[Prec_346$Timestamps >= "1996-01-01" & Prec_346$Timestamps <= "2015-12-31",]

# construct ecdf functions
ecdf_Prec_346 <- ecdf(Prec_346_ref$rainfallsum)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$precipitation)

#get ranks of total ERA5 dataset according to calibration period
prec_ranks <- ecdf_ERA5_prec(ERA5_2$precipitation)

#quantile mapping according to reftemp and ranks
prec_downscaled <- quantile(Prec_346_ref$rainfallsum, prec_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_2_downscaled <- cbind(ERA5_2, prec_downscaled)

#rename table variables
prec <- ERA5_2_downscaled$prec_downscaled
dwns_346 <- ERA5_2_downscaled %>% dplyr::select(Timestamps)
dwns_346 <- cbind(dwns_346, prec)

######################################################### STATION 347 #########################################################

# filter data on calibration period
#calibration period T: 1999-2007
#calibration period P: 1996-2004
ERA5_reftemp <- ERA5_3[ERA5_3$Timestamps >= "1999-01-01" & ERA5_3$Timestamps <= "2007-12-31",]
T_347_reftemp <- T_347[T_347$Timestamps >= "1999-01-01" & T_347$Timestamps <= "2007-12-31",]
ERA5_refprec <- ERA5_3[ERA5_3$Timestamps >= "1996-01-01" & ERA5_3$Timestamps <= "2004-12-31",]
Prec_347_ref <- Prec_347[Prec_347$Timestamps >= "1996-01-01" & Prec_347$Timestamps <= "2004-12-31",]

# construct ecdf functions
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$precipitation)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_3$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_3$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_3$precipitation)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(T_347_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(T_347_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Prec_347_ref$rainfallsum, prec_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_3_downscaled <- cbind(ERA5_3, mintemp_downscaled, maxtemp_downscaled, prec_downscaled)

#rename table variables
mintemp <- ERA5_3_downscaled$mintemp_downscaled
maxtemp <- ERA5_3_downscaled$maxtemp_downscaled
prec <- ERA5_3_downscaled$prec_downscaled
dwns_347 <- ERA5_3_downscaled %>% dplyr::select(Timestamps)
dwns_347 <- cbind(dwns_347, mintemp, maxtemp, prec)

######################################################### STATION 349 #########################################################

# filter data on calibration period
#calibration period T: 1989-2002
#calibration period P: 1996-2009
ERA5_reftemp <- ERA5_3[ERA5_3$Timestamps >= "1989-01-01" & ERA5_3$Timestamps <= "2002-12-31",]
T_349_reftemp <- T_349[T_349$Timestamps >= "1989-01-01" & T_349$Timestamps <= "2002-12-31",]
ERA5_refprec <- ERA5_3[ERA5_3$Timestamps >= "1996-01-01" & ERA5_3$Timestamps <= "2009-12-31",]
Prec_349_ref <- Prec_349[Prec_349$Timestamps >= "1996-01-01" & Prec_349$Timestamps <= "2009-12-31",]

# construct ecdf functions
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$precipitation)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_3$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_3$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_3$precipitation)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(T_349_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(T_349_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Prec_349_ref$rainfallsum, prec_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_3_downscaled <- cbind(ERA5_3, mintemp_downscaled, maxtemp_downscaled, prec_downscaled)

#rename table variables
mintemp <- ERA5_3_downscaled$mintemp_downscaled
maxtemp <- ERA5_3_downscaled$maxtemp_downscaled
prec <- ERA5_3_downscaled$prec_downscaled
dwns_349 <- ERA5_3_downscaled %>% dplyr::select(Timestamps)
dwns_349 <- cbind(dwns_349, mintemp, maxtemp, prec)

######################################################### STATION 363 #########################################################

# filter data on calibration period
#calibration period T: 1992
#calibration period P: 1991-1992
ERA5_reftemp <- ERA5_2[ERA5_2$Timestamps >= "1992-01-01" & ERA5_2$Timestamps <= "1992-12-31",]
T_363_reftemp <- T_363[T_363$Timestamps >= "1992-01-01" & T_363$Timestamps <= "1992-12-31",]
ERA5_refprec <- ERA5_2[ERA5_2$Timestamps >= "1991-01-01" & ERA5_2$Timestamps <= "1992-12-31",]
Prec_363_ref <- Prec_363[Prec_363$Timestamps >= "1991-01-01" & Prec_363$Timestamps <= "1992-12-31",]

# construct ecdf functions
ecdf_T_363_mintemp <- ecdf(T_363_reftemp$mintemp)
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_T_363_maxtemp <- ecdf(T_363_reftemp$maxtemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_Prec_363 <- ecdf(Prec_363_ref$rainfallsum)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$precipitation)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_2$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_2$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_2$precipitation)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(T_363_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(T_363_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Prec_363_ref$rainfallsum, prec_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_2_downscaled <- cbind(ERA5_2, mintemp_downscaled, maxtemp_downscaled, prec_downscaled)

#rename table variables
mintemp <- ERA5_2_downscaled$mintemp_downscaled
maxtemp <- ERA5_2_downscaled$maxtemp_downscaled
prec <- ERA5_2_downscaled$prec_downscaled
dwns_363 <- ERA5_2_downscaled %>% dplyr::select(Timestamps)
dwns_363 <- cbind(dwns_363, mintemp, maxtemp, prec)

######################################################### STATION Kmd #########################################################

#on daily timescale!
#including SolRad

# filter data on calibration period
#calibration period T: 2020 jan - aug
#calibration period P: 2020 jan - aug
ERA5_reftemp <- ERA5_highr4_d[ERA5_highr4_d$Timestamps >= "2020-01-01" & ERA5_highr4_d$Timestamps < "2020-09-01",]
Meteo_Kmd_d_reftemp <- Meteo_Kmd_d[Meteo_Kmd_d$Timestamps >= "2020-01-01" & Meteo_Kmd_d$Timestamps < "2020-09-01",]
ERA5_refprec <- ERA5_highr4_d[ERA5_highr4_d$Timestamps >= "2020-01-01" & ERA5_highr4_d$Timestamps < "2020-09-01",]
Meteo_Kmd_d_ref <- Meteo_Kmd_d[Meteo_Kmd_d$Timestamps >= "2020-01-01" & Meteo_Kmd_d$Timestamps < "2020-09-01",]

# construct ecdf functions
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$prec)
ecdf_ERA5_SolRad <- ecdf(ERA5_refprec$SolRad)
ecdf_ERA5_Temperature <- ecdf(ERA5_reftemp$Temperature)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_highr4_d$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_highr4_d$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_highr4_d$prec)
SolRad_ranks <- ecdf_ERA5_SolRad(ERA5_highr4_d$SolRad)
Temperature_ranks <- ecdf_ERA5_Temperature(ERA5_highr4_d$Temperature)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(Meteo_Kmd_d_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(Meteo_Kmd_d_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Meteo_Kmd_d_ref$Prec, prec_ranks, na.rm = T)
SolRad_downscaled <- quantile(Meteo_Kmd_d_ref$SolRad, SolRad_ranks, na.rm = T)
Temperature_downscaled <- quantile(Meteo_Kmd_d_reftemp$Temperature, Temperature_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_4_downscaled <- cbind(ERA5_highr4_d, mintemp_downscaled, maxtemp_downscaled, prec_downscaled, SolRad_downscaled,
                           Temperature_downscaled)

#rename table variables
mintemp <- ERA5_4_downscaled$mintemp_downscaled
maxtemp <- ERA5_4_downscaled$maxtemp_downscaled
prec <- ERA5_4_downscaled$prec_downscaled
SolRad <- ERA5_4_downscaled$SolRad_downscaled
Temperature <- ERA5_4_downscaled$Temperature_downscaled
dwns_Kmd <- ERA5_4_downscaled %>% dplyr::select(Timestamps)
dwns_Kmd <- cbind(dwns_Kmd, mintemp, maxtemp, prec, SolRad, Temperature)

################################################### STATION Kmd hourly resolution ##################################################

#on hourly timescale!
#including SolRad
#including Temperature
#excluding mintemp and maxtemp

# filter data on calibration period
#calibration period T: 2020 jan - aug midnight
#calibration period P: 2020 jan - aug midnight
ERA5_reftemp <- ERA5_highr4[ERA5_highr4$Timestamps >= "2020-01-01 00:45:00" & ERA5_highr4$Timestamps < "2020-09-01 00:45:00",]
Meteo_Kmd_reftemp <- Meteo_Kmd_h[Meteo_Kmd_h$Timestamps >= "2020-01-01 00:00:00" & Meteo_Kmd_h$Timestamps < "2020-09-01 00:00:00",]
ERA5_refprec <- ERA5_highr4[ERA5_highr4$Timestamps >= "2020-01-01 00:45:00" & ERA5_highr4$Timestamps < "2020-09-01 00:45:00",]
Meteo_Kmd_ref <- Meteo_Kmd_h[Meteo_Kmd_h$Timestamps >= "2020-01-01 00:00:00" & Meteo_Kmd_h$Timestamps < "2020-09-01 00:00:00",]

# construct ecdf functions
ecdf_ERA5_prec <- ecdf(ERA5_refprec$prec)
ecdf_ERA5_SolRad <- ecdf(ERA5_refprec$SolRad)
ecdf_ERA5_Temperature <- ecdf(ERA5_reftemp$Temperature)

#get ranks of total ERA5 dataset according to calibration period
prec_ranks <- ecdf_ERA5_prec(ERA5_highr4$prec)
SolRad_ranks <- ecdf_ERA5_SolRad(ERA5_highr4$SolRad)
Temperature_ranks <- ecdf_ERA5_Temperature(ERA5_highr4$Temperature)

#quantile mapping according to reftemp and ranks
prec_downscaled <- quantile(Meteo_Kmd_ref$Prec, prec_ranks, na.rm = T)
SolRad_downscaled <- quantile(Meteo_Kmd_ref$SolRad, SolRad_ranks, na.rm = T)
Temperature_downscaled <- quantile(Meteo_Kmd_ref$Temperature, Temperature_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_4_downscaled <- cbind(ERA5_highr4, prec_downscaled, SolRad_downscaled, Temperature_downscaled)

#rename table variables
prec <- ERA5_4_downscaled$prec_downscaled
SolRad <- ERA5_4_downscaled$SolRad_downscaled
Temperature <- ERA5_4_downscaled$Temperature_downscaled
dwns_Kmd_h <- ERA5_4_downscaled %>% dplyr::select(Timestamps)
dwns_Kmd_h <- cbind(dwns_Kmd_h, prec, SolRad, Temperature)

######################################################### STATION Brp #########################################################

#on daily timescale!
#including SolRad

# filter data on calibration period
#calibration period T: 2020 feb 18th - sept 1st
#calibration period P: 2020 feb 18th - sept 1st
ERA5_reftemp <- ERA5_highr5_d[ERA5_highr5_d$Timestamps >= "2020-02-18" & ERA5_highr5_d$Timestamps <= "2020-09-01",]
Meteo_Brp_d_reftemp <- Meteo_Brp_d[Meteo_Brp_d$Timestamps >= "2020-02-18" & Meteo_Brp_d$Timestamps <= "2020-09-01",]
ERA5_refprec <- ERA5_highr5_d[ERA5_highr5_d$Timestamps >= "2020-02-18" & ERA5_highr5_d$Timestamps <= "2020-09-01",]
Meteo_Brp_d_ref <- Meteo_Brp_d[Meteo_Brp_d$Timestamps >= "2020-02-18" & Meteo_Brp_d$Timestamps <= "2020-09-01",]

# construct ecdf functions
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$prec)
ecdf_ERA5_SolRad <- ecdf(ERA5_refprec$SolRad)
ecdf_ERA5_Temperature <- ecdf(ERA5_reftemp$Temperature)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_highr5_d$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_highr5_d$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_highr5_d$prec)
SolRad_ranks <- ecdf_ERA5_SolRad(ERA5_highr5_d$SolRad)
Temperature_ranks <- ecdf_ERA5_Temperature(ERA5_highr5_d$Temperature)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(Meteo_Brp_d_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(Meteo_Brp_d_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Meteo_Brp_d_ref$Prec, prec_ranks, na.rm = T)
SolRad_downscaled <- quantile(Meteo_Brp_d_ref$SolRad, SolRad_ranks, na.rm = T)
Temperature_downscaled <- quantile(Meteo_Brp_d_ref$Temperature, Temperature_ranks, na.rm=T)

#full ERA5 and downscaled table
ERA5_highr5_d_downscaled <- cbind(ERA5_highr5_d, mintemp_downscaled, maxtemp_downscaled, prec_downscaled, SolRad_downscaled,
                                  Temperature_downscaled)

#rename table variables
mintemp <- ERA5_highr5_d_downscaled$mintemp_downscaled
maxtemp <- ERA5_highr5_d_downscaled$maxtemp_downscaled
prec <- ERA5_highr5_d_downscaled$prec_downscaled
SolRad <- ERA5_highr5_d_downscaled$SolRad_downscaled
Temperature <- ERA5_highr5_d_downscaled$Temperature_downscaled
dwns_Brp <- ERA5_highr5_d_downscaled %>% dplyr::select(Timestamps)
dwns_Brp <- cbind(dwns_Brp, mintemp, maxtemp, prec, SolRad, Temperature)

################################################### STATION Brp hourly resolution ##################################################

#on hourly timescale!
#including SolRad
#including Temperature
#excluding mintemp and maxtemp

# filter data on calibration period
#calibration period T: 2020 feb 18th - sept 1st
#calibration period P: 2020 feb 18th - sept 1st
ERA5_reftemp <- ERA5_highr5[ERA5_highr5$Timestamps >= "2020-02-18 00:45:00" & ERA5_highr5$Timestamps < "2020-09-01 00:45:00",]
Meteo_Brp_reftemp <- Meteo_Brp_h[Meteo_Brp_h$Timestamps >= "2020-02-18 00:00:00" & Meteo_Brp_h$Timestamps < "2020-09-01 00:00:00",]
ERA5_refprec <- ERA5_highr5[ERA5_highr5$Timestamps >= "2020-02-18 00:45:00" & ERA5_highr5$Timestamps < "2020-09-01 00:45:00",]
Meteo_Brp_ref <- Meteo_Brp_h[Meteo_Brp_h$Timestamps >= "2020-02-18 00:00:00" & Meteo_Brp_h$Timestamps < "2020-09-01 00:00:00",]

# construct ecdf functions
ecdf_ERA5_prec <- ecdf(ERA5_refprec$prec)
ecdf_ERA5_SolRad <- ecdf(ERA5_refprec$SolRad)
ecdf_ERA5_Temperature <- ecdf(ERA5_reftemp$Temperature)

#get ranks of total ERA5 dataset according to calibration period
prec_ranks <- ecdf_ERA5_prec(ERA5_highr5$prec)
SolRad_ranks <- ecdf_ERA5_SolRad(ERA5_highr5$SolRad)
Temperature_ranks <- ecdf_ERA5_Temperature(ERA5_highr5$Temperature)

#quantile mapping according to reftemp and ranks
prec_downscaled <- quantile(Meteo_Brp_ref$Prec, prec_ranks, na.rm = T)
SolRad_downscaled <- quantile(Meteo_Brp_ref$SolRad, SolRad_ranks, na.rm = T)
Temperature_downscaled <- quantile(Meteo_Brp_ref$Temperature, Temperature_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_5_downscaled <- cbind(ERA5_highr5, prec_downscaled, SolRad_downscaled, Temperature_downscaled)

#rename table variables
prec <- ERA5_5_downscaled$prec_downscaled
SolRad <- ERA5_5_downscaled$SolRad_downscaled
Temperature <- ERA5_5_downscaled$Temperature_downscaled
dwns_Brp_h <- ERA5_5_downscaled %>% dplyr::select(Timestamps)
dwns_Brp_h <- cbind(dwns_Brp_h, prec, SolRad, Temperature)

######################################################### STATION Spg #########################################################

#on daily timescale!

# filter data on calibration period
#calibration period T: 2020 feb 18th - sept 1st
#calibration period P: 2020 feb 18th - sept 1st
ERA5_reftemp <- ERA5_1[ERA5_1$Timestamps >= "2020-02-18" & ERA5_1$Timestamps <= "2020-09-01",]
Meteo_Spg_d_reftemp <- Meteo_Spg_d[Meteo_Spg_d$Timestamps >= "2020-02-18" & Meteo_Spg_d$Timestamps <= "2020-09-01",]
ERA5_refprec <- ERA5_1[ERA5_1$Timestamps >= "2020-02-18" & ERA5_1$Timestamps <= "2020-09-01",]
Meteo_Spg_d_ref <- Meteo_Spg_d[Meteo_Spg_d$Timestamps >= "2020-02-18" & Meteo_Spg_d$Timestamps <= "2020-09-01",]

# construct ecdf functions
ecdf_ERA5_mintemp <- ecdf(ERA5_reftemp$mintemp)
ecdf_ERA5_maxtemp <- ecdf(ERA5_reftemp$maxtemp)
ecdf_ERA5_prec <- ecdf(ERA5_refprec$precipitation)

#get ranks of total ERA5 dataset according to calibration period
mintemp_ranks <- ecdf_ERA5_mintemp(ERA5_1$mintemp)
maxtemp_ranks <- ecdf_ERA5_maxtemp(ERA5_1$maxtemp)
prec_ranks <- ecdf_ERA5_prec(ERA5_1$precipitation)

#quantile mapping according to reftemp and ranks
mintemp_downscaled <- quantile(Meteo_Spg_d_reftemp$mintemp, mintemp_ranks, na.rm = T)
maxtemp_downscaled <- quantile(Meteo_Spg_d_reftemp$maxtemp, maxtemp_ranks, na.rm = T)
prec_downscaled <- quantile(Meteo_Spg_d_ref$Prec, prec_ranks, na.rm = T)

#full ERA5 and downscaled table
ERA5_1_downscaled <- cbind(ERA5_1, mintemp_downscaled, maxtemp_downscaled, prec_downscaled)

#rename table variables
mintemp <- ERA5_1_downscaled$mintemp_downscaled
maxtemp <- ERA5_1_downscaled$maxtemp_downscaled
prec <- ERA5_1_downscaled$prec_downscaled
dwns_Spg <- ERA5_1_downscaled %>% dplyr::select(Timestamps)
dwns_Spg <- cbind(dwns_Spg, mintemp, maxtemp, prec)