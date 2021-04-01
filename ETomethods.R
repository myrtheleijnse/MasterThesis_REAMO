########################################### ETo methods ###########################################

#note that the downscaled data is necessary here, 
#so before using functions, run "QM_downscaling.R"
#also run "RetrieveDate.R"

##################################### Makkink ETo #####################################
#Makkink Formula: ETo = C * delta/(delta+gamma) * Rs/(lambda*rho)
#inputdata: temperature and solar radiation
#data on daily scale

################################################################
##################### Makkink ETo function #####################
################################################################
meteo_makkink <- function(table){
  #set initials
  i = 1
  table$ETo = 0
  
  #calculate Makkink ETo
  while (i < nrow(table)){
    eo = 0.6107*10^(7.5*table$Temperature[i]/(237.3+table$Temperature[i]))            #saturation vapour pressure [kPa]
    delta = 7.5*237.3/(237.3 + table$Temperature[i])^2*ln(10)*eo                      #slope of saturation vapour pressure curve [kPa/C]
    gamma = 0.0646 + 0.00006 * table$Temperature[i]                                   #psychometric constant [kPa]
    lambda = (2501-2.375*table$Temperature[i]) * 1000                                 #heat of vaporization [J/kg]
    c = 0.65                                                                          #constant [-]
    rho = 1000                                                                        #density of water [kg/m3]
    Rs = table$SolRad[i] *3600 * 24                                                   #incoming shortwave solar radiation [J/m2/day]
    
    table$ETo[i] = c * delta/(delta+gamma) * Rs/(lambda*rho) * 1000                   #reference evaporation [mm/day]
    i = i + 1
  }
  return(table)
}

##################################### Hargreaves ETo #####################################
#Hargreaves formula: ETo = 0.408 * Ra * 0.0023 (Tmean + 17.8) * (Tmax - Tmin)^0.5
#inputdata: maxtemp and mintemp
#data on daily scale

####################################################################
##################### Hargreaves ETo  function #####################
####################################################################
meteo_hargreaves <- function(table){
  table <- getdays(table)
  table <- renametoprec(table)
  
  #set initials
  table$ETo = 0
  table$Precdef = 0
  i = 1
  
  #calculation of Hargreaves ETo
  while (i < nrow(table)){
    Ra = 33                                   #extraterrestrial radiation [MJ/m2/day] (assumption)
    c = 0.408                                 #conversion MJ/m2/day to mm/day
    Tmean = (table$maxtemp[i] + table$mintemp[i])/2
    
    table$ETo[i] = c * Ra * 0.0023 * (Tmean + 17.8) * sqrt(table$maxtemp[i] - table$mintemp[i])
    table$Precdef[i] = table$prec[i] - table$ETo[i]
    i = i + 1
  }
  
  return(table)
}

##################################### Penman-Monteith ETo #####################################
#Penman-Monteith formula: ETo = (0.408 * delta * (Rn - G) + gamma * (900 / (Temperature + 273)) * u * (es - ea)) / (delta + gamma * (1 + 0.34 * u)) 
#!note input on 15 min scale; input startdate: "%Y-%M-%d %H"
#inputdata: temperature, windspeed, atmospheric pressure, min temperature, max temperature, elevation, precipitation

###########################################################
##################### ETo function PM #####################
###########################################################

meteo_PM <- function(table, startdate, enddate, elevation){
  #to daily time scale
  table$Timestamps <- strptime(table$Timestamps, "%Y-%m-%d")
  table$Timestamps <- as.POSIXct(table$Timestamps)
  table <- table %>% group_by(Timestamps) %>% summarize(SolRad = mean(SolRad), 
                                                        prec = sum(prec), 
                                                        WindSpeed = mean(WindSpeed), 
                                                        VaporPressure = mean(VaporPressure), 
                                                        AtmosphericPressure = mean(AtmosphericPressure),
                                                        mintemp = min(Temperature),
                                                        maxtemp = max(Temperature),
                                                        Temperature = mean(Temperature))
  table <- getleapdays(table)
  
  #subset table
  table <- table[table$Timestamps >= startdate & table$Timestamps <= enddate,]
  
  #set initials
  table$ETo = 0
  table$ETosum = 0
  table$precsum = 0
  table$precdef = 0
  i = 1
  
  #while loop for calculating PM ETo, accumulation of P, E and P deficit
  while (i <= nrow(table)){
    ## variables of PM calculation
    ea <- table$VaporPressure[i]                                                        #actual vapour pressure [kPa]
    gamma <- 0.665*10^-3 * table$AtmosphericPressure[i]                                 #psychrometric constant [kPa/C]
    eo <- 0.6108 * exp(17.27 * table$Temperature[i] / (table$Temperature[i] + 237.3))   #saturation pressure [kPa]
    es <- ((0.6108 * exp(17.27 * table$maxtemp[i] / (table$maxtemp[i] + 237.3))) + 
             (0.6108 * exp(17.27 * table$mintemp[i] / (table$mintemp[i] + 237.3)))) / 2 #saturation vapour pressure [kPa]
    G = 0                                                                               #ground soil flux assumed to be 0 on daily time scale
    u <- table$WindSpeed[i]                                                             #wind speed [m/s]
    delta <- (4098 * eo) / (table$Temperature[i] + 237.3)^2                             #slope of saturation vapour pressure curve [kPa/C]
    
    # radiation variables of PM calculation
    alpha = 0.23                                                                        #albedo [-]
    Rs = table$SolRad[i]*10^-6 * 3600 * 24                                              #shortwave radiation [MJ/m2/day]
    Sn <- (1-alpha) * Rs                                                                #net shortwave radiation [MJ/m2/day] with conversion from s to h
    
    Ra = 33                                                                             #extraterrestrial radiation [MJ/m2/day]
    
    Rso = (0.75 + 2 * 10 ^-5 * elevation) * Ra                                          #clear sky solar radiation [MJ/m2/day]
    
    Ln = 4.903 * 10^-9 *((table$maxtemp[i]+273.16)^4+(table$mintemp[i]+273.16)^4)/2 * (0.34 - 0.14*sqrt(ea)) * 
      (1.35 * Rs/Rso - 0.35)                                                            #net outgoing longwave radiation [MJ/m2/day]
    
    Rn = Sn - Ln                                                                        #net radiation [MJ/m2/day]
    
    
    #PM calculation per t step
    table$ETo[i] = (0.408 * delta * (Rn - G) + gamma * (900 / (table$Temperature[i] + 273)) * u * (es - ea)) / 
      (delta + gamma * (1 + 0.34 * u))                                                  #reference evaporation [mm/day]
    
    i = i+1
    }
  return(table)
}