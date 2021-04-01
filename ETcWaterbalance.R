################## ETc water balance (Makkink) ##################
# using P-ETo(Makkink) (2020)
# using GDD thresholds and old GDD function
# using Kc
# at Kathmandu and Bharatpur stations

#### calculating GDDsum
# GDDsum = regular GDDsum
# GDDsummean = mean GDDsum 2000-2020
# GDDsumlow = low percentile GDDsum 2000-2020
# GDDsumhigh = high percentile GDDsum 2000-2020

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ETomethods.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Reading_data_vFinal") #alter to own working directory
Kc <- read.delim("Kc_Bhandari.txt", sep="")

##################################################
####### calculate GDDsum and ETo anomalies #######
##################################################
GDDsum_f3 <- function(table, startmonth, endmonth, startyear, startdate) {
  table <- getdays(table)
  table <- meteo_makkink(table)
  table <- table[table$year >= startyear & table$month >= startmonth & table$month <= endmonth,]
  
  #get data.table frame
  require(data.table)
  table <- data.table(table)
  
  #subset specific starting date
  table <- table[table$Timestamps >= startdate,]
  
  #calculate accumulated GDD
  table[, GDDsum := cumsum(Temperature), by = list(year)]
  
  return(table)
}

##########################  
##### Precdef posneg #####
##########################
posneg <- function(table){
  i = 1
  table$cumsumPrecdefpos = 0
  table$cumsumPrecdefneg = 0
  while (i <= nrow(table)) {
    if(table$cumsumPrecdef[i] < 0) {
      table$cumsumPrecdefneg[i] = table$cumsumPrecdef[i]
    } else {
      table$cumsumPrecdefpos[i] = table$cumsumPrecdef[i]
    }
    i=i+1
  }
  return(table)
}

##################################################### Rice ######################################################

#date initials
startmonth = 7            
endmonth = 11
startyear = 2000
startdate = "2000-07-15"

#Calculate GDD sum and ETo per station
table1 <- GDDsum_f3(dwns_Kmd, startmonth, endmonth, startyear, startdate)
table1$station <- "Kathmandu"
table1$crop <- "Rice"
table2 <- GDDsum_f3(dwns_Brp, startmonth, endmonth, startyear, startdate)
table2$station <- "Bharatpur"
table2$crop <- "Rice"
table <- rbind(table1, table2)

###############################################
#### Calculating growth stage and ETc Rice ####
###############################################
GSRice <- function(table, startday, startyear) {
  library(dplyr)
  
  # Calculate GS
  #Initial threshold values for growth stages
  RiceInitiation = 1023
  RiceFlowering = 2028
  RiceMaturity = 2793
  
  #subset
  table <- table[table$days >= startday & table$year >= startyear,]
  
  #Give growth stages 0-3 and string 
  table$GS = 0
  table$GSstring = 0
  i = 1
  while (i <= nrow(table)){
    if(table$GDDsum[i] < RiceInitiation){
      table$GS[i] = 0
      table$GSstring[i] = "Sowing"
    } else if(table$GDDsum[i] >= RiceInitiation & table$GDDsum[i] < RiceFlowering){
      table$GS[i] = 1  
      table$GSstring[i] = "Initiation"
    } else if(table$GDDsum[i] >= RiceFlowering & table$GDDsum[i] < RiceMaturity){
      table$GS[i] = 2
      table$GSstring[i] = "Flowering"
    } else {
      table$GS[i] = 3
      table$GSstring[i] = "Maturity"
    }
    i = i + 1
  }
  
  # Calculate ETc
  #set initials
  i = 1
  table$Kc = 0
  table$ETc = 0
  
  #while loop conditional Kc
  while (i <= nrow(table)){
    if(table$GS[i] == 0){
      table$Kc[i] = Kc$initial[3]
    } else if (table$GS[i] == 1){
      table$Kc[i] = Kc$cropdevelop[3]
    } else if (table$GS[i] == 2){
      table$Kc[i] = Kc$midseas[3]
    } else if (table$GS[i] == 3){
      table$Kc[i] = Kc$lateseas[3]
    }
    #calculate crop evaporation ETc
    table$ETc[i] = table$ETo[i] * table$Kc[i]
    
    i = i+1
  }
  
  #calculate cumulative water balance (precdef)
  require(data.table)
  table <- data.table(table)
  table[, Precdef := prec-ETc, by = list(days, station)]
  table[, cumsumPrecdef := cumsum(Precdef), by = list(year, station)]
  
  #separate positive and negative values
  table <- posneg(table)
  
  return(table)
}

startday <- table$days[1] #subset starting day

#Calculate growth stage and ETc water balance for observations (obs), all years (full), 2015
tableobs <- GSRice(table, startday, 2020)
table_full <- GSRice(table, startday, 2000)
print(table_full %>% group_by(year, station) %>% summarize(ETcsum = sum(ETc))) # Table yearly summed ETc
table2015 <- table_full[table_full$year == 2015,]

#Calculate median, high and low percentiles
tablemedian <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdef = median(cumsumPrecdef))
tablehigh <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdefhigh = quantile(cumsumPrecdef, 0.9)) 
tablelow <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdeflow = quantile(cumsumPrecdef, 0.1))
tablepercentile <- cbind(tablelow, tablehigh$cumsumPrecdefhigh)
colnames(tablepercentile)[colnames(tablepercentile) == "...5"] <- "cumsumPrecdefhigh"

### Plotting ETc
ggplot() +
  geom_ribbon(data = tablepercentile, aes(x=days, ymin = cumsumPrecdefhigh, ymax = cumsumPrecdeflow, 
                                          fill = paste0("10-90% range", "\n", "2000-2020")), alpha = 0.3) +
  geom_line(data = tableobs, aes(days, cumsumPrecdef, colour = "black"), lwd = 1.2) +
  geom_area(data = tableobs, aes(days, cumsumPrecdefpos, fill = "Positive"), alpha = 0.5) +
  geom_area(data = tableobs, aes(days, cumsumPrecdefneg, fill = "Negative"), alpha = 0.5) + 
  geom_line(data = table2015, aes(days, cumsumPrecdef, colour = "purple"), lwd = 1) +
  geom_line(data = tablemedian, aes(days, cumsumPrecdef, colour = "red"), lwd = 1) +
  facet_wrap(station~crop) +
  labs(x = "", y = expression(paste(Sigma, "P-ETc (mm)"))) +
  ggtitle("Crop specific cumulative climatic water balance", paste0("Sowing date: ", min(tableobs$Timestamps))) +
  geom_hline(yintercept = 0, col="grey", lty = "longdash") +
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(name = "Legend", labels = c("2020", "2015", "Median 2000-2020"), 
                     values = c("black","purple", "red3")) +
  scale_fill_manual(name = "Water balance", values = c("grey70","red","steelblue")) +
  theme_minimal()

############################################ Maize ################################################

#date initials
startmonth = 4            
endmonth = 6
startyear = 2000
startdate = "2000-04-01"

#Calculate GDD sum and ETo per station
table1 <- GDDsum_f3(dwns_Kmd, startmonth, endmonth, startyear, startdate)
table1$station <- "Kathmandu"
table1$crop <- "Maize"
table2 <- GDDsum_f3(dwns_Brp, startmonth, endmonth, startyear, startdate)
table2$station <- "Bharatpur"
table2$crop <- "Maize"
table <- rbind(table1, table2)

###############################################
#### Calculating growth stage and ETc Maize ####
###############################################
GSMaize <- function(table, startday, startyear) {
  library(dplyr)
  
  # Calculate GS
  #Initial threshold values for growth stages
  MaizeInitiation = 642        
  MaizeFlowering = 950          #anthesis
  MaizeMaturity = 1481
  
  #subset
  table <- table[table$days >= startday & table$year >= startyear,]
  
  #Give growth stages 0-3 and string 
  table$GS = 0
  table$GSstring = 0
  i = 1
  while (i <= nrow(table)){
    if(table$GDDsum[i] < MaizeInitiation){
      table$GS[i] = 0
      table$GSstring[i] = "Sowing"
    } else if(table$GDDsum[i] >= MaizeInitiation & table$GDDsum[i] < MaizeFlowering){
      table$GS[i] = 1  
      table$GSstring[i] = "Initiation"
    } else if(table$GDDsum[i] >= MaizeFlowering & table$GDDsum[i] < MaizeMaturity){
      table$GS[i] = 2
      table$GSstring[i] = "Flowering"
    } else {
      table$GS[i] = 3
      table$GSstring[i] = "Maturity"
    }
    i = i + 1
  }
  
  # Calculate ETc
  #set initials
  i = 1
  table$Kc = 0
  table$ETc = 0
  
  #while loop conditional Kc
  while (i <= nrow(table)){
    if(table$GS[i] == 0){
      table$Kc[i] = Kc$initial[2]
    } else if (table$GS[i] == 1){
      table$Kc[i] = Kc$cropdevelop[2]
    } else if (table$GS[i] == 2){
      table$Kc[i] = Kc$midseas[2]
    } else if (table$GS[i] == 3){
      table$Kc[i] = Kc$lateseas[2]
    }
    #calculate crop evaporation ETc
    table$ETc[i] = table$ETo[i] * table$Kc[i]
    
    i = i+1
  }
  
  #calculate cumulative water balance (precdef)
  require(data.table)
  table <- data.table(table)
  table[, Precdef := prec-ETc, by = list(days, station)]
  table[, cumsumPrecdef := cumsum(Precdef), by = list(year, station)]
  
  #separate positive and negative values
  table <- posneg(table)
  
  return(table)
}

startday <- table$days[1] #subset starting day

#Calculate growth stage and ETc water balance for observations (obs), all years (full), 2015
tableobs <- GSMaize(table, startday, 2020)
table_full <- GSMaize(table, startday, 2000)
print(table_full %>% group_by(year, station) %>% summarize(ETcsum = sum(ETc))) # Table yearly summed ETc
table2015 <- table_full[table_full$year == 2015,]

#Calculate median, high and low percentiles
tablemedian <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdef = median(cumsumPrecdef))
tablehigh <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdefhigh = quantile(cumsumPrecdef, 0.9)) 
tablelow <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdeflow = quantile(cumsumPrecdef, 0.1))
tablepercentile <- cbind(tablelow, tablehigh$cumsumPrecdefhigh)
colnames(tablepercentile)[colnames(tablepercentile) == "...5"] <- "cumsumPrecdefhigh"

### Plotting ETc
ggplot() +
  geom_ribbon(data = tablepercentile, aes(x=days, ymin = cumsumPrecdefhigh, ymax = cumsumPrecdeflow, 
                                          fill = paste0("10-90% range", "\n", "2000-2020")), alpha = 0.3) +
  geom_line(data = tableobs, aes(days, cumsumPrecdef, colour = "black"), lwd = 1.2) +
  geom_area(data = tableobs, aes(days, cumsumPrecdefpos, fill = "Positive"), alpha = 0.5) +
  geom_area(data = tableobs, aes(days, cumsumPrecdefneg, fill = "Negative"), alpha = 0.5) + 
  geom_line(data = table2015, aes(days, cumsumPrecdef, colour = "purple"), lwd = 1) +
  geom_line(data = tablemedian, aes(days, cumsumPrecdef, colour = "red"), lwd = 1) +
  facet_wrap(station~crop) +
  labs(x = "", y = expression(paste(Sigma, "P-ETc (mm)"))) +
  ggtitle("Crop specific cumulative climatic water balance", paste0("Sowing date: ", min(tableobs$Timestamps))) +
  geom_hline(yintercept = 0, col="grey", lty = "longdash") +
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(name = "Legend", labels = c("2020", "2015", "Median 2000-2020"), 
                     values = c("black","purple", "red3")) +
  scale_fill_manual(name = "Water balance", values = c("grey70","red","steelblue")) +
  theme_minimal()

############################################ Wheat #################################################

#date initials
startmonth = 1            
endmonth = 3
startyear = 2000
startdate = "2000-01-01"

#Calculate GDD sum and ETo per station
table1 <- GDDsum_f3(dwns_Kmd, startmonth, endmonth, startyear, startdate)
table1$station <- "Kathmandu"
table1$crop <- "Wheat"
table2 <- GDDsum_f3(dwns_Brp, startmonth, endmonth, startyear, startdate)
table2$station <- "Bharatpur"
table2$crop <- "Wheat"
table <- rbind(table1, table2)

################################################
#### Calculating growth stage and ETc Wheat ####
################################################
GSWheat <- function(table, startday, startyear) {
  #Initial threshold values for growth stages
  WheatInitiation = 203
  WheatFlowering = 701
  WheatMaturity = 1071
  
  #subset
  table <- table[table$days >= startday & table$year >= startyear,]
  
  #Give growth stages 0-3 and string
  table$GS = 0
  table$GSstring = 0
  i = 1
  while (i <= nrow(table)){
    if(table$GDDsum[i] < WheatInitiation){
      table$GS[i] = 0
      table$GSstring[i] = "Sowing"
    } else if(table$GDDsum[i] >= WheatInitiation & table$GDDsum[i] < WheatFlowering){
      table$GS[i] = 1
      table$GSstring[i] = "Initiation"
    } else if(table$GDDsum[i] >= WheatFlowering & table$GDDsum[i] < WheatMaturity){
      table$GS[i] = 2
      table$GSstring[i] = "Flowering"
    } else {
      table$GS[i] = 3
      table$GSstring[i] = "Maturity"
    }
    i = i + 1
  }
  
  # Calculate ETc
  #set initials
  i = 1
  table$Kc = 0
  table$ETc = 0
  
  #while loop conditional Kc
  while (i <= nrow(table)){
    if(table$GS[i] == 0){
      table$Kc[i] = Kc$initial[1]
    } else if (table$GS[i] == 1){
      table$Kc[i] = Kc$cropdevelop[1]
    } else if (table$GS[i] == 2){
      table$Kc[i] = Kc$midseas[1]
    } else if (table$GS[i] == 3){
      table$Kc[i] = Kc$lateseas[1]
    }
    #calculate crop evaporation ETc
    table$ETc[i] = table$ETo[i] * table$Kc[i]
    
    i = i+1
  }
  
  #calculate cumulative water balance (precdef)
  require(data.table)
  table <- data.table(table)
  table[, Precdef := prec-ETc, by = list(days, station)]
  table[, cumsumPrecdef := cumsum(Precdef), by = list(year, station)]
  
  #separate positive and negative values
  table <- posneg(table)
  
  return(table)
}

startday <- table$days[1] #subset starting day

#Calculate growth stage and ETc water balance for observations (obs), all years (full), 2015
tableobs <- GSWheat(table, startday, 2020)
table_full <- GSWheat(table, startday, 2000)
print(table_full %>% group_by(year, station) %>% summarize(ETcsum = sum(ETc))) # Table yearly summed ETc
table2015 <- table_full[table_full$year == 2015,]

#Calculate median, high and low percentiles
tablemedian <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdef = median(cumsumPrecdef))
tablehigh <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdefhigh = quantile(cumsumPrecdef, 0.9)) 
tablelow <- table_full %>% group_by(days, station, crop) %>% summarize(cumsumPrecdeflow = quantile(cumsumPrecdef, 0.1))
tablepercentile <- cbind(tablelow, tablehigh$cumsumPrecdefhigh)
colnames(tablepercentile)[colnames(tablepercentile) == "...5"] <- "cumsumPrecdefhigh"

### Plotting ETc
ggplot() +
  geom_ribbon(data = tablepercentile, aes(x=days, ymin = cumsumPrecdefhigh, ymax = cumsumPrecdeflow, 
                                          fill = paste0("10-90% range", "\n", "2000-2020")), alpha = 0.3) +
  geom_line(data = tableobs, aes(days, cumsumPrecdef, colour = "black"), lwd = 1.2) +
  geom_area(data = tableobs, aes(days, cumsumPrecdefpos, fill = "Positive"), alpha = 0.5) +
  geom_line(data = table2015, aes(days, cumsumPrecdef, colour = "purple"), lwd = 1) +
  geom_area(data = tableobs, aes(days, cumsumPrecdefneg, fill = "Negative"), alpha = 0.5) + 
  geom_line(data = tablemedian, aes(days, cumsumPrecdef, colour = "red"), lwd = 1) +
  facet_wrap(station~crop) +
  labs(x = "", y = expression(paste(Sigma, "P-ETc (mm)"))) +
  ggtitle("Crop specific cumulative climatic water balance", paste0("Sowing date: ", min(tableobs$Timestamps))) +
  geom_hline(yintercept = 0, col="grey", lty = "longdash") +
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(name = "Legend", labels = c("2020", "2015", "Median 2000-2020"), 
                     values = c("black","purple", "red3")) + 
  scale_fill_manual(name = "Water balance", values = c("grey70","red","steelblue")) +
  theme_minimal()