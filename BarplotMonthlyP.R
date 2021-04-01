######################## Long-term averages and percentile bands of precipitation at all stations ########################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

################################################
#daily maximum precipitation observed per month#
################################################
maxprec <- function(table, stationname){ 
  table <- getdays(table)
  table <- renametoprec(table)
  table <- table[table$year >= 2020,]
  table <- na.omit(table)
  table <- table %>% group_by(month) %>% summarize(prec = max(prec))
  table$Station = stationname
  return(table)
}

##############################################
# anomalies max daily precpitation per month #
##############################################
maxpreclongterm <- function(table, stationname){ 
  #set initials
  table$mean = 0
  table$min = 0
  table$max = 0
  table <- getdays(table)
  table <- renametoprec(table)
  table <- table[table$year >= 2000 & table$year < 2020,]                       #subset
  table <- na.omit(table)                                                       #remove NAs
  table <- table %>% group_by(month, year) %>% summarize(prec = max(prec))      #calculate maximum precipitation per month
  table <- table %>% group_by(month) %>% summarize(mean = mean(prec),
                                                   max = quantile(prec, 0.9),
                                                   min = quantile(prec, 0.1))   #calculate mean P per month and 10-90th percentiles
  table$Station = stationname
  return(table)
}

#Compute maximum daily precipitation per month per station
table1 <- rbind(maxprec(dwns_Spg, "Sherpageon"), maxprec(dwns_363, "Paigutang"), maxprec(dwns_295, "Timure"), 
                maxprec(dwns_346, "Thamachit"), maxprec(dwns_349, "Pansayakhola"), maxprec(dwns_347, "Dhunche"), 
                maxprec(dwns_Kmd, "Kathmandu"), maxprec(dwns_Brp, "Bharatpur"))
#Compute statistics max daily precipitation per month per station
table2 <- rbind(maxpreclongterm(dwns_Spg, "Sherpageon"), maxpreclongterm(dwns_363, "Paigutang"), maxpreclongterm(dwns_295, "Timure"), 
                maxpreclongterm(dwns_346, "Thamachit"), maxpreclongterm(dwns_349, "Pansayakhola"), 
                maxpreclongterm(dwns_347, "Dhunche"), maxpreclongterm(dwns_Kmd, "Kathmandu"), maxpreclongterm(dwns_Brp, "Bharatpur"))
#barplot
ggplot() +
  geom_bar(data = table1, aes(month, prec, fill = Station), col="black", stat = "identity", position = position_dodge(width = 0.9)) +
  geom_pointrange(data = table2, aes(x=month, y=mean, ymin=min, ymax=max, group = Station, 
                                     colour = paste0("Long-term (2000-2019) average", "\n", "with 90-10% percentiles")), 
                  stat = "identity",  alpha=0.9, position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "Blues") +
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  scale_colour_manual(name = "", values = c("black")) +
  labs(y="Precipitation (mm/d)", x="") +
  ggtitle("Maximum daily precipitation per month", "2020 and long-term statistics") +
  theme_minimal() +
  theme(legend.position="bottom")

####################################################
#number of days per month 95 percentile is exceeded#
####################################################
precexceeding <- function(table, stationname){
  table <- getdays(table)
  table <- renametoprec(table)
  threshold <- quantile(table$prec, 0.95, na.rm=T)    #estimate long-term threshold of extreme P
  table <- table[table$year >= 2020,]                 #subset 2020
  table <- na.omit(table)                             #remove NAs
  #Calculate days with exceedence of threshold
  table$exceedence = 0
  i = 1
  while(i <= nrow(table)){
    if (table$prec[i] > threshold){
      table$exceedence[i] = 1
    } else {
      table$exceedence[i] = 0
    }
    i = i+1
  }
  # sum all days with exceedence of threshold
  table <- table %>% group_by(month) %>% summarize(exceedence = sum(exceedence))
  # add station name
  table$Station = stationname
  return(table)
}

################################################################
# anomalies number of days per month 95 percentile is exceeded #
################################################################
precexceedinglongterm <- function(table, stationname){
  #set initials
  table$mean = 0
  table$min = 0
  table$max = 0
  table <- getdays(table)
  table <- renametoprec(table)
  threshold <- quantile(table$prec, 0.95, na.rm=T)          #estimate long-term threshold of extreme P
  table <- table[table$year >= 2000 & table$year < 2020,]   #subset
  table <- na.omit(table)                                   #remove NAs
  #Calculate days with exceedence of threshold
  table$exceedence = 0
  i = 1
  while(i <= nrow(table)){
    if (table$prec[i] > threshold){
      table$exceedence[i] = 1
    } else {
      table$exceedence[i] = 0
    }
    i = i+1
  }
  # sum all days with exceedence of threshold and the mean, 10-90th percentiles
  table <- table %>% group_by(month, year) %>% summarize(exceedence = sum(exceedence))
  table <- table %>% group_by(month) %>% summarize(mean = mean(exceedence),
                                                   min = quantile(exceedence, 0.1),
                                                   max = quantile(exceedence, 0.9))
  #add station name
  table$Station = stationname
  return(table)
}

#Compute days with extreme precipitation per month per station
table1 <- rbind(precexceeding(dwns_Spg, "Sherpageon"), precexceeding(dwns_363, "Paigutang"), precexceeding(dwns_295, "Timure"), 
                precexceeding(dwns_346, "Thamachit"), precexceeding(dwns_349, "Pansayakhola"), precexceeding(dwns_347, "Dhunche"), 
                precexceeding(dwns_Kmd, "Kathmandu"), precexceeding(dwns_Brp, "Bharatpur"))
#Compute statistics days with extreme precipitation per month per station
table2 <- rbind(precexceedinglongterm(dwns_Spg, "Sherpageon"), precexceedinglongterm(dwns_363, "Paigutang"), 
                precexceedinglongterm(dwns_295, "Timure"), precexceedinglongterm(dwns_346, "Thamachit"), 
                precexceedinglongterm(dwns_349, "Pansayakhola"), precexceedinglongterm(dwns_347, "Dhunche"),
                precexceedinglongterm(dwns_Kmd, "Kathmandu"), precexceedinglongterm(dwns_Brp, "Bharatpur"))

#barplot
ggplot() +
  geom_bar(data = table1, aes(month, exceedence, fill = Station), col = "black", stat= "identity", position = "dodge", alpha = 0.8) +
  geom_pointrange(data = table2, aes(x=month, y=mean, ymin=min, ymax=max, group = Station, 
                                     colour = paste0("Long-term (2000-2019) average", "\n", "with 90-10% percentiles")), 
                  stat = "identity",  alpha=0.9, position = position_dodge(width = 0.9)) +
  scale_fill_brewer(palette = "Purples") +
  scale_x_continuous(breaks = seq(1,12,1),
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) + 
  scale_y_continuous(breaks = seq(1, 1000, 2)) +
  scale_colour_manual(name = "", values = c("black")) +
  labs(y="Days", x="") +
  ggtitle("Days per month with extreme precipitation", "2020 and long-term statistics") +
  theme_minimal()+
  theme(legend.position = "bottom")