################################################### Crop calendar 2020 ###################################################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

#####################################
####### calculate GDDsum 2020 #######
#####################################
GDDsum_f2 <- function(table, startdate) {
  table <- getdays(table)
  # subset 2020
  table <- table[table$Timestamps >= "2020-01-01",]
  
  #While loop for calculation of accumulated GDD
  table$GDD = 0
  table$GDDsum = 0
  i = which(table$Timestamps == startdate)
  
  while (i < nrow(table)) {
    table$GDD[i] = table$Temperature[i]
    if(is.na(table$GDD[i])){
      table$GDDsum[i] = table$GDDsum[i]
    } else if (i == 1) {
      table$GDDsum[i] = table$GDD[i]
    } else {
      table$GDDsum[i] = table$GDD[i] + table$GDDsum[i-1]
    }
    i = i + 1
  }
  
  return(table)
}

#################################################
####### calculate growth stages (GS) Rice #######
#################################################
GSRice <- function(table) {
  library(dplyr)
  # Calculate GS
  #Initial threshold values for growth stages
  RiceInitiation = 1023
  RiceFlowering = 2028
  RiceMaturity = 2793
  
  #Give growth stages 0-3 and string
  table$GS = 0
  table$GSstring = "No growth"
  i = which(table$GDDsum > 0)[1]
  
  while (i <= nrow(table)){
    if(table$GDDsum[i] < RiceInitiation){
      table$GS[i] = 1
      table$GSstring[i] = "Sowing"
    } else if(table$GDDsum[i] >= RiceInitiation & table$GDDsum[i] < RiceFlowering){
      table$GS[i] = 2  
      table$GSstring[i] = "Initiation"
    } else if(table$GDDsum[i] >= RiceFlowering & table$GDDsum[i] < RiceMaturity){
      table$GS[i] = 3
      table$GSstring[i] = "Flowering"
    } else {
      table$GS[i] = 4
      table$GSstring[i] = "Maturity"
    }
    i = i + 1
  }
  return(table)
}

startdate = "2020-07-15"

table1 <- GDDsum_f2(dwns_Kmd, startdate)
table1 <- GSRice(table1)
table1$station <- "Kathmandu"
table1$crop <- "Rice"
table2 <- GDDsum_f2(dwns_Brp, startdate)
table2 <- GSRice(table2)
table2$station <- "Bharatpur"
table2$crop <- "Rice"
tableRice <- rbind(table1, table2)

##################################################
####### calculate growth stages (GS) Maize #######
##################################################
GSMaize <- function(table) {
  #Initial threshold values for growth stages
  MaizeInitiation = 642        #anthesis
  MaizeFlowering = 950
  MaizeMaturity = 1481
  
  #Give growth stages 0-3 and string
  table$GS = 0
  table$GSstring = "No growth"
  i = which(table$GDDsum > 0)[1]
  
  while (i <= nrow(table)){
    if(table$GDDsum[i] < MaizeInitiation){
      table$GS[i] = 1
      table$GSstring[i] = "Sowing"
    } else if(table$GDDsum[i] >= MaizeInitiation & table$GDDsum[i] < MaizeFlowering){
      table$GS[i] = 2 
      table$GSstring[i] = "Initiation"
    } else if(table$GDDsum[i] >= MaizeFlowering & table$GDDsum[i] < MaizeMaturity){
      table$GS[i] = 3
      table$GSstring[i] = "Flowering"
    } else {
      table$GS[i] = 4
      table$GSstring[i] = "Maturity"
    }
    i = i + 1
  }
  return(table)
}

startdate = "2020-04-01"

table1 <- GDDsum_f2(dwns_Kmd, startdate)
table1 <- GSMaize(table1)
table1$station <- "Kathmandu"
table1$crop <- "Maize"
table2 <- GDDsum_f2(dwns_Brp, startdate)
table2 <- GSMaize(table2)
table2$station <- "Bharatpur"
table2$crop <- "Maize"
tableMaize <- rbind(table1, table2)

##################################################
####### calculate growth stages (GS) Wheat #######
##################################################
GSWheat <- function(table) {
  #Initial threshold values for growth stages
  WheatInitiation = 203
  WheatFlowering = 701
  WheatMaturity = 1071
  
  #Give growth stages 0-3 and string
  table$GS = 0
  table$GSstring = "No growth"
  i = which(table$GDDsum > 0)[1]
  
  while (i <= nrow(table)){
    if(table$GDDsum[i] < WheatInitiation){
      table$GS[i] = 1
      table$GSstring[i] = "Sowing"
    } else if(table$GDDsum[i] >= WheatInitiation & table$GDDsum[i] < WheatFlowering){
      table$GS[i] = 2
      table$GSstring[i] = "Initiation"
    } else if(table$GDDsum[i] >= WheatFlowering & table$GDDsum[i] < WheatMaturity){
      table$GS[i] = 3
      table$GSstring[i] = "Flowering"
    } else {
      table$GS[i] = 4
      table$GSstring[i] = "Maturity"
    }
    i = i + 1
  }
  return(table)
}

startdate = "2020-01-01"

table1 <- GDDsum_f2(dwns_Kmd, startdate)
table1 <- GSWheat(table1)
table1$station <- "Kathmandu"
table1$crop <- "Wheat"
table2 <- GDDsum_f2(dwns_Brp, startdate)
table2 <- GSWheat(table2)
table2$station <- "Bharatpur"
table2$crop <- "Wheat"
tableWheat <- rbind(table1, table2)

# combine tables of wheat, rice and maize growth stage occurrence
cropCal <- rbind(tableRice, tableMaize, tableWheat)

################################################
### finding boundaries between growth stages ###
################################################
findGSchange <- function(table){
  table$Sowing = 0
  table$Initiation = 0
  table$Flowering = 0
  table$Maturity = 0
  table$Maturity.end = 0
  
  i = 2
  while (i <= nrow(table)){
    if (table$GS[i] > table$GS[i-1] & table$GSstring[i] == "Sowing"){
      table$Sowing[i] = table$days[i]
    } else if (table$GS[i] > table$GS[i-1] & table$GSstring[i] == "Initiation"){
      table$Initiation[i] = table$days[i]
    } else if (table$GS[i] > table$GS[i-1] & table$GSstring[i] == "Flowering"){
      table$Flowering[i] = table$days[i]
    } else if (table$GS[i] > table$GS[i-1] & table$GSstring[i] == "Maturity") {
      table$Maturity[i] = table$days[i]
    }
    i = i+1
  }
  
  table <- table[table$Sowing >0 | table$Initiation >0 | table$Flowering >0 | table$Maturity >0,]
  
  outcome <- table %>% dplyr::select(station, crop, Sowing, Initiation, Flowering, Maturity, Maturity.end)
  outcome <- outcome %>% group_by(station, crop) %>% summarize(Sowing = sum(Sowing),
                                                               Initiation = sum(Initiation),
                                                               Flowering = sum(Flowering),
                                                               Maturity = sum(Maturity),
                                                               Maturity.end = sum(Maturity) + 30)
  return(outcome)
}

GSdays <- findGSchange(cropCal)

### plotting
ggplot(GSdays, aes(x=crop)) +
  geom_linerange(aes(ymin=Sowing, ymax=Initiation, color="green"), size=15) +
  geom_linerange(aes(ymin=Initiation, ymax=Flowering, color="green3"), size=15) +
  geom_linerange(aes(ymin=Flowering, ymax=Maturity, color="yellow"), size=15) +
  geom_linerange(aes(ymin=Maturity, ymax=Maturity.end, colour="red"), size=15) +
  coord_flip() +
  facet_wrap(~station) +
  scale_y_continuous("", breaks = cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"),
                     limits = c(0, 365)) +
  ggtitle('Crop Calendar')+ xlab("") + ylab("") +
  scale_color_identity("",guide="legend",
                       labels=c("Sowing","Crop development","Harvesting","Mid-season")) +
  theme_bw() +
  theme(panel.grid.minor = element_line(colour = "white", size=0.75))
