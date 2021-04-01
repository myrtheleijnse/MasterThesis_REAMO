###################################### Landslide inventories and ARI ######################################

#set workdirectory
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Reading_data_vFinal") #alter to own working directory

#reading inventories per station 2017-2020
invKmd <- read.csv("Inventories per station/Kmd2017-2020.csv", sep = ";")
invKmd$Timestamps <- strptime(invKmd$Timestamps, "%d-%m-%Y")
invKmd$Timestamps <- as.POSIXct(invKmd$Timestamps)
invKmd <- invKmd %>% dplyr::select(Timestamps, landsliderisk)
invBrp <- read.csv("Inventories per station/Brp2017-2020.csv", sep = ";")
invBrp$Timestamps <- strptime(invBrp$Timestamps, "%d-%m-%Y")
invBrp$Timestamps <- as.POSIXct(invBrp$Timestamps)
invBrp <- invBrp %>% dplyr::select(Timestamps, landsliderisk)
inv349 <- read.csv("Inventories per station/3492017-2020.csv", sep = ";")
inv349$Timestamps <- strptime(inv349$Timestamps, "%d-%m-%Y")
inv349$Timestamps <- as.POSIXct(inv349$Timestamps)
inv349 <- inv349 %>% dplyr::select(Timestamps, landsliderisk)
invSpg <- read.csv("Inventories per station/Spg2017-2020.csv", sep = ";")
invSpg$Timestamps <- strptime(invSpg$Timestamps, "%d-%m-%Y")
invSpg$Timestamps <- as.POSIXct(invSpg$Timestamps)
invSpg <- invSpg %>% dplyr::select(Timestamps, landsliderisk)
inv295 <- read.csv("Inventories per station/2952017-2020.csv", sep = ";")
inv295$Timestamps <- strptime(inv295$Timestamps, "%d-%m-%Y")
inv295$Timestamps <- as.POSIXct(inv295$Timestamps)
inv295 <- inv295 %>% dplyr::select(Timestamps, landsliderisk)
inv347 <- read.csv("Inventories per station/3472017-2020.csv", sep = ";")
inv347$Timestamps <- strptime(inv347$Timestamps, "%d-%m-%Y")
inv347$Timestamps <- as.POSIXct(inv347$Timestamps)
inv347 <- inv347 %>% dplyr::select(Timestamps, landsliderisk)
inv346 <- read.csv("Inventories per station/3462017-2020.csv", sep = ";")
inv346$Timestamps <- strptime(inv346$Timestamps, "%d-%m-%Y")
inv346$Timestamps <- as.POSIXct(inv346$Timestamps)
inv346 <- inv346 %>% dplyr::select(Timestamps, landsliderisk)

#reading inventory over entire study area 2017-2020
inventory2017 <- read.csv("LandslideInventory2017-2020R.csv", sep = ";")
inventory2017$Timestamps <- strptime(inventory2017$Timestamps, "%d-%m-%Y")
inventory2017$Timestamps <- as.POSIXct(inventory2017$Timestamps)
inventory2017 <- inventory2017 %>% dplyr::select(Timestamps, landsliderisk)
inventory2017 <- inventory2017[inventory2017$Timestamps >= "2017-01-01",]

#############################################
### Antecedent Rainfall Index calculation ###
#############################################
ARI <- function(table, downscaledt, startdate, enddate) {
  #rename precipitation column
  table <- renametoprec(table)
  downscaledt <- renametoprec(downscaledt)
  
  #subset table
  table <- table[table$Timestamps >= startdate & table$Timestamps <= enddate,]
  
  #set initials
  table$ARI = 0
  table$landsliderisk = 0
  i = 1
  
  #loop calculating ARI for precipitation of table
  while (i < nrow(table)) {
    threshold = quantile(downscaledt$prec, 0.95)          #95th percentile threshold over entire downscaled period 1979-2020
    sumw = 7^-2+6^-2+5^-2+4^-2+3^-2+2^-2+1^-2             #sum of weight
    
    #Calc ARI
    if (i == 1) {                                         #correct for first 6 time steps
      p1 = table$prec[i] * (1)^-2
      table$ARI[i] <- sum(p1)/(sumw - 7^-2+6^-2+5^-2+4^-2+3^-2+2^-2)
    } else if (i == 2) { 
      p1 = table$prec[i] * (1)^-2
      p2 = table$prec[i-1] * (2)^-2
      table$ARI[i] <- sum(p1,p2)/(sumw - 7^-2+6^-2+5^-2+4^-2+3^-2)
    } else if (i == 3) { 
      p1 = table$prec[i] * (1)^-2
      p2 = table$prec[i-1] * (2)^-2
      p3 = table$prec[i-2] * (3)^-2
      table$ARI[i] <- sum(p1,p2,p3)/(sumw - 7^-2+6^-2+5^-2+4^-2)
    } else if (i == 4) { 
      p1 = table$prec[i] * (1)^-2
      p2 = table$prec[i-1] * (2)^-2
      p3 = table$prec[i-2] * (3)^-2
      p4 = table$prec[i-3] * (4)^-2
      table$ARI[i] <- sum(p1,p2,p3,p4)/(sumw - 7^-2+6^-2+5^-2)
    } else if (i == 5) { 
      p1 = table$prec[i] * (1)^-2
      p2 = table$prec[i-1] * (2)^-2
      p3 = table$prec[i-2] * (3)^-2
      p4 = table$prec[i-3] * (4)^-2
      p5 = table$prec[i-4] * (5)^-2
      table$ARI[i] <- sum(p1,p2,p3,p4,p5)/(sumw - 7^-2+6^-2)
    } else if (i == 6) {
      p1 = table$prec[i] * (1)^-2
      p2 = table$prec[i-1] * (2)^-2
      p3 = table$prec[i-2] * (3)^-2
      p4 = table$prec[i-3] * (4)^-2
      p5 = table$prec[i-4] * (5)^-2
      p6 = table$prec[i-5] * (6)^-2
      table$ARI[i] <- sum(p1,p2,p3,p4,p5,p6)/(sumw - 7^-2)
    } else { 
      p1 = table$prec[i] * (1)^-2
      p2 = table$prec[i-1] * (2)^-2
      p3 = table$prec[i-2] * (3)^-2
      p4 = table$prec[i-3] * (4)^-2
      p5 = table$prec[i-4] * (5)^-2
      p6 = table$prec[i-5] * (6)^-2
      p7 = table$prec[i-6] * (7)^-2
      table$ARI[i] <- sum(p1,p2,p3,p4,p5,p6,p7)/sumw
    }
    #Calculate landslide risk
    if (table$ARI[i] >= threshold) {
      table$landsliderisk[i] = 1
    }
    i = i+1
  }
  
  #Change significance of ARI
  table$ARI <- lapply(table$ARI, round, 3)
  table$ARI <- as.numeric(table$ARI)
  table <- na.omit(table)
  
  return(table)
}
