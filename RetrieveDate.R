############################### Retrieve year, month, day from timestamps #####################################

####################################
# date-function without leap years #
####################################
getdays <- function(table) {
  table$year = year(table$Timestamps)
  table$month = month(table$Timestamps)
  table$day = as.numeric(format(as.Date(table$Timestamps), "%d"))
  #accumulated days per year
  table$days <- as.numeric(strftime(table$Timestamps, format = "%j"))  
  
  #correction for the day starting at 31st
  i = 1
  while (i < nrow(table)){
    table$day[i] = table$day[i+1]
    i = i + 1
  }
  
  #remove leap years
  table <- table[!(table$day == 29 & table$month ==2),]
  
  i = 1
  while (i <= nrow(table)) {
    if((table$year[i] == 1980 | table$year[i] == 1984 | table$year[i] == 1988 | table$year[i] == 1992 | table$year[i] == 1996 | 
        table$year[i] == 2000 |
        table$year[i] == 2004 | table$year[i] == 2008 | table$year[i] == 2012 | table$year[i] == 2016 | table$year[i] == 2020) &
       table$days[i] > 60) {
      table$days[i] = table$days[i] - 1
    }
    i = i + 1
  }
  
  return(table)
}

#################################
# date-function with leap years #
#################################
getleapdays <- function(table) {
  table$year = year(table$Timestamps)
  table$month = month(table$Timestamps)
  table$day = as.numeric(format(as.Date(table$Timestamps), "%d"))
  #accumulated days per year
  table$days <- as.numeric(strftime(table$Timestamps, format = "%j")) 
  
  #correction for the day starting at 31st
  i = 1
  while (i < nrow(table)){
    table$day[i] = table$day[i+1]
    i = i + 1
  }

  return(table)
}

#####################################################
# Rename any daily precipitation variable to "prec" #
#####################################################
renametoprec <- function(table){
  colnames(table)[colnames(table) == "rainfallsum"] <- "prec"
  colnames(table)[colnames(table) == "Prec"] <- "prec"
  colnames(table)[colnames(table) == "Precipitation"] <- "prec"
  return(table)
}