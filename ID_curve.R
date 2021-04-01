##################### ID curve #####################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveLandslides.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ARI2017-2020.R")

#works only for Brp and Kmd stations due to availability of high temporal resolution (hourly)

### Match date to landslide risk (from inventory)

###############################
# Obtain dates with landslide #
###############################
matchlandslides <- function(table, inventory){
  landslidedates <- inventory[inventory$landslideriskstr == "Landslide risk",]  #subset dates with landslide
  table$Date <- strptime(table$Timestamps, "%Y-%m-%d")                          #date without hour
  table$Date <- as.POSIXct(table$Date)
  table <- table[table$Date >= "2017-01-01",]                                   #subset 2017 onwards
  
  #loop matching hourly measurements to date of landslide occurrence
  i = 1
  table$landsliderisk = "No risk"
  while (i <= nrow(table)) {
    if (is.element(table$Date[i], landslidedates$Timestamps)){
      table$landsliderisk[i] = "Landslide"
    }
    i=i+1
  } 
  
  return(table)
}

Kmd_landslides <- matchlandslides(dwns_Kmd_h, landsliderisk.str(invKmd))
Kmd_landslides$station <- "Kathmandu"
Brp_landslides <- matchlandslides(dwns_Brp_h, landsliderisk.str(invBrp))
Brp_landslides$station <- "Bharatpur"

#########################################################################################
### Calculate duration and intensity (I and D) of landside-triggering rainfall events ###
#########################################################################################
ID <- function(table){
  # maximum precipitation per day 
  table <- table %>% group_by(Date) %>% mutate(maxprec = max(prec))
  
  ### Loop calculating hour of landslide occurrence (assuming highest preciptiation intensity has triggered landslide)
  i=1
  table$landsliderisk.hour = "No landslide"
  while (i <= nrow(table)) {
    if(table$prec[i] == table$maxprec[i] & table$landsliderisk[i] == "Landslide"){
      table$landsliderisk.hour[i] = "Landslide"
    }
    i=i+1
  }
  
  ### Loop calculating duration (h) of precipitation event that triggers landslide
  # And calculating average intensity (mm/h) of event
  i = 1
  table$duration = 0                  #duration of rainfall event [h]
  table$cum.intensity = 0             #accumulation of intensity during rainfall event [mm/h]
  table$intensity = 0                 #mean intensity of rainfall event [mm/h]
  while(i <= nrow(table)) {
    if (table$landsliderisk.hour[i] == "Landslide"){
      table$cum.intensity[i] = table$prec[i]
      j = i+1
      while (table$prec[j] > 0 ){
        table$cum.intensity[j] = table$cum.intensity[j-1] + table$prec[j]
        j = j+1
      }
      table$duration[i] = j-i
      if(table$duration[i] > 0){
        table$intensity[i] = table$cum.intensity[j-1]/table$duration[i]
      }
    }
    i=i+1
  }
  
  ### Subset hours with landslide occurrence
  output <- table[table$landsliderisk.hour == "Landslide",]
  
  # only unique days
  i = 1
  while (i < nrow(output)){
    if (output$Date[i] == 	output$Date[i+1]){
      output$duration[i+1] = 0
    }
    i = i+1
  }
  
  output <- output[output$duration > 0,]
  
  return(output)
}
  
table <- rbind(ID(Brp_landslides), ID(Kmd_landslides))

### Plotting Intensity-duration per landslide event

#Intensity-duration point plot (mean intensity)
ggplot(table) +
  geom_point(aes(duration, intensity)) +
  labs(title= paste("Intensity and duration of landslide-triggering rainfall events"), x="Duration (h)", y="Intensity (mm/h)") +
  theme_minimal()

# Rainfall threshold Dahal and Hasegawa (2008) in Nepal
#I = 73.90D^-0.79

#mean intensity ID curve
ggplot(table) +
  geom_line(aes(duration, 73.9*duration^(-0.79)), col = "red", lwd = 1.5) +
  geom_point(aes(duration, intensity)) +
  labs(title= paste("ID-curve"), x="Duration (h)", y="Intensity (mm/h)") +
  annotate("text", min(table$duration), max(table$prec), vjust = -1, hjust = -0.2, 
           label = expression(paste(bold("I = 73.90D"^"-0.79"))), col = "red")+
  theme_minimal()
