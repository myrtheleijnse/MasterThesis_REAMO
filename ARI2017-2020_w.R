################### Weekly heatmap landslide risk vs. station specific inventory 2017-2020 #############

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveLandslides.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

##### Create ARI data frame #####
ARIdwns_295 <- ARI(dwns_295, dwns_295, "2017-01-01", "2020-12-31")
ARIdwns_346 <- ARI(dwns_346, dwns_346, "2017-01-01", "2020-12-31")
ARIdwns_347 <- ARI(dwns_347, dwns_347, "2017-01-01", "2020-12-31")
ARIdwns_349 <- ARI(dwns_349, dwns_349, "2017-01-01", "2020-12-31")
ARIMeteo_Spg_d <- ARI(dwns_Spg, dwns_Spg, "2017-01-01", "2020-12-31")
ARIdwns_Kmd <- ARI(dwns_Kmd, dwns_Kmd, "2017-01-01", "2020-12-31")
ARIdwns_Brp <- ARI(dwns_Brp, dwns_Brp, "2017-01-01", "2020-12-31")

################################
# function to find week numbers#
################################
getweeks <- function(table){
  table$week <- as.numeric(strftime(table$Timestamps, format = "%V"))
  table$year <- year(table$Timestamps)
  return(table)
}

##################################
# aggregate per week and         #
# sorting relevant columns only  #
##################################
sorting <- function(table){
  table <- getweeks(table)
  table <- table %>% dplyr::select(Timestamps, week, year, landsliderisk)
  table <- table %>% group_by(week, year) %>% summarize(landsliderisk = sum(landsliderisk))
  i = 1
  while (i <= nrow(table)){
    if (table$landsliderisk[i] >= 1){
      table$landsliderisk[i] = 1
    } 
    i = i+1
  }
  return(table)
}

ARIdwns_295 <- sorting(ARIdwns_295)
ARIdwns_349 <- sorting(ARIdwns_349)
ARIdwns_346 <- sorting(ARIdwns_346)
ARIdwns_347 <- sorting(ARIdwns_347)
ARIMeteo_Spg_d <- sorting(ARIMeteo_Spg_d)
ARIdwns_Kmd <- sorting(ARIdwns_Kmd)
ARIdwns_Brp <- sorting(ARIdwns_Brp)
inv295 <- sorting(inv295)
inv346 <- sorting(inv346)
inv347 <- sorting(inv347)
inv349 <- sorting(inv349)
invSpg <- sorting(invSpg)
invKmd <- sorting(invKmd)
invBrp <- sorting(invBrp)

#include station names
#filter specific northern stations
northern_stations <- station_locations[station_locations$Abbreviation == "Spg" | station_locations$Abbreviation == "Tmr" | 
                                         station_locations$Abbreviation == "Tmc" | station_locations$Abbreviation == "Dnc" | 
                                         station_locations$Abbreviation == "Psy",]
ARIdwns_295$station <- northern_stations$Name[2]
inv295$station <- "Inventory"
ARIdwns_346$station <- northern_stations$Name[3]
inv346$station <- "Inventory"
ARIdwns_347$station <- northern_stations$Name[4]
inv347$station <- "Inventory"
ARIdwns_349$station <- northern_stations$Name[5]
inv349$station <- "Inventory"
ARIMeteo_Spg_d$station <- northern_stations$Name[1]
invSpg$station <- "Inventory"
ARIdwns_Kmd$station <- station_locations$Name[1]
invKmd$station <- "Inventory"
ARIdwns_Brp$station <- station_locations$Name[3]
invBrp$station <- "Inventory"

#################################
# Make string of landslide risk #
#################################
landsliderisk.str <- function(table){
  table$landslideriskstr = "No risk"
  i=1
  while (i <= nrow(table)){
    if (table$landsliderisk[i] == 1){
      table$landslideriskstr[i] = "Landslide risk"
    }
    i = i+1  
  }
  return(table)
}
  
#combine tables and landsliderisk in string
ARI_295 <- landsliderisk.str(rbind(ARIdwns_295, inv295))
ARI_346 <- landsliderisk.str(rbind(ARIdwns_346, inv346))
ARI_347 <- landsliderisk.str(rbind(ARIdwns_347, inv347))
ARI_349 <- landsliderisk.str(rbind(ARIdwns_349, inv349))
ARI_Spg <- landsliderisk.str(rbind(ARIMeteo_Spg_d, invSpg))
ARI_Kmd <- landsliderisk.str(rbind(ARIdwns_Kmd, invKmd))
ARI_Brp <- landsliderisk.str(rbind(ARIdwns_Brp, invBrp))

# combine all stations into one df
ARI_all <- landsliderisk.str(rbind(ARI_295, ARI_346, ARI_347, ARI_349, ARI_Spg, ARI_Kmd, ARI_Brp))
#without inventory
ARI_all_1 <- ARI_all[ARI_all$station != "Inventory",]

### Plotting Kmd
ggplot() +
  geom_tile(data = ARI_Kmd_1, aes(year, week, fill = landslideriskstr), color = "white", size = 0.1) +
  scale_fill_manual(values = c("red", "springgreen4"), name=("Landslide risk")) +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,52,2)) +
  theme_minimal(base_size = 8) +
  labs(title= paste("Landslide risk per week"), x="Year", y="Week") +
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) +
  theme(legend.position = "bottom")

### Plotting all stations
ggplot() +
  geom_tile(data = ARI_all_1, aes(year, week, fill = landslideriskstr), color = "white", size = 0.1) +
  scale_fill_manual(values = c("red", "springgreen4"), name=("Landslide risk")) +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,52,2)) +
  theme_minimal(base_size = 8) +
  labs(title= paste("Landslide risk per week"), x="Year", y="Week") +
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) +
  theme(legend.position = "bottom")

##### True/False positives/negatives #####

######################################################################
############ function to compare ~station~ with inventory ############
######################################################################
skillscore <- function(tableobs, tableinv){
  #table <- tableobs %>% select(Timestamps, station)
  #table <- getleapdays(table)
  table <- tableobs
  table$skillscore = 0
  table$TN = 0
  table$FN = 0
  table$TP = 0
  table$FP = 0
  #identify True/False
  i=1
  while (i <= nrow(tableobs)) {
    if (tableobs$landsliderisk[i] == 0 & tableinv$landsliderisk[i] == 0) {
      table$skillscore[i] = "TN"                                                                  #True negative
      table$TN[i] = 1
      table$FN[i] = 0
      table$TP[i] = 0
      table$FP[i] = 0
    } else if (tableobs$landsliderisk[i] == 0 & tableinv$landsliderisk[i] == 1) {
      table$skillscore[i] = "FN"                                                                  #False negative
      table$FN[i] = 1
      table$TN[i] = 0
      table$TP[i] = 0
      table$FP[i] = 0
    } else if (tableobs$landsliderisk[i] == 1 & tableinv$landsliderisk[i] == 1) {
      table$skillscore[i] = "TP"                                                                  #True positive
      table$TP[i] = 1
      table$FN[i] = 0
      table$TN[i] = 0
      table$FP[i] = 0
    } else {
      table$skillscore[i] = "FP"                                                                  #False negative
      table$FP[i] = 1
      table$TN[i] = 0
      table$FN[i] = 0
      table$TP[i] = 0
    }
    i = i+1
  }
  return(table)
}

skillscore_df <- rbind(skillscore(ARIdwns_295, inv295), skillscore(ARIdwns_346, inv346), skillscore(ARIdwns_347, inv347),
                       skillscore(ARIdwns_349, inv349), skillscore(ARIMeteo_Spg_d, invSpg), skillscore(ARIdwns_Kmd, invKmd),
                       skillscore(ARIdwns_Brp, invBrp))

#########################################
############ sum skillscores ############
#########################################
sumskillscore <- function(table) {
  table <- table %>% group_by(station) %>% summarize(TN = sum(TN),
                                                     FN = sum(FN),
                                                     TP = sum(TP),
                                                     FP = sum(FP))
  table <- table %>% mutate(TNperc = TN/(TN+FN+TP+FP)*100,
                            FNperc = FN/(TN+FN+TP+FP)*100,
                            TPperc = TP/(TN+FN+TP+FP)*100,
                            FPperc = FP/(TN+FN+TP+FP)*100,
                            TPrate = TP/(TP+FN),
                            TNrate = TN/(TN+FP),
                            Precision = TP/(TP+FP),
                            Accuracy = (TP+TN)/(TP+TN+FP+FN))
  return(table)
}

# Skillscores table
sumskillscore_df <- sumskillscore(skillscore_df)
print(sumskillscore_df)

### Different colours in plot for TN-FN-TP-TP

ggplot() +
  geom_tile(data = skillscore_df, aes(year, week, fill = skillscore), color = "white", size = 0.1) +
  scale_fill_manual(values = c("tomato1", "tomato4", "mediumseagreen", "mediumspringgreen"), name=("Classifier"), 
                    labels = c("False negative", "False positive", "True negative", "True positive")) +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,52,5)) +
  theme_minimal(base_size = 8) +
  labs(title= paste("Landslide risk per week"), x="Year", y="Week") +
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) +
  theme(legend.position = "bottom") +
  theme(text=element_text(size=15))
