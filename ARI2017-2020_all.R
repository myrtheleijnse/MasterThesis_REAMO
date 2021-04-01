#################### Heatmap landslide risk vs. inventory 2017-2020 ##################

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

#############################
#sort dataframes into one df#
#############################
sorting <- function(table){
  table <- table %>% dplyr::select(Timestamps, landsliderisk)
  return(table)
}

ARIdwns_295 <- sorting(ARIdwns_295)
ARIdwns_346 <- sorting(ARIdwns_346)
ARIdwns_347 <- sorting(ARIdwns_347)
ARIdwns_349 <- sorting(ARIdwns_349)
ARIMeteo_Spg_d <- sorting(ARIMeteo_Spg_d)
ARIdwns_Kmd <- sorting(ARIdwns_Kmd)
ARIdwns_Brp <- sorting(ARIdwns_Brp)

#filter specific northern stations
northern_stations <- station_locations[station_locations$Abbreviation == "Spg" | station_locations$Abbreviation == "Tmr" | 
                                         station_locations$Abbreviation == "Tmc" | station_locations$Abbreviation == "Dnc" | 
                                         station_locations$Abbreviation == "Psy",]
ARIdwns_295$station <- northern_stations$Name[2]
ARIdwns_346$station <- northern_stations$Name[3]
ARIdwns_347$station <- northern_stations$Name[4]
ARIdwns_349$station <- northern_stations$Name[5]
ARIMeteo_Spg_d$station <- northern_stations$Name[1]
ARIdwns_Kmd$station <- station_locations$Name[1]
ARIdwns_Brp$station <- station_locations$Name[3]
inventory2017$station <- "Inventory"

ARI_df <- rbind(ARIdwns_295, ARIdwns_346, ARIdwns_347, ARIdwns_349, ARIMeteo_Spg_d, ARIdwns_Kmd, ARIdwns_Brp)

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

ARI_df <- landsliderisk.str(getleapdays(ARI_df))

##### Heatmapping landslide risk #####
ggplot() +
  geom_tile(data = ARI_df, aes(month, day, fill = landslideriskstr), color = "white", size = 0.1) +
  scale_fill_manual(values = c("red", "springgreen4"), name=("Landslide risk")) +
  facet_wrap(year~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,31,2)) +
  scale_x_continuous(breaks = seq(1,12,1), labels = c("jan","feb", "mar", "apr","may", "jun", "jul", "aug", "sep", "okt", 
                                                      "nov", "dec")) +
  theme_minimal(base_size = 8) +
  labs(title= paste("Landslide risk per day"), x="Month", y="Day") +
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) +
  theme(legend.position = "bottom")

### count amount of landslide risks estimated per station
totallandslides <- ARI_df %>% group_by(station) %>% summarize(total = sum(landsliderisk))
print(totallandslides)

##### True/False positives/negatives #####

######################################################################
############ function to compare ~station~ with inventory ############
######################################################################
skillscore <- function(tableobs, tableinv){
  table <- tableobs %>% select(Timestamps, station)
  table <- getleapdays(table)
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

skillscore_df <- rbind(skillscore(ARIdwns_295, inventory2017), skillscore(ARIdwns_346, inventory2017), 
                       skillscore(ARIdwns_347, inventory2017), skillscore(ARIdwns_349, inventory2017), 
                       skillscore(ARIMeteo_Spg_d, inventory2017), skillscore(ARIdwns_Kmd, inventory2017),
                       skillscore(ARIdwns_Brp, inventory2017))

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

# Percentages of skillscores table
sumskillscore_df <- sumskillscore(skillscore_df)
print(sumskillscore_df)

# rearrange table for barplot
dfTN <- sumskillscore_df %>% select(station, TNperc)
colnames(dfTN)[colnames(dfTN) == "TNperc"] <- "skillscore"
dfTN$type <- "TN"
dfFN <- sumskillscore_df %>% select(station, FNperc)
colnames(dfFN)[colnames(dfFN) == "FNperc"] <- "skillscore"
dfFN$type <- "FN"
dfTP <- sumskillscore_df %>% select(station, TPperc)
colnames(dfTP)[colnames(dfTP) == "TPperc"] <- "skillscore"
dfTP$type <- "TP"
dfFP <- sumskillscore_df %>% select(station, FPperc)
colnames(dfFP)[colnames(dfFP) == "FPperc"] <- "skillscore"
dfFP$type <- "FP"

percskillscore_df <- rbind(dfTN, dfFN, dfTP, dfFP)

##### Barplot skillscore landslides #####
ggplot() +
  geom_bar(data = percskillscore_df, aes(station, skillscore, fill=type), col = "black", stat = "identity", position = "dodge", 
           alpha = 0.8) +
  scale_fill_manual(values = c("red", "red3", "green3", "green4"), name = "", label = c("False negative", "False positive", "True negative", "True positive")) +
  labs(y="Skillscore (%)", x = "Station") +
  ggtitle("Performance of landslide prediction per station") +
  theme_minimal() +
  theme(legend.position = "bottom")

### Different colours in plot for TN-FN-TP-TP

ggplot() +
  geom_tile(data = skillscore_df, aes(month, day, fill = skillscore), color = "white", size = 0.1) +
  scale_fill_manual(values = c("tomato1", "tomato4", "mediumseagreen", "mediumspringgreen"), name=("Classifier"), 
                    labels = c("False negative", "False positive", "True negative", "True positive")) +
  facet_wrap(station~year) +
  scale_y_continuous(trans = "reverse", breaks = seq(1,31,2)) +
  scale_x_continuous(breaks = seq(1,12,1), labels = c("jan","feb", "mar", "apr","may", "jun", "jul", "aug", "sep", "okt", 
                                                      "nov", "dec")) +
  theme_minimal(base_size = 8) +
  labs(title= paste("Landslide risk per day"), x="Month", y="Day") +
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) +
  theme(legend.position = "bottom")
