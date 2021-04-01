######################## Lollipop max P intensity ########################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveLandslides.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

# use hourly data only
#####################################
# daily max precipitation intensity #
#####################################
dailyprec <- function(table){  
  table$year = year(table$Timestamps)
  table$month = month(table$Timestamps)
  table$day = as.numeric(format(as.Date(table$Timestamps), "%d"))
  table$days <- as.numeric(strftime(table$Timestamps, format = "%j"))
  table <- renametoprec(table)
  table <- table[table$year >= 2017 & table$year <= 2020,]
  table <- na.omit(table)
  table <- table %>% group_by(station, year, days) %>% summarize(prec = max(prec))
  return(table)
}

#Daily max P intensity for 2020
dwns_Kmd_h$station = "Kathmandu"
dwns_Brp_h$station = "Bharatpur"
df_Kmd <- dailyprec(dwns_Kmd_h)
df_Brp <- dailyprec(dwns_Brp_h)

df_Kmd <- data.frame(df_Kmd)
df_Brp <- data.frame(df_Brp)

#combine daily max P intensity and inventory
df_Kmd <- cbind(invKmd, df_Kmd)
df_Brp <- cbind(invBrp, df_Brp)

#determine landslide occurrence
landsliderisk <- function(table){
  i = 1
  landslideriskstr = 0        #String landslide occurrence
  while (i <= nrow(table)){
    if (table$landsliderisk[i] == 1){
      table$landslideriskstr[i] = "Landslide"
    } else {
      table$landslideriskstr[i] = "No landslide"
    }
    i = i+1
  }
  return(table)
}

df_Kmd <- landsliderisk(df_Kmd)
df_Brp <- landsliderisk(df_Brp)

#lollipop plot Kmd
ggplot() + 
  geom_point(data = df_Kmd, aes(days, prec, colour = landslideriskstr)) + 
  geom_segment(data=df_Kmd, aes(x=days, xend=days, y=0, yend=prec, colour = landslideriskstr)) +
  facet_wrap(station~year) +
  scale_colour_manual(name = "Landslide occurrence:", values = c("red", "black")) +
  scale_x_continuous(breaks = cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"),
                     limits = c(0, 365)) +
  xlab("")+
  ylab("Daily precipitation intensity (mm/h)")+
  ggtitle("Daily P intensity and landslide occurrence") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(text=element_text(size=15))

#lollipop plot Brp
ggplot() + 
  geom_point(data = df_Brp, aes(days, prec, colour = landslideriskstr)) + 
  geom_segment(data=df_Brp, aes(x=days, xend=days, y=0, yend=prec, colour = landslideriskstr)) +
  facet_wrap(station~year) +
  scale_colour_manual(name = "Landslide occurrence:", values = c("red", "black")) +
  scale_x_continuous(breaks = cumsum(c(0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)), 
                     labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan"),
                     limits = c(0, 365)) +
  xlab("")+
  ylab("Daily precipitation intensity (mm/h)")+
  ggtitle("Daily P intensity and landslide occurrence") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  theme(text=element_text(size=15))