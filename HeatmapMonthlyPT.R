############### Heatmap precipitation and temperature anomalies ###############

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

###################################################
### Relative change in monthly T and P per year ###
###################################################
relPTchange <- function(table, stationname){
  table = getdays(table)
  table = table[table$year >= 2000,]                                                  #subset year >= 2000
  #calculate monthly summed P and mean T
  table = table %>% group_by(month, year) %>% summarize(Temperature = mean(Temperature),
                                                        prec = sum(prec))
  #calculate mean monthly P and T over all years
  table2 = table %>% group_by(month) %>% summarize(Tmean = mean(Temperature),
                                                   Pmean = mean(prec))
  table3 <- table2
  #repeat monthly average for all years
  for (i in 1:21) {table3[i,] <- table2[1,]}
  for (i in 22:42) {table3[i,] <- table2[2,]}
  for (i in 43:63) {table3[i,] <- table2[3,]}
  for (i in 64:84) {table3[i,] <- table2[4,]}
  for (i in 85:105) {table3[i,] <- table2[5,]}
  for (i in 106:126) {table3[i,] <- table2[6,]}
  for (i in 127:147) {table3[i,] <- table2[7,]}
  for (i in 148:168) {table3[i,] <- table2[8,]}
  for (i in 169:189) {table3[i,] <- table2[9,]}
  for (i in 190:210) {table3[i,] <- table2[10,]}
  for (i in 211:231) {table3[i,] <- table2[11,]}
  for (i in 232:252) {table3[i,] <- table2[12,]}
  table <- cbind(table$year, table$month, table$Temperature, table$prec, table3$Tmean, table3$Pmean)
  colnames(table) <- c("year", "month", "Temperature", "prec", "Tmean", "Pmean")
  table <- data.frame(table)
  
  #calculate relative and absolute change in P and T
  table <- table %>% mutate(Trel = (Temperature - Tmean)/Tmean*100,
                            Prel = (prec - Pmean)/Pmean*100,
                            Tabs = Temperature - Tmean,
                            Pabs = prec - Pmean
  )
  
  #remove extremely high P anomalies > 100%
  i = 1
  while (i <= nrow(table)){
    if (table$Prel[i] >= 100) {
      table$Prel[i] = 100
    } 
    i=i+1
  }
  
  table$station = stationname
  
  return(table)
}

table1 = relPTchange(dwns_Kmd, "Kathmandu")
table2 = relPTchange(dwns_Brp, "Bharatpur")
table = rbind(table1, table2)

#heatmap relative change in T (anomalies)
ggplot() +
  geom_tile(data = table, aes(month, year, fill = Trel), color = "white", size = 0.1) +
  scale_fill_distiller(name = paste0("Change", "\n", "in T (%)"), palette = "Spectral") +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(2000,2020,1)) +
  scale_x_continuous(breaks = seq(1,12,1), labels = c("Jan","Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                      "Nov", "Dec")) +
  ggtitle("Monthly temperature anomalies (relative)") +
  theme_minimal() +  
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10))

#heatmap absolute change in T (anomalies)
ggplot() +
  geom_tile(data = table, aes(month, year, fill = Tabs), color = "white", size = 0.1) +
  scale_fill_distiller(name = paste0("Change", "\n", "in T (°C)"), palette = "Spectral", 
                       limits = c(-max(table$Tabs), max(table$Tabs))) +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(2000,2020,1)) +
  scale_x_continuous(breaks = seq(1,12,1), labels = c("Jan","Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                                                      "Nov", "Dec")) +
  ggtitle("Monthly temperature anomalies (absolute)") +
  theme_minimal() +  
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) 

#heatmap relative change in P (anomalies)
ggplot() +
  geom_tile(data = table, aes(month, year, fill = Prel), color = "white", size = 0.1) +
  scale_fill_distiller(name = paste0("Change", "\n", "in P (%)"), palette = "Spectral", direction = 1) +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(2000,2020,1)) +
  scale_x_continuous(breaks = seq(1,12,1), labels = c("Jan","Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct", 
                                                      "Nov", "Dec"))  +
  ggtitle("Monthly precipitation anomalies (relative)") +
  theme_minimal() +  
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10))

#heatmap absolute change in P (anomalies)
ggplot() +
  geom_tile(data = table, aes(month, year, fill = Pabs), color = "white", size = 0.1) +
  scale_fill_distiller(name = paste0("Change", "\n", "in P (mm)"), palette = "Spectral", direction = 1, 
                       limits=c(-max(table$Pabs),max(table$Pabs))) +
  facet_wrap(~station) +
  scale_y_continuous(trans = "reverse", breaks = seq(2000,2020,1)) +
  scale_x_continuous(breaks = seq(1,12,1), labels = c("Jan","Feb", "Mar", "Apr","May", "Jun", "Jul", "Aug", "Sep", "Oct",
                                                      "Nov", "Dec")) +
  ggtitle("Monthly precipitation anomalies (absolute)") +
  theme_minimal() +  
  theme(plot.title=element_text(size = 14))+
  theme(axis.text.y=element_text(size=10)) +
  theme(strip.background = element_rect(colour="white"))+
  theme(plot.title=element_text(hjust=0))+
  theme(axis.ticks=element_blank())+
  theme(axis.text=element_text(size=10))+
  theme(legend.title=element_text(size=12))+
  theme(legend.text=element_text(size=10)) 
