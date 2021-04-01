########################################### Cumulative GDD ###########################################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

#southern stations: Kmd and Brp
#based on full dwns series

#GDD thresholds
RiceInitiation = 1023
RiceFlowering = 2028
RiceMaturity = 2793
WheatInitiation = 203
WheatFlowering = 701
WheatMaturity = 1071
MaizeInitiation = 642
MaizeFlowering = 950                 #anthesis
MaizeMaturity = 1481

#################################################################
####### calculate GDDsum anomalies and 2020 for dwns data #######
#################################################################
GDDsum_f <- function(table, startmonth, endmonth, startyear, startdays, startdate) {
  table <- getdays(table)
  table <- table[table$year >= startyear & table$month >= startmonth & table$month <= endmonth & table$days >= startdays,]
  
  #get data.table frame
  require(data.table)
  table <- data.table(table)
  
  #calculate cumulative GDD per year
  table[, GDDsum := cumsum(Temperature), by = list(year)]
  
  #calculate mean and 10-90th percentiles
  table[, GDDsumhigh:= quantile(GDDsum, 0.9), by = list(days)]
  table[, GDDsumlow:= quantile(GDDsum, 0.1), by = list(days)]
  table[, GDDsummean:= mean(GDDsum), by = list(days)]
  
  table <- table[table$Timestamps >= startdate,]
  
  return(table)
}

################################################################
### find day where GDDsum is equal to growth stage threshold ###
################################################################
thresholdday <- function(table, threshold){ 
  output = 0
  i = 1
  while (i<=nrow(table)){
    if(table$GDDsum[i] >= threshold){
      output = table$days[i]
      i = nrow(table)
    }
    i = i+1
  }
  return(output)
}

############################################################# RICE ############################################################

#rice initial dates
startmonth = 7            
endmonth = 11
startyear = 2000
startdate = "2020-07-15"
startdays = as.numeric(strftime(startdate, format = "%j"))

#run GDDsum function
table <- GDDsum_f(dwns_Kmd, startmonth, endmonth, startyear, startdays, startdate)
table$station = "Kathmandu"
table2 <- GDDsum_f(dwns_Brp, startmonth, endmonth, startyear, startdays, startdate)
table2$station = "Bharatpur"
table <- rbind(table, table2)

#plotting
coeff <- 30           #scaling coefficient for precipitation

ggplot() +
  geom_bar(data = table, aes(x=days, y=prec*coeff, fill = "Precipitation"), stat="identity", alpha = 0.6) +
  geom_ribbon(data = table, aes(x=days, ymin = GDDsumlow, ymax = GDDsumhigh, 
                                fill = paste0("10-90th percentile" , "\n", "range (2000-2020)")), alpha = 0.5) +
  geom_line(data = table, aes(days, GDDsummean, col = "red"), lwd = 1.5) +
  geom_line(data = table, aes(days, GDDsum, col = "black"), lwd = 1.5) +
  geom_segment(data = table, aes(x = days[1], y = RiceInitiation, xend = thresholdday(table, RiceInitiation), 
                                 yend = RiceInitiation), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = days[1], y = RiceFlowering, xend = thresholdday(table, RiceFlowering), 
                                 yend = RiceFlowering), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = days[1], y = RiceMaturity, xend = thresholdday(table, RiceMaturity), 
                                 yend = RiceMaturity), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = thresholdday(table, RiceInitiation), y = 0, xend = thresholdday(table, RiceInitiation), 
                                 yend = RiceInitiation), lty=5, lwd = 1, alpha = 0.2) +
  geom_segment(data = table, aes(x = thresholdday(table, RiceFlowering), y = 0, xend = thresholdday(table, RiceFlowering), 
                                 yend = RiceFlowering), lty=5, lwd = 1, alpha = 0.2) +
  geom_segment(data = table, aes(x = thresholdday(table, RiceMaturity), y = 0, xend = thresholdday(table, RiceMaturity), 
                                 yend = RiceMaturity), lty=5, lwd = 1, alpha = 0.2) +
  facet_wrap(~station) +
  ggtitle("Accumulated GDD", paste(" Date of sowing:", startdate, "\n", "Crop type: Rice")) +
  labs(x = "Time (months)", y = "Cumulative GDD (°C)") + 
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name = "Cumulative GDD (°C)", sec.axis = sec_axis(~./coeff, name="Precipitation (mm/day)")) +
  scale_color_manual(name = "", labels = c("2020", "Mean 2000-2020"), 
                     values = c("black", "red")) +
  scale_fill_manual(name = "Legend", values = c("grey70", "steelblue")) +
  annotate("text", min(table$days), RiceInitiation, vjust = -1, hjust = 0, label = "Initiation") +
  annotate("text", min(table$days), RiceFlowering, vjust = -1, hjust = 0, label = "Flowering") +
  annotate("text", min(table$days), RiceMaturity, vjust = -1, hjust = 0, label = "Maturity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 13), axis.title.y.right = element_text(colour = "steelblue", size = 13), 
        axis.text.y.right = element_text(color = "steelblue"))

############################################################# Maize ############################################################

#maize initial dates
startmonth = 4            
endmonth = 6
startyear = 2000
startdate = "2020-04-01"
startdays = as.numeric(strftime(startdate, format = "%j"))

#run GDDsum function
table <- GDDsum_f(dwns_Kmd, startmonth, endmonth, startyear, startdays, startdate)
table$station = "Kathmandu"
table2 <- GDDsum_f(dwns_Brp, startmonth, endmonth, startyear, startdays, startdate)
table2$station = "Bharatpur"
table <- rbind(table, table2)

#plotting
coeff <- 30           #scaling coefficient for precipitation

ggplot() +
  geom_bar(data = table, aes(x=days, y=prec*coeff, fill = "Precipitation"), stat="identity", alpha = 0.6) +
  geom_ribbon(data = table, aes(x=days, ymin = GDDsumlow, ymax = GDDsumhigh, 
                                fill = paste0("10-90th percentile" , "\n", "range (2000-2020)")), alpha = 0.5) +
  geom_line(data = table, aes(days, GDDsummean, col = "red"), lwd = 1.5) +
  geom_line(data = table, aes(days, GDDsum, col = "black"), lwd = 1.5) +
  geom_segment(data = table, aes(x = days[1], y = MaizeInitiation, xend = thresholdday(table, MaizeInitiation), 
                                 yend = MaizeInitiation), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = days[1], y = MaizeFlowering, xend = thresholdday(table, MaizeFlowering), 
                                 yend = MaizeFlowering), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = days[1], y = MaizeMaturity, xend = thresholdday(table, MaizeMaturity), 
                                 yend = MaizeMaturity), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = thresholdday(table, MaizeInitiation), y = 0, xend = thresholdday(table, MaizeInitiation), 
                                 yend = MaizeInitiation), lty=5, lwd = 1, alpha = 0.2) +
  geom_segment(data = table, aes(x = thresholdday(table, MaizeFlowering), y = 0, xend = thresholdday(table, MaizeFlowering), 
                                 yend = MaizeFlowering), lty=5, lwd = 1, alpha = 0.2) +
  geom_segment(data = table, aes(x = thresholdday(table, MaizeMaturity), y = 0, xend = thresholdday(table, MaizeMaturity), 
                                 yend = MaizeMaturity), lty=5, lwd = 1, alpha = 0.2) +
  facet_wrap(~station) +
  ggtitle("Accumulated GDD", paste(" Date of sowing:", startdate, "\n", "Crop type: Maize")) +
  labs(x = "Time (months)", y = "Cumulative GDD (°C)") + 
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name = "Cumulative GDD (°C)", sec.axis = sec_axis(~./coeff, name="Precipitation (mm/day)")) +
  scale_color_manual(name = "", labels = c("2020", "Mean 2000-2020"), 
                     values = c("black", "red")) +
  scale_fill_manual(name = "Legend", values = c("grey70", "steelblue")) +
  annotate("text", min(table$days), MaizeInitiation, vjust = -1, hjust = 0, label = "Initiation") +
  annotate("text", min(table$days), MaizeFlowering, vjust = -1, hjust = 0, label = "Flowering") +
  annotate("text", min(table$days), MaizeMaturity, vjust = -1, hjust = 0, label = "Maturity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 13), axis.title.y.right = element_text(colour = "steelblue", size = 13), 
        axis.text.y.right = element_text(color = "steelblue"))

############################################################# Maize ############################################################

#wheat initial dates
startmonth = 1            
endmonth = 3
startyear = 2000
startdate = "2020-01-01"
startdays = as.numeric(strftime(startdate, format = "%j"))

#run GDDsum function
table <- GDDsum_f(dwns_Kmd, startmonth, endmonth, startyear, startdays, startdate)
table$station = "Kathmandu"
table2 <- GDDsum_f(dwns_Brp, startmonth, endmonth, startyear, startdays, startdate)
table2$station = "Bharatpur"
table <- rbind(table, table2)

#plotting
coeff <- 30           #scaling coefficient for precipitation

ggplot() +
  geom_bar(data = table, aes(x=days, y=prec*coeff, fill = "Precipitation"), stat="identity", alpha = 0.6) +
  geom_ribbon(data = table, aes(x=days, ymin = GDDsumlow, ymax = GDDsumhigh, 
                                fill = paste0("10-90th percentile" , "\n", "range (2000-2020)")), alpha = 0.5) +
  geom_line(data = table, aes(days, GDDsummean, col = "red"), lwd = 1.5) +
  geom_line(data = table, aes(days, GDDsum, col = "black"), lwd = 1.5) +
  geom_segment(data = table, aes(x = days[1], y = WheatInitiation, xend = thresholdday(table, WheatInitiation), 
                                 yend = WheatInitiation), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = days[1], y = WheatFlowering, xend = thresholdday(table, WheatFlowering), 
                                 yend = WheatFlowering), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = days[1], y = WheatMaturity, xend = thresholdday(table, WheatMaturity), 
                                 yend = WheatMaturity), lty=5, lwd = 1, alpha = 0.5) +
  geom_segment(data = table, aes(x = thresholdday(table, WheatInitiation), y = 0, xend = thresholdday(table, WheatInitiation), 
                                 yend = WheatInitiation), lty=5, lwd = 1, alpha = 0.2) +
  geom_segment(data = table, aes(x = thresholdday(table, WheatFlowering), y = 0, xend = thresholdday(table, WheatFlowering), 
                                 yend = WheatFlowering), lty=5, lwd = 1, alpha = 0.2) +
  geom_segment(data = table, aes(x = thresholdday(table, WheatMaturity), y = 0, xend = thresholdday(table, WheatMaturity), 
                                 yend = WheatMaturity), lty=5, lwd = 1, alpha = 0.2) +
  facet_wrap(~station) +
  ggtitle("Accumulated GDD", paste(" Date of sowing:", startdate, "\n", "Crop type: Wheat")) +
  labs(x = "Time (months)", y = "Cumulative GDD (°C)") + 
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_y_continuous(name = "Cumulative GDD (°C)", sec.axis = sec_axis(~./coeff, name="Precipitation (mm/day)")) +
  scale_color_manual(name = "", labels = c("2020", "Mean 2000-2020"), 
                     values = c("black", "red")) +
  scale_fill_manual(name = "Legend", values = c("grey70", "steelblue")) +
  annotate("text", min(table$days), WheatInitiation, vjust = -1, hjust = 0, label = "Initiation") +
  annotate("text", min(table$days), WheatFlowering, vjust = -1, hjust = 0, label = "Flowering") +
  annotate("text", min(table$days), WheatMaturity, vjust = -1, hjust = 0, label = "Maturity") +
  theme_minimal() +
  theme(axis.title.y = element_text(size = 13), axis.title.y.right = element_text(colour = "steelblue", size = 13), 
        axis.text.y.right = element_text(color = "steelblue"))