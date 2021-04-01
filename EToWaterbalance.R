##################### ETo water balance (Makkink) #####################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ETomethods.R")

################################################
### Makkink waterbalance for downscaled ERA5 ###
################################################
makkinkwaterbal <- function(table, startday, startmonth, endmonth, startyear, endyear) {
  table <- meteo_makkink(table)
  table <- getdays(table)
  table <- table[table$day >= startday & table$year >= startyear & table$year <= endyear &
                   table$month >= startmonth & table$month <= endmonth,]
  #precipitation deficit (water balance) per timestep
  table <- table %>% mutate(Precdef = prec-ETo)
  
  #get data.table frame
  require(data.table)
  table <- data.table(table)
  
  #calculate cumsums
  table[, cumsumPrecdef := cumsum(Precdef), by = list(year)]
  
  #get statistics
  #median
  table[, meansumPrecdef := median(cumsumPrecdef), by = list(days)]
  #high percentile (90%)
  table[, highsumPrecdef := quantile(cumsumPrecdef, 0.90, na.rm=T), by = list(days)]
  #low percentile (10%)
  table[, lowsumPrecdef := quantile(cumsumPrecdef, 0.1, na.rm=T), by = list(days)]
  #min values
  table[, minsumPrecdef := min(cumsumPrecdef), by = list(days)]
  #max values
  table[, maxsumPrecdef := max(cumsumPrecdef), by = list(days)]
  
  return(table)
}

#####################################################
### Makkink waterbalance for downscaled ERA5 2020 ###
#####################################################
makkinkwaterbal2020 <- function(table, startday, startmonth, endmonth, startyear) {
  table <- meteo_makkink(table)
  table <- getdays(table)
  table <- table[table$day >= startday & table$month >= startmonth & table$month <= endmonth & table$year >= startyear,]
  table <- table %>% mutate(Precdef = prec-ETo)
  
  #get data.table frame
  require(data.table)
  table <- data.table(table)
  
  #Calculate cumsum
  table[, cumsumPrecdef := cumsum(Precdef), by = list(year)]
  
  return(table)
}

######################################################### Rice #########################################################

#date initials
startday <- 15
startmonth <- 7
endmonth <- 11
startyear <- 2000
endyear <- 2020

#calculate water balance per station
table1 <- makkinkwaterbal(dwns_Kmd, startday, startmonth, endmonth, startyear, endyear)
table1$station = "Kathmandu"
table2 <- makkinkwaterbal2020(dwns_Kmd, startday, startmonth, endmonth, 2020)
table2$station = "Kathmandu"
table3 <- table1[table1$Timestamps >= "2015-01-01" & table1$Timestamps <= "2015-12-31",]
table3$station = "Kathmandu"
table4 <- makkinkwaterbal(dwns_Brp, startday, startmonth, endmonth, startyear, endyear)
table4$station = "Bharatpur"
table5 <- makkinkwaterbal2020(dwns_Brp, startday, startmonth, endmonth, 2020)
table5$station = "Bharatpur"
table6 <- table4[table4$Timestamps >= "2015-01-01" & table4$Timestamps <= "2015-12-31",]
table6$station = "Bharatpur"

#combine stations in one data frame
table <- rbind(table1, table4)
table2 <- rbind(table2, table5)
table3 <- rbind(table3, table6)

### plotting Rice

ggplot() +
  geom_ribbon(data = table, aes(ymin = lowsumPrecdef, ymax = highsumPrecdef, x= days, 
                                fill = paste0("10-90th percentile" , "\n", "range (2000-2020)")), alpha = 0.3) +
  geom_line(data = table3, aes(days, cumsumPrecdef, colour = "purple"), lwd = 2) +
  geom_line(data = table2, aes(days, cumsumPrecdef, colour = "black"), lwd = 2) +
  geom_line(data = table, aes(days, minsumPrecdef, colour = "orange"), lwd = 1.2) +
  geom_line(data = table, aes(days, maxsumPrecdef, colour = "steelblue"), lwd = 1.2) +
  geom_line(data = table, aes(days, meansumPrecdef, colour = "red"), lwd = 1.2) +
  facet_wrap(~station) +
  labs(x = "", y = expression(paste(Sigma, "P-ETo (mm)"))) +
  geom_hline(yintercept = 0, col="grey", lty = "longdash") +
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(name = "Legend", labels = c("2020", "Min (2000-2020)", "2015", "Median (2000-2020)", "Max (2000-2020)"), 
                     values = c("black", "orange", "purple", "red", "steelblue")) +
  scale_fill_manual(name = "Band", values = "grey70") +
  ggtitle("Cumulative climatic water balance", paste(" Crop: Rice", "\n", 
                                                     "Sowing date:", table2$Timestamps[1])) +
  theme_minimal()

######################################################### Maize #########################################################

startday <- 1
startmonth <- 4
endmonth <- 6
startyear <- 2000
endyear <- 2020

#calculate water balance per station
table1 <- makkinkwaterbal(dwns_Kmd, startday, startmonth, endmonth, startyear, endyear)
table1$station = "Kathmandu"
table2 <- makkinkwaterbal2020(dwns_Kmd, startday, startmonth, endmonth, 2020)
table2$station = "Kathmandu"
table3 <- table1[table1$Timestamps >= "2015-01-01" & table1$Timestamps <= "2015-12-31",]
table3$station = "Kathmandu"
table4 <- makkinkwaterbal(dwns_Brp, startday, startmonth, endmonth, startyear, endyear)
table4$station = "Bharatpur"
table5 <- makkinkwaterbal2020(dwns_Brp, startday, startmonth, endmonth, 2020)
table5$station = "Bharatpur"
table6 <- table4[table4$Timestamps >= "2015-01-01" & table4$Timestamps <= "2015-12-31",]
table6$station = "Bharatpur"

#combine stations in one data frame
table <- rbind(table1, table4)
table2 <- rbind(table2, table5)
table3 <- rbind(table3, table6)

### plotting Maize

ggplot() +
  geom_ribbon(data = table, aes(ymin = lowsumPrecdef, ymax = highsumPrecdef, x= days, 
                                fill = paste0("10-90th percentile" , "\n", "range (2000-2020)")), alpha = 0.3) +
  geom_line(data = table3, aes(days, cumsumPrecdef, colour = "purple"), lwd = 2) +
  geom_line(data = table2, aes(days, cumsumPrecdef, colour = "black"), lwd = 2) +
  geom_line(data = table, aes(days, minsumPrecdef, colour = "orange"), lwd = 1.2) +
  geom_line(data = table, aes(days, maxsumPrecdef, colour = "steelblue"), lwd = 1.2) +
  geom_line(data = table, aes(days, meansumPrecdef, colour = "red"), lwd = 1.2) +
  facet_wrap(~station) +
  labs(x = "", y = expression(paste(Sigma, "P-ETo (mm)"))) +
  geom_hline(yintercept = 0, col="grey", lty = "longdash") +
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(name = "Legend", labels = c("2020", "Min (2000-2020)", "2015", "Median (2000-2020)", "Max (2000-2020)"), 
                     values = c("black", "orange", "purple", "red", "steelblue")) +
  scale_fill_manual(name = "Band", values = "grey70") +
  ggtitle("Cumulative climatic water balance", paste(" Crop: Maize", "\n", 
                                                     "Sowing date:", table2$Timestamps[1])) +
  theme_minimal()

######################################################### Wheat #########################################################

startday <- 1
startmonth <- 1
endmonth <- 3
startyear <- 2000
endyear <- 2020

#calculate water balance per station
table1 <- makkinkwaterbal(dwns_Kmd, startday, startmonth, endmonth, startyear, endyear)
table1$station = "Kathmandu"
table2 <- makkinkwaterbal2020(dwns_Kmd, startday, startmonth, endmonth, 2020)
table2$station = "Kathmandu"
table3 <- table1[table1$Timestamps >= "2015-01-01" & table1$Timestamps <= "2015-12-31",]
table3$station = "Kathmandu"
table4 <- makkinkwaterbal(dwns_Brp, startday, startmonth, endmonth, startyear, endyear)
table4$station = "Bharatpur"
table5 <- makkinkwaterbal2020(dwns_Brp, startday, startmonth, endmonth, 2020)
table5$station = "Bharatpur"
table6 <- table4[table4$Timestamps >= "2015-01-01" & table4$Timestamps <= "2015-12-31",]
table6$station = "Bharatpur"

#combine stations in one data frame
table <- rbind(table1, table4)
table2 <- rbind(table2, table5)
table3 <- rbind(table3, table6)

### plotting Wheat

ggplot() +
  geom_ribbon(data = table, aes(ymin = lowsumPrecdef, ymax = highsumPrecdef, x= days, 
                                fill = paste0("10-90th percentile" , "\n", "range (2000-2020)")), alpha = 0.3) +
  geom_line(data = table3, aes(days, cumsumPrecdef, colour = "purple"), lwd = 2) +
  geom_line(data = table2, aes(days, cumsumPrecdef, colour = "black"), lwd = 2) +
  geom_line(data = table, aes(days, minsumPrecdef, colour = "orange"), lwd = 1.2) +
  geom_line(data = table, aes(days, maxsumPrecdef, colour = "steelblue"), lwd = 1.2) +
  geom_line(data = table, aes(days, meansumPrecdef, colour = "red"), lwd = 1.2) +
  facet_wrap(~station) +
  labs(x = "", y = expression(paste(Sigma, "P-ETo (mm)"))) +
  geom_hline(yintercept = 0, col="grey", lty = "longdash") +
  scale_x_continuous(breaks = seq(0,361,30.4), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_color_manual(name = "Legend", labels = c("2020", "Min (2000-2020)", "2015", "Median (2000-2020)", "Max (2000-2020)"), 
                     values = c("black", "orange", "purple", "red", "steelblue")) +
  scale_fill_manual(name = "Band", values = "grey70") +
  ggtitle("Cumulative climatic water balance", paste(" Crop: Wheat", "\n", 
                                                     "Sowing date:", table2$Timestamps[1])) +
  theme_minimal()