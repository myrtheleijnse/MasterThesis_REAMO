##################### Explore monthly T and P 2020 and anomalies #####################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

################################################ Compared to dwns2020 ################################################

### Kathmandu & Bharatpur ###

############################################
##### monthly average P and T observed #####
############################################
monthlyPT <- function(table, stationname){ 
  table <- getdays(table)
  table <- table %>% mutate(Temperature = (mintemp + maxtemp) / 2)
  table <- renametoprec(table)
  table <- table[table$year >= 2020,]
  table <- na.omit(table)
  table <- table %>% group_by(month) %>% summarize(prec = sum(prec),
                                                   Temperature = mean(Temperature))
  table$Station = stationname
  return(table)
}

#############################################
##### monthly average P and T anomalies #####
#############################################
monthlyPTanomalies <- function(table, stationname){
  table <- table %>% mutate(Temperature = (mintemp + maxtemp) / 2)
  table <- getdays(table)
  table <- renametoprec(table)
  table <- table[table$year >= 2000 & table$year <= 2020,]
  table <- na.omit(table)
  table <- table %>% group_by(month, year) %>% summarize(prec = sum(prec),
                                                         Temperature = mean(Temperature))
  table <- table %>% group_by(month) %>% summarize(Pmean = mean(prec),
                                                   Plow = quantile(prec, 0.1),
                                                   Phigh = quantile(prec, 0.9),
                                                   Tmean = mean(Temperature),
                                                   Tlow = quantile(Temperature, 0.1),
                                                   Thigh = quantile(Temperature, 0.9))
  table$Station = stationname
  return(table)
}

#combine both stations
df <- monthlyPT(dwns_Kmd[dwns_Kmd$Timestamps >= "2020-01-01",], "Kathmandu")
df2 <- monthlyPTanomalies(dwns_Kmd, "Kathmandu")
df3 <- monthlyPT(dwns_Brp[dwns_Brp$Timestamps >= "2020-01-01",], "Bharatpur")
df4 <- monthlyPTanomalies(dwns_Brp, "Bharatpur")

df <- rbind(df, df3)
df2<- rbind(df2, df4)
coeff = 20

### bar and line plot monthly P and T
ggplot() +
  geom_bar(data = df, aes(month, prec/coeff, fill = "P (2020)"), stat = "identity", alpha = 0.8) +
  geom_ribbon(data = df2, aes(x= month, ymin = Tlow, ymax = Thigh, fill = "10-90% percentiles T (2000-2019)"), alpha = 0.5) +
  geom_line(data = df2, aes(month, Tmean, col = "Mean T (2000-2019)"), lwd = 1.2) +
  geom_line(data = df, aes(month, Temperature, col = "T (2020)"), lwd = 1.2) +
  geom_pointrange(data = df2, aes(x=month, y=Pmean/coeff, ymin=Plow/coeff, ymax=Phigh/coeff, 
                                  colour = paste0("Mean and 90-10% percentiles", "\n", "P (2000-2019)")), 
                  stat = "identity",  alpha=0.9, position = position_dodge(width = 0.9)) +
  facet_wrap(~Station) +
  scale_y_continuous(name = "Temperature (°C)", sec.axis = sec_axis(~.*coeff, name="Precipitation (mm/month)")) +
  scale_x_continuous(breaks = seq(1,12,1), 
                     labels = c("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  scale_fill_manual(name = "Legend", values = c("grey70", "steelblue")) +
  scale_colour_manual(name = " ", values = c("black", "red", "orange")) +
  ggtitle("Monthly accumulated P and mean T", paste0("Year: 2020")) +
  theme_minimal()