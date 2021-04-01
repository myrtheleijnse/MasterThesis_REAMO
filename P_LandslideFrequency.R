######################### Trend rainfall vs landslide frequency #########################
# All stations
# On monthly basis
# Including R^2 
# Location specific landslide inventory

#reading location-specific inventories
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveLandslides.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")

monthlyinventory <- function(table){
  table <- getleapdays(table)
  table <- table %>% group_by(year, month) %>% summarize(nrlandslides = sum(landsliderisk))
  return(table)
}

monthlyinv295 <- monthlyinventory(inv295)
monthlyinv346 <- monthlyinventory(inv346)
monthlyinv347 <- monthlyinventory(inv347)
monthlyinv349 <- monthlyinventory(inv349)
monthlyinvKmd <- monthlyinventory(invKmd)
monthlyinvBrp <- monthlyinventory(invBrp)
monthlyinvSpg <- monthlyinventory(invSpg)

# calculate monthly summed precipitation
monthlyprec <- function(table, startdate, enddate){
  table <- table[table$Timestamps >= startdate & table$Timestamps <= enddate,]
  table <- getleapdays(table)
  #rainfall sum per month
  table <- table %>% group_by(year, month) %>% summarize(prec = sum(prec))
  return(table)
}

df_Kmd <- monthlyprec(dwns_Kmd, "2017-01-01", "2020-12-31")
df_Kmd$station = station_locations$Name[1]
df_Brp <- monthlyprec(dwns_Brp, "2017-01-01", "2020-12-31")
df_Brp$station = station_locations$Name[3]
df_Spg <- monthlyprec(dwns_Spg, "2017-01-01", "2020-12-31")
df_Spg$station = station_locations$Name[4]
df_295 <- monthlyprec(dwns_295, "2017-01-01", "2020-12-31")
df_295$station = station_locations$Name[5]
df_346 <- monthlyprec(dwns_346, "2017-01-01", "2020-12-31")
df_346$station = station_locations$Name[6]
df_347 <- monthlyprec(dwns_347, "2017-01-01", "2020-12-31")
df_347$station = station_locations$Name[7]
df_349 <- monthlyprec(dwns_349, "2017-01-01", "2020-12-31")
df_349$station = station_locations$Name[8]

# combine dataframe with monthly landslide inventory and monthly precipitation
df_monthlyprec <- rbind(df_Kmd, df_Brp, df_Spg, df_295, df_346, df_347, df_349)
monthlyinv <- rbind(monthlyinvKmd, monthlyinvBrp, monthlyinvSpg, monthlyinv295, monthlyinv346, monthlyinv347, monthlyinv349)
df <- cbind(df_monthlyprec, monthlyinv$nrlandslides)
colnames(df)[colnames(df) == "...5"] <- "nrlandslides"

# Calculate (adjusted) R^2
i = 1
df$r2 = 0
while (i <= nrow(df)){
  if (df$station[i] == "Kathmandu"){
    data <- df[df$station == "Kathmandu",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  } else if (df$station[i] == "Bharatpur"){
    data <- df[df$station == "Bharatpur",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  } else if (df$station[i] == "Sherpagaon"){
    data <- df[df$station == "Sherpagaon",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  } else if (df$station[i] == "Timure"){
    data <- df[df$station == "Timure",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  } else if (df$station[i] == "Pansayakhola"){
    data <- df[df$station == "Pansayakhola",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  } else if (df$station[i] == "Thamachit"){
    data <- df[df$station == "Thamachit",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  } else {
    data <- df[df$station == "Dhunche",]
    df$r2[i] = format(summary(lm(prec~nrlandslides, data=data))$adj.r.squared, digits = 3)
  }
  i = i+1
}

### plotting per station
ggplot(data = df, aes(nrlandslides, prec)) +
  geom_point() +
  geom_smooth(method="lm") +
  facet_wrap(~station) +
  xlab("Landslides per month") +
  ylab("Precipitation per month (mm)") +
  ggtitle("Trend monthly precipitation and landslide frequency") +
  theme_minimal()

#Separate all stations
df_Kmd <- df[df$station == "Kathmandu",]
df_Brp <- df[df$station == "Bharatpur",]
df_295 <- df[df$station == "Timure",]
df_346 <- df[df$station == "Thamachit",]
df_347 <- df[df$station == "Dhunche",]
df_349 <- df[df$station == "Pansayakhola",]
df_Spg <- df[df$station == "Sherpagaon",]

#Plot grid separate stations
plot_grid(
  ggplot(data = df_Kmd, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("Trend monthly precipitation and landslide frequency", paste0("Station: ", df_Kmd$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_Kmd))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal(),
  ggplot(data = df_Brp, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("", paste0("Station: ", df_Brp$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_Brp))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal(),
  ggplot(data = df_Spg, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("", paste0("Station: ", df_Spg$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_Spg))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal(),
  ggplot(data = df_295, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("", paste0("Station: ", df_295$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_295))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal(),
  ggplot(data = df_346, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("", paste0("Station: ", df_346$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_346))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal(),
  ggplot(data = df_347, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("", paste0("Station: ", df_347$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_347))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal(),
  ggplot(data = df_349, aes(nrlandslides, prec)) +
    geom_point() +
    geom_smooth(method="lm") +
    xlab("Landslides per month") +
    ylab("Precipitation per month (mm)") +
    ggtitle("", paste0("Station: ", df_349$station)) +
    annotate("text", label = paste0("r2 = ", format(summary(lm(prec~nrlandslides, data=df_349))$adj.r.squared, digits = 3)),
             x = 0.5, y = 1750, size = 4) +
    theme_minimal()
)
