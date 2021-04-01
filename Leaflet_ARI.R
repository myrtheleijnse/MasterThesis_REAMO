###################################### Bubblemap landslide risk ######################################

setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("QM_downscaling.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveLandslides.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("RetrieveDate.R")
setwd("C:/Users/myrth/Documents/Universiteit_2020-2021/Master_Thesis/Data/Rfiles/Rfiles_vFinal") #alter to own working directory
source("ARI2017-2020.R")

#bubble map indicating landslide risk for all stations on specific date

#get ARI data
ARI_df <- ARI_all_1

#loop estimating lon and lat per station
i = 1
ARI_df$lon = 0
ARI_df$lat = 0
while (i <= nrow(ARI_df)){
  j = 1
  while (j <= nrow(station_locations)){ 
    if (is.element(ARI_df$station[i], station_locations$Name[j])){
      ARI_df$lon[i] = station_locations$Lon[j]
      ARI_df$lat[i] = station_locations$Lat[j]
    } 
    j = j + 1
  }
  i = i+1
}

#bubble mapping
landslideriskmap <- function(table, date){
  #subset date
  table <- table[table$Timestamps == date,]
  #prepare text for pop-up
  mytext <- paste(
    "Station: ", table$station, "<br/>",
    "Precipitation: ", lapply(table$prec, round, 2), "mm", "<br/>",
    "Date: ", date) %>%
    lapply(htmltools::HTML)
  
  #obtain colour palette
  mypalette <- colorBin(palette=c("springgreen4", "red"), domain=table$landsliderisk, na.color="transparent", bins=c(0,0.1,1))
  #make bubble map
  leaflet(table) %>%
    addTiles() %>%
    setView(lng = 85.325, lat = 28.125, zoom = 8) %>%
    #addProviderTiles("Esri.WorldImagery") %>%                        #for satellite image background
    addCircleMarkers(~lon, ~lat, fillColor = ~mypalette(landsliderisk), fillOpacity = 0.7, color="white", radius=8, stroke=FALSE,
                     label = mytext,
                     labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", 
                                                 direction = "auto")) %>%
    addLegend(pal=mypalette, values=~landsliderisk, opacity=0.9, title = paste("Landslide risk", "<br/>", date), 
              position = "bottomright",
              labFormat=function(type, cuts, p){paste0(c("No risk", "Landslide risk"))})
}

landslideriskmap(ARI_df, "2020-01-17")
