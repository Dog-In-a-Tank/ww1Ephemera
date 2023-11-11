library(readxl)
library(dplyr)
library(measurements)
library(mapview)
library(htmltools)
library(htmlwidgets)
library("sf")
##############################################################
# Load Data
##############################################################

impData <-read_excel("./MasterRecord.xlsx",1)
#Add combined date Column 
impData$entry_Date <-as.Date(with(impData,paste(entry_Year,entry_Month,entry_Day,sep="-")),"%Y-%m-%d")
#FILTER DATA SOURCE HERE 
impData <-subset(impData,REF=="HAMPTON - CHESHIRE-270223")
##############################################################
# Data Wrangling 
##############################################################

result <-impData %>%select(starts_with(c("sub","entry","creator")))
#FILTER out Blanks
result2 <- result %>% filter_at(vars(entry_From_Log),all_vars(!is.na(.)))
#add in coords 
result3 <- st_as_sf(result2, coords=c("entry_From_Log","entry_From_Lat"),crs = 4326)
##############################################################
# Output results 
##############################################################
mapviewOptions(basemaps = c("Esri.WorldShadedRelief","Esri.WorldGrayCanvas"),
               raster.palette = grey.colors,
               layers.control.pos = "bottomright",legend= FALSE)

map<-mapview(result3,col.regions = "Green")






