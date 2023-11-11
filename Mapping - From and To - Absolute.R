library(readxl)
library(dplyr)
library(geosphere)
library(measurements)
library(mapdeck)

##############################################################
# Load Data
##############################################################

impData <-read_excel("./MasterRecord - wip_v3.xlsx",1)
#Add combined date Column 
impData$entry_Date <-as.Date(with(impData,paste(entry_At,entry_Year,entry_Month,entry_Day,sep="-")),"%Y-%m-%d")
#FILTER DATA SOURCE HERE 
impData <-subset(impData,REF=="HAMPTON - CHESHIRE-270223")
impData <-subset(impData,entry_At=="Hampton Hall, Malpas")
##############################################################
# Data Wrangling 
##############################################################

result <-impData %>%select(starts_with(c("sub","entry","creator")))
#FILTER out Blanks
result2 <- result %>% filter_at(vars(entry_From,entry_At),all_vars(!is.na(.)))
#ADD DISTANCE convert to Miles
result2$DistCrowFlies <-  distHaversine(cbind(result2$entry_At_Lat,result2$entry_At_Log),cbind(result2$entry_From_Lat,result2$entry_From_Log) )
result2$DistCrowFlies<-conv_unit(result2$DistCrowFlies,"m","mi")


##############################################################
# Output results 
##############################################################

set_token("pk.eyJ1IjoiY2hhbmNlNDMyMSIsImEiOiJjbGZmanp2NncyenB1M3lwY3duZ3Ruc2JkIn0.wUF0oSjRVwBr5Ok-_LoE7Q")


mapdeck(
  style = mapdeck_style('dark')
  , location = c(0, 0)
  , zoom = 1
  , pitch = 70
) %>%
  add_arc(
    data = result2
    , origin = c("entry_At_Log", "entry_At_Lat")
    , destination = c("entry_From_Log", "entry_From_Lat")
    , layer_id = 'arcs'
    , stroke_from_opacity = 100
    , stroke_to_opacity = 100
    , stroke_width = 3
    , stroke_from = "#ccffff"
      , stroke_to = "#ccffff"
  ) 


