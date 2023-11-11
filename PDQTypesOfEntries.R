library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
##############################################################
# Load Data
##############################################################
impData <-read_excel("./MasterRecord.xlsx",1)
#Add combined date Column 
impData$entry_Date <-as.Date(with(impData,paste(entry_Year,entry_Month,entry_Day,sep="-")),"%Y-%m-%d")
#FILTER DATA SOURCE HERE 
#impData <-subset(impData,REF=="ARTHUR-UNKNOWN-030423")

##############################################################
# Data Wrangling 
##############################################################

#Select columns here 

result <-impData %>%select(starts_with(c("Type","sub_Type")))

#Role up data 
result2 <-result%>%group_by(Type,sub_Type)%>%
  summarise(count=n())


##############################################################
# Output results 
##############################################################

kable(result2) %>%kable_styling(bootstrap_options = c("striped", "hover"),
                                font_size = 12,
                                position = "left")