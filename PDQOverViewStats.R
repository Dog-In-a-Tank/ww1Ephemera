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

##############################################################
# Data Wrangling 
##############################################################

#Select columns here 

result <-impData %>%select((c("REF","sub_Ref","entry_Date")))

#Role up data 
result2 <-result%>%group_by(REF) %>%
  summarise_all(funs(min,max), na.rm = TRUE)
