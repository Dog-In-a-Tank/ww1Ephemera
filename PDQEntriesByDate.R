library(readxl)
library(dplyr)
library(knitr)
library(kableExtra)
##############################################################
# Load Data
##############################################################
impData <-read_excel("./MasterRecord.xlsx,1)
#Add combined date Column 
impData$entry_Date <-as.Date(with(impData,paste(entry_Year,entry_Month,entry_Day,sep="-")),"%Y-%m-%d")
#FILTER DATA SOURCE HERE 
impData <-subset(impData,REF=="HAMPTON - CHESHIRE-270223")

##############################################################
# Data Wrangling 
##############################################################

#Select columns here 

result <-impData %>%select(starts_with(c("sub","entry","creator")))

#Order Data 
result2 <-result%>%arrange(entry_Date, creator_Surname)

##############################################################
# Output results 
##############################################################

kable(result2) %>%kable_styling(bootstrap_options = c("striped", "hover"),
                                font_size = 12,
                                position = "left")
