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
#add length of signature 
impData$lenSignature <- nchar((impData$creator_Signature))

#FILTER DATA SOURCE HERE 
#impData <-subset(impData,REF=="Broken Blue - 030423")
impData <-subset(impData,creator_is_Military =="Y")

##############################################################
# Data Wrangling 
##############################################################

#Select columns here 
result <-impData %>%select(starts_with(c("sub","entry","creator","len")))
#Role up data 
#sort by name and sig length to get longest sig
result2 <-result%>%arrange(creator_Surname, creator_Forename, desc(lenSignature)) 
result3 <-result2%>%group_by(creator_Identified,
                             creator_Forename,
                             creator_Surname,
                             creator_Military_ServiceNo)%>%
  summarize(max(creator_Signature),max(entry_Date), min(entry_Date), count=n())
#Order Data 
result3 <-result3%>%arrange(creator_Surname, creator_Forename)

##############################################################
# Output results 
##############################################################

kable(result3) %>%kable_styling(bootstrap_options = c("striped", "hover"),
                             font_size = 12,
                             position = "left")
