library(readxl)
library(dplyr)
#library(writexl) used for testing
library(tidyverse)
library(tidytext)
library(wordcloud2)
##############################################################
# Load Data
##############################################################
impData <-read_excel("./MasterRecord.xlsx",1)
##############################################################
# Create Filter List
##############################################################
filtFolders <-impData%>%distinct(REF)
filtFolders$REF <-str_c("./",filtFolders$REF,"/") #format as filepaths
##############################################################
# Build a list of Files 
##############################################################
for (r in filtFolders)
{
  filenames <- list.files(r, pattern="*.txt", full.names=TRUE)
}

##############################################################
# strip the extension 
##############################################################


#create empty dataframe

dfText <- data.frame(matrix(ncol=2,nrow=0))
colnames(dfText) <- c('text','ref')

#use a loop to populate data frame 
for (i in 1:length(filenames))
{
  #get text and remove carriage returns
  strWord <-read_file(filenames[i])
  strWord <-gsub("[\r\n]", " ", strWord)
  strPath <-str_extract_all(filenames[i],"(?<=./).+(?=//)")
  #strFile <-str_sub(sub(".txt","",filenames[i]),-4) future development if we wnat filenames
  #add_Row
  dfText[nrow(dfText) + 1,] <- c(strWord,strPath)
}

############### SPLIT DATA INTO INDIVIDUAL WORDS ######################################

dfTidyText <-dfText%>%
  unnest_tokens(word,text)

##############################################################
# Remove stopWords and Create individual words 
##############################################################


#build matrix
dfletterCount <- dfTidyText%>%group_by(ref)%>%count(word)

tidy_word <-dfletterCount %>% anti_join(stop_words) %>%
  na.omit()%>%filter(is.na(as.numeric(word)))
#write_xlsx(tidy_word,"bob.xlsx") testing 

##############################################################
# build report 
##############################################################\

rEntries = dfText%>%group_by(ref)%>%tally()
rEntries = rEntries%>%rename(NumberEntries=n)

rUnique = tidy_word%>%group_by(ref)%>%tally()
rUnique = rUnique%>%rename(UniqueWords=n)

rTotals = tidy_word%>%group_by(ref,.drop = FALSE)%>%summarize(TotalWords =sum(n))

report<-merge(rEntries,rTotals,by = "ref")
report<-merge(report,rUnique,by = "ref")

report2<- report%>%mutate(AverageLength=report$TotalWords/report$NumberEntries)
report2$AverageLength<-round(report2$AverageLength,1)

report2
