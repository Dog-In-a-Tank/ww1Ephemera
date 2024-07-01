library(readxl)
library(dplyr)
library(writexl)
library(tm)
library(tidyverse)
library(tidytext)
library(wordcloud2)

#CHANGE CRITERIA AT BOTTOM TO SEARCH FOR WORD in results table

##############################################################
# Load Data
##############################################################
impData <-read_excel("./MasterRecord.xlsx",1)

##############################################################
# Create Filter List
##############################################################

filtFolders <-impData%>%distinct(REF)
#remove NA 
filtFolders <-filtFolders[!(is.na(filtFolders$REF)),]

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

dfText <- data.frame(matrix(ncol=3,nrow=0))
colnames(dfText) <- c('text','REF','sub_Ref') #future tidy up column names 

#use a loop to populate data frame 
for (i in 1:length(filenames))
{
  #get text and remove carriage returns
  strWord <-read_file(filenames[i])
  strWord <-gsub("[\r\n]", " ", strWord)
  strPath <-str_extract_all(filenames[i],"(?<=./).+(?=//)")
  strFile <-str_sub(sub(".txt","",filenames[i]),-4) 
  #add_Row
  dfText[nrow(dfText) + 1,] <- c(strWord,strPath,strFile)
}

############### FIND WORD HERE 
dfTextMerge <-inner_join(dfText,impData, by=c("REF","sub_Ref"))
dfTextMerge <-dfTextMerge%>%select(text,REF,sub_Ref)

dfWords <- dfTextMerge %>%
  unnest_tokens(word, text)
# Get the stop words dataset
data("stop_words")
dfWords_filtered <- dfWords %>%
  anti_join(stop_words, by = "word")


results <- dfWords_filtered %>%
  filter(grepl("Postman", word, ignore.case = TRUE))
