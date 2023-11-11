library(readxl)
library(dplyr)
library(writexl)
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


dfText <-select(dfText,-ref)#drop the ref column - not needed for this 
dfTidyText <-dfText%>%
  unnest_tokens(word,text)

##############################################################
# Remove stopWords and Create individual words 
##############################################################

#build matrix
dfletterCount <- dfTidyText%>%group_by(ref)%>%count(word)


tidy_word <- dfText %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords(), by = "word") %>%
  na.omit()

############### SPLIT DATA INTO INDIVIDUAL WORDS ######################################

dfTidyText <-dfText%>%
  unnest_tokens(word,text)
############### WORDCLOUD ######################################

#build matrix
dfletterCount <- count(dfTidyText,word)
#remove stopwords
dfletterCount <- dfletterCount %>% anti_join(stop_words)%>%
  filter(is.na(as.numeric(word)))
set.seed(1224)

wordcloud2(dfletterCount, size=1,  color = "random-light", backgroundColor = "black" )


############### OPTIONAL VOLCAB ######################################
write_xlsx(dfTidyText,"\\file name.xlsx")
volcab <-dfTidyText
volcab2 <-volcab%>%group_by(ref)%>%summarize(x = n_distinct(word))
