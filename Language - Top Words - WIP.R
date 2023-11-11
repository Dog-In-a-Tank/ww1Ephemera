library(readxl)
library(dplyr)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(gt)
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


rank_word<-tidy_word%>%mutate(rank = order(order(ref, n, decreasing=TRUE)))
rank_word<-rank_word%>%filter(rank <= 20)

#PIVOT DATA
xCross <-pivot_wider(
  rank_word, 
  names_from = ref,
  values_from = c(word,n))

xWords = xCross%>%select(rank,starts_with("word_"))

#REMOVE COLUMN PREFIX 
xWords = xWords%>%rename_at(vars(everything()), ~ sub("word_", "", .x))
#ORDER BY 
xWords <- xWords%>%arrange(xWords$rank)
##############################################################
# BUILD TABLE
##############################################################

tab1<-gt(xWords) %>%
  tab_header(
    title = md("**Most Frequent Words: By Source**"),
    subtitle = md("*Stop Words Removed*"))%>%
  tab_options(table.font.names = 'Times New Roman',
              column_labels.font.weight = 'bold',
              heading.title.font.size = 22,
              heading.subtitle.font.size = 16,
              table.font.color = 'Black',
              source_notes.font.size = 14,
              #source_notes.
              table.font.size = 14)%>%
  tab_source_note(source_note = "Source: RobertStJohnSmith.com - Work in Progress")

tab1
