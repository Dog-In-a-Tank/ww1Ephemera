library(readxl)
library(dplyr)
library(writexl)
library(tm)
library(tidyverse)
library(tidytext)

library(gt)
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

############### BRING IN DATA ABOUT WHETEHR INFLUENCED 
#//RSJS note bringing in as a join so code is here for future usage. 
dfTextMerge <-inner_join(dfText,impData, by=c("REF","sub_Ref"))

############### Get the columns we want 
#RSJS need to tidy up copy column naming, hence here using original_author
# for futur dev make this clearer 
dfTextMerge <-dfTextMerge%>%select(text,REF,Original_Author)
dfTextMerge <-dfTextMerge%>%mutate(status = if_else(is.na(Original_Author), "ORIGINAL", "COPY"))

############### SPLIT DATA INTO INDIVIDUAL WORDS ######################################


dfText <-select(dfTextMerge,-REF,-Original_Author) #Drop columns we don't need 


##############################################################
#  Create individual words and remove stop words
##############################################################
tidy_word <- dfTextMerge %>%
  unnest_tokens(word, text)

# Get the stop words dataset
data("stop_words")
tidy_word_filtered <- tidy_word %>%
  anti_join(stop_words, by = "word")


#remove numbers
tidy_word <-tidy_word_filtered%>%mutate(word = gsub(x = word, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))
#remove th etc 
tidy_word <- tidy_word %>%
  filter(!(word %in% c("nd", "th", "st", "rd")))

#remove blanks
tidy_word <-subset(tidy_word, word!="") 

#build matrix

dfletterCount <- tidy_word%>%group_by(status)%>%count(word)

############### SPLIT DATA INTO TABLES ######################################
top_words <- dfletterCount %>%
  group_by(word) %>%
  summarize(total_n = sum(n)) %>%
  top_n(30, wt = total_n) %>%
  arrange(desc(total_n))


copy_words <- dfletterCount %>%
  filter(status == "COPY")%>%
  select(word, n) %>%
  distinct()

original_words <- dfletterCount %>%
  filter(status == "ORIGINAL")  %>%
  select(word, n) %>%
  distinct()

appear_in_both <-copy_words %>%
  inner_join(original_words, by = "word", suffix = c(".copy", ".original")) %>%
  mutate(
    n = n.copy + n.original,
    status = "BOTH"
  ) %>%
  select(status, word, n)

unique_to_copy <- copy_words %>%
  anti_join(original_words, by = "word") %>%
  arrange(desc(n)) %>%
  head(30)

unique_to_original <- original_words %>%
  anti_join(copy_words, by = "word") %>%
  arrange(desc(n)) %>%
  head(30)

############## Rank Data 

rank_Top<-top_words%>%mutate(rank = order(total_n, decreasing=TRUE))
rank_Top<-rank_Top%>%filter(rank <= 20)
rank_Top <- rank_Top %>% mutate(status = "ALL")
rank_Top <- rank_Top %>% rename(n = total_n)
# Reorder rank_Top to match the order of rank_Original and rank_Copy
rank_Top <- rank_Top %>% select(status, word, n, rank)

rank_Copy<-unique_to_copy%>%mutate(rank = order(n, decreasing=TRUE))
rank_Copy<-rank_Copy%>%filter(rank <= 20)

rank_Original<-unique_to_original%>%mutate(rank = order(n, decreasing=TRUE))
rank_Original<-rank_Original%>%filter(rank <= 20)

rank_Both<-appear_in_both%>%mutate(rank = order(n, decreasing=TRUE))
rank_Both<-rank_Both%>%filter(rank <= 20)

###############COMBINE 
combined_df <- union(rank_Original, rank_Copy)
combined_df <- union(combined_df, rank_Top)
combined_df <- union(combined_df, rank_Both)
###############PIVOT 
xCross <-pivot_wider(
  combined_df, 
  names_from = status,
  values_from = c(word,n))
xWords = xCross%>%select(rank,starts_with("word_"))
xWords = xWords%>%rename_at(vars(everything()), ~ sub("word_", "", .x))

##############################################################
# BUILD TABLE
##############################################################

tab1<-gt(xWords) %>%
  tab_header(
    title = md("**Frequent Words: Original entries vs Copied Entries vs All Entries**"),
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
tab1%>%gtsave("Survey Results:Frequent Words by Type.png")
