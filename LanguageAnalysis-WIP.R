library(tidyverse)
library(tidytext)
library(dplyr)
library(readxl)
library(stringr)

library(ggplot2)
library(ggchicklet)

library(ggtext)
##############################################################
# Load Data
##############################################################
impData <-read_excel("./MasterRecord - wip_v4.xlsx",1)

##############################################################
#Get Sentiments
##############################################################
sentiment_dict<- get_sentiments("nrc") %>% 
  filter(!sentiment %in% c("positive", "negative"))

sentiment_posit<- get_sentiments("nrc") %>% 
  filter(sentiment %in% c("positive", "negative"))

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


##############################################################
# Remove stopWords and Create individual words 
##############################################################
tidy_word <- dfText %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords(), by = "word") %>%
  na.omit()
##############################################################
# Join to Sentiment list, Group By Source, add percentage
##############################################################

result<-tidy_word%>%
  inner_join(sentiment_dict)

result2 <-result%>%
  group_by(ref,sentiment) %>% tally()%>%mutate(Percent = n/sum(n))
 
result4 <-result2%>%fct_reorder(sentiment= Ref, sentiment,percent)
#result3 <-result2%>%arrange(ref,desc(n)) %>%mutate(rank = row_number())#ranking 

##############################################################
# Create Chart
##############################################################
ggplot(result2,aes(x=ref,y=Percent, fill = sentiment))+ 
  geom_chicklet(width = 0.75) +
  scale_y_continuous(limits = c(0,1),expand = c(0, 0)) +
  coord_flip()+
  ggthemes::scale_fill_tableau("Classic Color Blind", name = NULL)+
  theme_minimal() +
  theme(axis.text.x = element_text(color = "gray60", size = 10)) +
  theme(legend.position = "top")+
  labs(
    y = "Percent",    x = NULL, fill = NULL,
    title = "Ephmera Autograph Book Sentiment Analysis",
    subtitle = "Work in Progress",
    caption = "Source: Emphmera Matrix
     NRC Database https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm"
  )
