library(tidyverse)
library(tidytext)

library(wordcloud2)
library(webshot)


#build file List 
#JCLEGG-CLITHEROE-150322
filedir <-"./JCLEGG-CLITHEROE-150322/"
filenames <- list.files(filedir, pattern="*.txt", full.names=TRUE)
filenamesShort <- list.files(filedir, pattern="*.txt", full.names=FALSE)

# strip the extension
filenamesShort <- gsub(".txt$", "", filenamesShort)

#create empty dataframe

dfText <- data.frame(matrix(ncol=2,nrow=0))
colnames(dfText) <- c('text','source')

#use a loop to populate data frame 
for (i in 1:length(filenames))
{
  #get text and remove carriage returns
  strWord <-read_file(filenames[i])
  strWord <-gsub("[\r\n]", " ", strWord)
  #add_Row
  dfText[nrow(dfText) + 1,] <- c(strWord,filenamesShort[i])
}

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

