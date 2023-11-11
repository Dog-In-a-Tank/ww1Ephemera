library(readxl)
library(dplyr)
library(ggplot2)
library(treemapify)
##############################################################
# Load Data
##############################################################
impData <-read_excel("./MasterRecord.xlsx",1)
#Add combined date Column 
impData$entry_Date <-as.Date(with(impData,paste(entry_Year,entry_Month,entry_Day,sep="-")),"%Y-%m-%d")
#FILTER DATA SOURCE HERE 
#impData <-subset(impData,REF=="ARTHUR-UNKNOWN-030423")

##############################################################
# Data Wrangling 
##############################################################

#Select columns here 

result <-impData %>%select(starts_with(c("Type")))

#Role up data 
result2 <-result%>%count(Type)

#As Percentage (optional)
result3 <-result2%>%mutate(Perc=paste0(round(n/sum(n)*100,2),"%"))

##############################################################
# Output results 
##############################################################
p<-ggplot(result3, aes(area = n, fill = Type,label=paste(Type,Perc,sep="\n"))) +
  geom_treemap(start = "topleft",linetype = "solid", color = "white", lwd = 2) +
  geom_treemap_text( start = "topleft",
                     place = "centre",
                     min.size = 2,
                     color = "White",
                     padding.x = unit(2, "mm"),
                     grow = TRUE,
  ) +
  scale_color_manual(guide = FALSE, values = c("black", "white"))+
  labs(title = "Autograph Book - Make-Up", 
       subtitle = "Treemap showing the relative size of type of entries in an Autograph.",
       caption ="Source: RobertStJohnsmith.com - Work in Progresss") 

p<-p + scale_fill_grey(start=.8, end= 0.1)+
  theme(text = element_text(family = "Times New Roman"),legend.position = "none")
p

ggsave(filename = "Autograph Book Make Up.png", p, width = 16, height = 10, dpi = 300, units = "cm", device='png')
