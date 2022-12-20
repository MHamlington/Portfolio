## Examine children's literature for diversity subject trends
## What subjects are written about the most and least
## Do most books focus on one subject or have multiple
## Which subjects are written about together
## What subjects need more books published

## future - compare to demographics, banned books lists

'
citation: Data on books by and about Black, Indigenous and People of Color 
compiled by the Cooperative Children’s Book Center (CCBC), School of Education, 
University of Wisconsin-Madison, based on its work analyzing the content of books 
published for children and teens received by the CCBC annually. 
https://ccbc.education.wisc.edu/literature-resources/ccbc-diversity-statistics/books-by-about-poc-fnn/
Accessed December 9, 2022
'

library(tidyverse)
install.packages('pheatmap')
library(pheatmap)
install.packages('paletteer')
library(paletteer)
install.packages('ggthemes')

DiversityRpt <- read_csv("Desktop/CCBC Diversity Report/CCBC Diversity Report.csv")

glimpse(DiversityRpt)

str(DiversityRpt)

## Cleaning

distinct(DiversityRpt)
# same number of rows as DiversityRpt, no duplicates

apply(is.na(DiversityRpt), 2, sum)
## missing ISBN probably because Advanced Readers Copy

# Elements that I will be usingappear to be clean

## Because focusing on diversity subjects, going to simplify DF and remove notes
## More specific analysis on specific subjects, i.e. type of disability, etc. could be done in the future

DRSimple <- select(DiversityRpt, c('Title', 'Year', 'CcbcCollection', 
                               'Genres', 'Subject', 'Publisher'))

glimpse(DRSimple)


head(DRSimple)

## Genre, Publisher have multiple entries, but unable to tell Subject by first 6 rows

head(DRSimple$Subject, n = 15)

## subject has multiple values, need to separate as well
## max number of genre and subject columns

Number_of_Genres <- str_count(DRSimple$Genres, ",") + 1
DRSimple['NumberOfGenres'] <- Number_of_Genres


Number_of_Subject <- str_count(DRSimple$Subject, ",") + 1
DRSimple['NumberOfSubjects'] <- Number_of_Subject


## Separate publisher and imprint

DRSimple <- DRSimple %>% 
  rename('PublisherImprint' = 'Publisher') %>% 
  separate(PublisherImprint, c('Publisher', 'Imprint'))

## Separate Genres and Subjects
           
DRSimple$Subject <- strsplit(DRSimple$Subject, split = ', ')
DRSimple <- unnest(DRSimple, Subject)           
           
DRSimple$Genres <- strsplit(DRSimple$Genres, split = ', ')
DRSimple <- unnest(DRSimple, Genres)



## number of publishers, pull from full DB to see Publisher and Imprint together

n_distinct(DiversityRpt$Publisher)

sort(table(DiversityRpt$Publisher), decreasing = TRUE)[1:10]

#number of diverse titles vs non diverse

DiverseTitles <- DRSimple %>% 
  distinct(Title, .keep_all = T) %>% 
  filter(!is.na(Subject))

TitleDiversity <- data.frame(group = c('Diverse Titles', 'Non-Diverse Titles'),
                             value = c(nrow(DiverseTitles), nrow(DiversityRpt)))

ggplot(TitleDiversity, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  scale_fill_manual(values=c("#00BFC4", "#F8766D")) +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = value ), 
            position = position_stack(vjust = 0.5)) +
  labs(x = NULL, Y = NULL)+
  theme(legend.title = element_blank())+
  labs(title = "Children's Books Published 2018 - 2022",
       subtitle = "Comparison of diverse titles vs. non-diverse titles",
       caption = "Data Source: Cooperative Children's Book Center")
  

## diverse vs non diverse by year published

DRSimple$Diverse <- DRSimple$Title %in% DiverseTitles$Title

DiverseByYear <- DRSimple %>% 
  distinct(Title, .keep_all = T) %>% 
  select(Year,Diverse)

TotalByYear <- DiverseByYear %>% 
  filter(Diverse == "FALSE") %>% 
  group_by(Year, Diverse) %>% 
  summarise('OtherBooks' = n())
TotalByYear <- TotalByYear[-6,]

DiverseByYear <- DiverseByYear %>% 
  filter(Diverse == "TRUE") %>% 
  group_by(Year, Diverse) %>% 
  summarise('DiverseTotal' = n())


DiverseByYear$TotalBooks = TotalByYear$OtherBooks

DiverseByYear %>% 
  pivot_longer(cols = c('DiverseTotal', 'TotalBooks'),
               names_to='group',
               values_to='total') %>% 
  ggplot(aes(x=Year, y=total, fill=group)) +
  geom_area() +
  labs(title ="
       All Children's Books Published 2018 - 2022",
       y = 'Total Books Published', caption = "Data Source: Cooperative Childrens Book Center") +
  scale_fill_discrete(labels = c("Non-Diverse Books", "Diverse Books"))+
  theme(legend.title= element_blank())


## most popular subject - some titles are counted multiple times if they have more than one subject
DRSCount <- DRSimple %>% 
  group_by(Subject) %>% 
  drop_na(Subject) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

DRSCount %>% 
  ggplot(aes(x = reorder(Subject, count), y = count)) +
  geom_segment(aes
               (x = reorder(Subject,count), xend =reorder(Subject,count), 
                 y = 0, yend = count), color = "skyblue") +
  geom_point(color = "blue", size = 4, alpha = 0.6)+
  coord_flip()+
  theme_light()+
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank())+
  labs(title = 'Diverse Book Subjects', x = 'Subjects', y = 'Title Count', 
  caption = "Data from Cooperative Children's Book Center, 2018-2022")

## most popular subject over years - what happened in 2020

Pop <- DRSimple %>% 
  select(Year, Subject) %>%
  drop_na(Subject) %>% 
  group_by(Year, Subject) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

Pop %>%   
  ggplot(aes(x=Year, y=count, color=fct_reorder2(Subject, Year, count))) +
  geom_line() +
  labs(title = 'Most Published Diversity Subjects 2018 - 2022', 
       y = 'Title Count', color = 'Subject Organized by 2022 Count', 
       caption = "Data Source: Cooperative Children's Book Center")


## Number of subjects per book

TitleSubjectCount<- DiversityRpt %>% 
  select(Title, Subject)

TitleSubjectCount$Count <- str_count(TitleSubjectCount$Subject, ', ')+1


TitleSubjectCount <- TitleSubjectCount %>% 
  drop_na()


ggplot(TitleSubjectCount, aes(x=Count))+
  geom_bar(color = "#D55E00", fill = "#D55E00")+
  labs(title = "Subjects Per Title", x = "Subject Count", y = "Title Count", 
       caption="Data from Cooperative Children's Book Center, 2018-2022")



##  compare books based on number of subjects (1 subject, 2, 3, 4)


SubjectDF <- select(DRSimple, c('Title', 'Subject'))

SubjectDF <- SubjectDF %>% 
  distinct() %>% 
  drop_na()



   


SubjectDFMatrix <- SubjectDF %>% 
  mutate(n = 1) %>% 
  spread(Subject, n, fill=0) %>% 
  select(-Title) %>% 
  {crossprod(as.matrix(.))} %>% 
  `diag<-`(0)

SubjectDFMatrix <- SubjectDFMatrix[DRSCount$Subject, DRSCount$Subject]



pheatmap(SubjectDFMatrix, treeheight_row = 0, treeheight_col = 0,
         cluster_rows = FALSE, cluster_cols = FALSE, 
         color = paletteer_c("ggthemes::Blue-Teal", 30),
         main = "Intersectionality in Children's Books: Two Word Pairs")



##STOP REPORT HERE














SubjectCount <- SubjectDF %>% 
  mutate(n = 1) %>% 
  spread(Subject, n, fill=0) %>% 
  select(-Title) %>% 
  {crossprod(as.matrix(.))} %>% 
  replace(lower.tri(., diag=T), NA) %>%
  reshape2::melt(na.rm=T) %>%
  unite('Pair', c('Var1', 'Var2'), sep=", ")

SubjectCount <- SubjectCount %>% 
  separate(Pair, into = c('Subject1', 'Subject2'), sep = ', ')

SubjectCount <- SubjectCount %>% 
  group_by(Subject1)





SubjectCount %>%  
  ggplot(aes(x = str_to_title(Subject1), y = str_to_title(Subject2), 
             size = value))+
  geom_point(alpha = 0.5)+
  scale_size(range = c(.1, 24))










IntersectionDF <- separate(IntersectionDF, Subject, into=c('Subject1', 'Subject2', 
                             'Subject3', 'Subject4', 'Subject5',
                             'Subject6', 'Subject7', 'Subject8'), sep=', ')







## Genre with the most diverse books


         






## ANALYSIS
## most popular subject

DRSimple %>% 
  group_by(Genres) %>% 
  drop_na(Genres) %>% 
  summarize(count = n()) %>%
  arrange(desc(count)) %>% 
  slice(1:10) %>% 
  tibble() %>% 
  ggplot(aes(x = reorder(Genres, -count), y = count)) +
  geom_col()




  










        




