#7_summarise_stats_sections.R
library(tidyverse)
library(tidytext)
library(openxlsx)
dat = readRDS('data/stats_section_cleaned.rds')
#ten topics, PLOS ONE
matches = read.csv('manuscript/plos_one_10topics.csv',header=T)
matches = left_join(matches,dat,by=c('DOI'='doi')) %>% rename('doi'=DOI)

#examples of boilerplate text: topic 3
 tidy_matches = matches %>%
   unnest_tokens(word,text_data_clean)
 wordcounts <- tidy_matches %>%
   group_by(topic_id,doi) %>%
   summarise(words = n(),.groups='drop')
 
matches = left_join(matches,wordcounts,by=c('doi','topic_id')) %>%
  arrange(topic_id,-value) %>%
  group_by(topic_id) %>% mutate(rank=row_number()) %>% ungroup() %>%
  mutate(topic_id = paste('Topic',topic_id))


save(matches,file='manuscript/plos.results.10topics.rda')


#example - topics 3 and 5
matches %>% filter(topic_id %in% c('Topic 1', 'Topic 3','Topic 5')) %>% ggplot(.,aes(x=rank,y=words)) + 
  geom_point(alpha=0.1)+ geom_smooth(method='loess',se=T,colour='blue')+ facet_wrap(~topic_id,scales = 'free') + 
  scale_x_continuous('Rank (1 = strongest topic match)')+scale_y_continuous('Word count (cleaned text)',breaks=seq(0,1500,100))+theme_minimal()


#example - topics 1 and 9
matches %>% filter(topic_id %in% c('Topic 1','Topic 9')) %>% ggplot(.,aes(x=rank,y=words)) + 
  geom_point(alpha=0.1)+ geom_smooth(method='loess',se=T,colour='blue')+ facet_wrap(~topic_id,scales='free') + 
  scale_x_continuous('Rank (1 = strongest topic match)')+scale_y_continuous('Word count (cleaned text)')+theme_minimal()


matches %>% filter(topic_id=='Topic 1') %>% View()

# 
# #five topics
# matches = read.xlsx('manuscript/top.matches.5topics.xlsx')
# matches = left_join(matches,dat,by='doi')
# write.xlsx(matches,file='manuscript/top.matches.5topics.text.xlsx',colNames=T)
# 


#topic 1
filter(matches,topic==1) %>% select(doi,text_data) %>% View()

# not run:
# tidy_dat = dat %>%
#   unnest_tokens(word,text_data_clean)
# 
# wordcounts <- tidy_dat %>%
#   group_by(doi) %>%
#   summarise(words = n(),.groups='drop')
# 
# #join with metadata
# #load meta data for all records found
# load('./data/plos_meta_data.rda')
# 
# wordcounts = left_join(wordcounts,meta_dat_allrecords,by='doi')
# wordcounts %>% group_by(volume) %>% summarise(med=median(words),
#                                               Min=min(words),Max=max(words),
#                                               q1=quantile(words,.25),q3=quantile(words,.75),.groups='drop')
# 
# 
# ggplot(wordcounts,aes(y=log(words),x=volume,group=volume))+geom_boxplot()  
