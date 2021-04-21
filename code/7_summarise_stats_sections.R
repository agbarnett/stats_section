#7_summarise_stats_sections.R
library(tidyverse)
library(tidytext)
library(openxlsx)

dat = readRDS('../data/stats_section_cleaned.rds')
#ten topics, PLOS ONE
matches = read.csv('../results/plos_one_10topics.csv',header=T)
matches = left_join(dat,matches,by=c('doi'='DOI')) %>%
  mutate(value = as.numeric(value)) %>%
  distinct(doi,text_heading,.keep_all = T)

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


cos.sim <- function(ix,distances.mat) 
{
  A = distances.mat[,ix[1]]
  B = distances.mat[,ix[2]]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
} 

distances = tidy_matches %>% left_join(.,matches %>% select(doi,rank),by='doi') %>% filter(rank<=100)

calc_dist_mat <- function(indata=distances,topicNumber){
 to_stat = indata %>% filter(topic_id==topicNumber) %>%
    distinct(doi,word,.keep_all=F) %>%
    group_by(doi) %>% count(word) %>%
    spread(doi,n,fill=0) 
  
  distances.mat = as.matrix(to_stat[,-1])
  distances.doi = colnames(to_stat[,-1])
  
  #calc cos similarity
  n <- ncol(distances.mat) 
  cmb <- expand.grid(i=1:n, j=1:n) %>% filter(i<j)

  cos.sim.vec <- apply(cmb,1,function(x) cos.sim(x,distances.mat))
  
  to_plot = cmb %>% add_column(sim=cos.sim.vec)
  
  return(list(to_plot=to_plot,doi.list=distances.doi))
}

stats_section.sim = lapply(1:10, function(tt)calc_dist_mat(topicNumber=tt))

save(stats_section.sim,file='manuscript/plos.cosinesim.10topics.rda')


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
