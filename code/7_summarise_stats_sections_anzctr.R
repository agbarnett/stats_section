#7_summarise_stats_sections.R
library(tidyverse)
library(tidytext)
library(openxlsx)

dat = readRDS('../data/stats_section_anzctr_cleaned.rds')
#ten topics, PLOS ONE
matches = read.csv('../results/anzctr_10topics.csv',header=T)

matches = left_join(dat,matches,by=c('number'='DOI')) %>%
  mutate(value = as.numeric(value)) %>%
  distinct(number,.keep_all = T)

tidy_matches = matches %>%
  unnest_tokens(word,text_data_clean)

wordcounts <- tidy_matches %>%
  group_by(topic_id,number) %>%
  summarise(words = n(),.groups='drop')

matches = left_join(matches,wordcounts,by=c('number','topic_id')) %>%
  arrange(topic_id,-value) %>%
  group_by(topic_id) %>% mutate(rank=row_number()) %>% ungroup() %>%
  mutate(topic_id = paste('Topic',topic_id))


cos.sim <- function(ix,distances.mat) 
{
  A = distances.mat[,ix[1]]
  B = distances.mat[,ix[2]]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
} 

distances = tidy_matches %>% left_join(.,matches %>% select(number,rank),by='number') %>% filter(rank<=500) %>% arrange(rank)

calc_dist_mat <- function(indata=distances,topicNumber){
  to_stat = indata %>% filter(topic_id==topicNumber) %>%
    distinct(number,word,.keep_all=F) %>%
    group_by(number) %>% count(word) %>%
    spread(number,n,fill=0) 
  
  distances.mat = as.matrix(to_stat[,-1])
  distances.number = colnames(to_stat[,-1])
  
  #calc cos similarity
  n <- ncol(distances.mat) 
  cmb <- expand.grid(i=1:n, j=1:n) %>% filter(i<j)
  
  cos.sim.vec <- apply(cmb,1,function(x) cos.sim(x,distances.mat))
  
  to_plot = cmb %>% add_column(sim=cos.sim.vec)
  
  return(list(to_plot=to_plot,number.list=distances.number))
}

stats_section.sim = lapply(1:10, function(tt)calc_dist_mat(topicNumber=tt))
#summarise for table
stats_section.sim.top500 = lapply(stats_section.sim,function(x) x$to_plot) %>% bind_rows(.,.id='topic')
stats_section.number.top500 = lapply(stats_section.sim,function(x) x$number.list) 

ftab.cosine.anzctr = stats_section.sim.top500 %>% group_by(topic) %>% summarise(Median=median(sim),Q1=quantile(sim,.25),Q3=quantile(sim,.75),
                                                                              pgt80=sum(sim>0.8),pgt90=sum(sim>0.9),peq1=sum(sim==1)) %>% arrange(as.numeric(topic),.groups='drop') %>%
  mutate_if(is.numeric,function(x) round(x,2))

ftab.cosine.plos = mutate(ftab.cosine.plos,'Median (IQR)' = paste0(Median,' (',Q1,' to ',Q3,')')) %>%
  select(topic,'Median (IQR)',pgt80,pgt90,peq1) %>%
  rename('Similarity > 0.8'=pgt80,'Similarity > 0.9' = pgt90, 'Similarity = 1' = peq1) 

#exact matches, topic 1
boilerplate = lapply(1:10,function(x) stats_section.sim.top500 %>% filter(topic==x,sim>0.8) %>% mutate(pair = row_number(),
                                                                                                       num.i = stats_section.number.top500[[x]][i],
                                                                                                       num.j = stats_section.number.top500[[x]][j]) %>% 
                       select(topic,pair,sim,num.i,num.j))


boilerplate.numbers = bind_rows(boilerplate) %>% gather(variable,number,-topic,-pair,-sim) %>% arrange(topic,-sim,pair) %>% select(-variable)

#join to text
boilerplate.text.anzctr = boilerplate.numbers %>% left_join(matches %>% select(number,text_data,text_data_clean,rank,words),by='number')

save(matches,file='../results/anzctr.results.10topics.rda')
save(stats_section.sim,ftab.cosine.anzctr,boilerplate.text.anzctr,file='../results/anzctr.cosinesim.10topics.rda')

#save boilerplate text as separate excel file
write.xlsx(boilerplate.text.anzctr,file='../results/anzctr.boilerplate.xlsx')

