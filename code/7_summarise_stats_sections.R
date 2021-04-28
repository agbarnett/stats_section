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

# Attached information of subejct classifications
#take subject classifications from meta-data afind top keywords/classifications per topic
load('../data/plos_meta_data.rda')

#remove level1 subject classifications common to all records (e.g biology and life sciences)
meta_dat_allrecords = meta_dat_allrecords %>% mutate(level_1 = paste0('/',
                                                                      str_replace_all(subject_level_1,pattern=',',replacement = '/|/'),
                                                                      '/'))
meta_dat_allrecords = meta_dat_allrecords %>% mutate(subjects = str_remove_all(subject,level_1), 
                                                     subjects = str_replace_all(subjects,pattern='/',replacement = ','),
                                                     subjects = str_split(subjects,',')) 



#join with matches
matches = matches %>% left_join(meta_dat_allrecords %>% select(doi,subject_level_1,subjects),by='doi')

#TODO
#extract most common words or search targeted words within a topic
subjects.bytopic = matches %>% group_by(doi,topic_id) %>% summarise(subjects_all = unlist(subjects),.groups='drop') %>%
  count(topic_id,subjects_all) %>% arrange(topic_id,-n)

# #not run:examples of targeted search
# #animal model
# subjects.bytopic %>% filter(grepl('animal models|chicken models|mouse models|pig models',fixed=F,ignore.case=T,subjects_all))
# #clinical trials
# subjects.bytopic %>% filter(grepl('clinical trial',fixed=F,ignore.case=T,subjects_all))
# #studies (e.g. cohort studies, animal studies)
# subjects.bytopic %>% filter(grepl('studies|study',fixed=F,ignore.case=T,subjects_all))
# #surveys
# subjects.bytopic %>% filter(grepl('survey',fixed=F,ignore.case=T,subjects_all))
# #genetics
# subjects.bytopic %>% filter(grepl('DNA|RNA|protein|genome|genomic|genetic',fixed=F,ignore.case=T,subjects_all))
# #cell biology
# subjects.bytopic %>% filter(grepl('cells',fixed=F,ignore.case=T,subjects_all))


#add study type to matches
study.types = matches %>% group_by(doi) %>% summarise(subjects_all = unlist(subjects),.groups='drop') %>%
  filter(grepl('clinical trial|studies|study|survey|meta-analysis',fixed=F,ignore.case=T,subjects_all)) %>%
  distinct(doi,subjects_all) %>%
  rename(study_type = subjects_all)

#add analysis types
analysis.types = matches %>% group_by(doi) %>% summarise(subjects_all = unlist(subjects),.groups='drop') %>%
  filter(grepl('analysis|analyses',fixed=F,ignore.case=T,subjects_all)) %>%
  distinct(doi,subjects_all) %>%
  rename(analysis_type = subjects_all)

#add animal models
animal.models = matches %>% group_by(doi) %>% summarise(subjects_all = unlist(subjects),.groups='drop') %>%
  filter(grepl('animal models|chicken models|mouse models|pig models',fixed=F,ignore.case=T,subjects_all)) %>%
  distinct(doi,subjects_all) %>%
  rename(model_type = subjects_all)

#add genetic studies
genetic.studies = matches %>% group_by(doi) %>% summarise(subjects_all = unlist(subjects),.groups='drop') %>%
  filter(grepl('DNA|RNA|protein|genome|genomic|genetic',fixed=F,ignore.case=T,subjects_all)) %>%
  distinct(doi,subjects_all) %>%
  rename(study_type = subjects_all)

#save
save(matches,study.types,analysis.types,animal.models,genetic.studies,file='../results/plos.results.10topics.rda')

#examples of boilerplate - spectrum analysis techniques (n = 4173)
filter(analysis.types,analysis_type=='Spectrum analysis techniques') %>%
  left_join(matches,by='doi') %>% arrange(topic_id,rank) %>% View()

# #Bioassays and physiological analysis (n = 6838)
# filter(analysis.types,analysis_type=='Bioassays and physiological analysis') %>% 
#   left_join(matches,by='doi') %>% arrange(topic_id,rank) %>% View()
# 
# #survey research
# filter(study.types,study_type %in% c('Suverys','Survey research')) %>% 
#   left_join(matches,by='doi') %>% arrange(topic_id,rank) %>% View()
# 
# #animal studies
# filter(study.types,study_type == 'Animal studies') %>% 
#   left_join(matches,by='doi') %>% arrange(topic_id,rank) %>% View()
# 
# #clinical trials by topic
# filter(study.types,study_type=='Clinical trials') %>% 
#   left_join(matches,by='doi')  %>% arrange(topic_id,rank) %>% View()

#within each topic, find highest ranking doi with match to clinical trial
results.clinicaltrial %>% group_by(topic_id) %>% summarise(n = n(),highest_rank=min(rank),.groups='drop')

#example: genome-wide association studies
results.gwas = 
results.gwas %>% group_by(topic_id) %>% summarise(n = n(),highest_rank=min(rank),.groups='drop')


#COSINE SIMILARITY CODE

cos.sim <- function(ix,distances.mat) 
{
  A = distances.mat[,ix[1]]
  B = distances.mat[,ix[2]]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
} 

distances = tidy_matches %>% left_join(.,matches %>% select(doi,rank),by='doi') %>% 
  filter(rank<=500) %>% 
  arrange(rank)

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


#summarise for table
stats_section.sim.top500 = lapply(stats_section.sim,function(x) x$to_plot) %>% bind_rows(.,.id='topic')
stats_section.doi.top500 = lapply(stats_section.sim,function(x) x$doi.list) 

ftab.cosine.plos = stats_section.sim.top500 %>% group_by(topic) %>% summarise(Median=median(sim),Q1=quantile(sim,.25),Q3=quantile(sim,.75),
                                                                              pgt80=sum(sim>0.8),pgt90=sum(sim>0.9),peq1=sum(sim==1)) %>% arrange(as.numeric(topic),.groups='drop') %>%
  mutate_if(is.numeric,function(x) round(x,2))

ftab.cosine.plos = mutate(ftab.cosine.plos,'Median (IQR)' = paste0(Median,' (',Q1,' to ',Q3,')')) %>%
  select(topic,'Median (IQR)',pgt80,pgt90,peq1) %>%
  rename('Similarity > 0.8'=pgt80,'Similarity > 0.9' = pgt90, 'Similarity = 1' = peq1) 

#boilerplate text, pairwise
boilerplate = lapply(1:10,function(x) stats_section.sim.top500 %>% filter(topic==x,sim>0.5) %>% mutate(pair = row_number(),
                                                                               doi_1 = stats_section.doi.top500[[x]][i],
                                                                               doi_2 = stats_section.doi.top500[[x]][j]) %>% select(topic,pair,sim,doi_1,doi_2))
  

boilerplate.dois = bind_rows(boilerplate) ##%>% gather(variable,doi,-topic,-pair,-sim) %>% arrange(topic,-sim,pair)

#join to text
boilerplate.text.plos = boilerplate.dois %>% left_join(matches %>% select(doi,text_data,rank,words),by=c('doi_1'='doi')) %>%
  rename('stats_section_1'=text_data,'rank_1'=rank,'words_1'=words) %>%
  left_join(matches %>% select(doi,text_data,rank,words),by=c('doi_2'='doi')) %>%
  rename('stats_section_2'=text_data,'rank_2'=rank,'words_2'=words) %>%
  select(topic,pair,sim,doi_1,rank_1,words_1,doi_2,rank_2,words_2,stats_section_1,stats_section_2) %>% arrange(topic,-sim)

save(stats_section.sim,ftab.cosine.plos,boilerplate.text.plos,file='../results/plos.cosinesim.10topics.rda')

#save boilerplate text as separate excel file
write.xlsx(boilerplate.text.plos,file='../results/plos.boilerplate.xlsx')

#################### END COSINE SIMILARITY CODE ###########################


