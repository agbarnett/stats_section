# look for almost identical sentences using the Jaccard score
#  targeted searches of keywords
# May 2021
library(tidyverse)
library(tidytext)
library(textreuse)
library(ggupset)

#general functions
source('code/99_boilerplate_functions.R')

#define settings for textreuse -> local senstivity hashing
n.minhash = 300
n.bands = 50
random.seed = TeachingDemos::char2seed('jaccard',set=F)
minhash <- minhash_generator(n = n.minhash, seed = random.seed)

g.theme = theme_bw(base_size=12)+theme(legend.position = 'top',legend.direction = 'horizontal',legend.text = element_text(size=10),
                           strip.background = element_rect(fill='white'))

## data
stats_sections = readRDS('data/stats_section_cleaned.rds')
topic_results <- read.csv('results/plos_one_10topics.csv')

topic_results = topic_results %>% rename('doi'=DOI,'topic_num'=topic_id) %>%
  mutate(topic_id = paste('Topic',topic_num)) %>% select(doi,topic_id,value)

matches = left_join(stats_sections,topic_results,by='doi') 


#data management
combined <- cleanup_plos_results()

# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% mutate(text_data_clean_s = str_split(text,"(?<=\\.)\\s(?=[a-z])",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))

#tidy up sentence formatting
dat.sentences = mutate(dat.sentences,text_data_clean_s = lapply(text_data_clean_s,function(x) str_remove_all(x,pattern = '^[[:digit:]]\\.|\\.$')))
dat.sentences = mutate(dat.sentences,text_data_clean_s = lapply(text_data_clean_s,function(x) str_remove_all(x,pattern = '^-\\s?|\\.\\s?-\\s+')))

#split each sentence into individual words, before starting n-gram analysis by topic
dat.sentences = dat.sentences %>% mutate(word=lapply(text_data_clean_s,function(.) str_split(.,' ') %>% unlist()))

#n-gram analysis by topic, excluding stop words
dat.ngram = dat.sentences %>% 
  unnest(word) %>% 
  anti_join(.,tidytext::stop_words,by='word') %>%
  #filter(!grepl('[[:digit:]]+',word)) %>% #keep numbers in
  group_by(topic_num,doi_num) %>% summarise(text=str_c(word,collapse=' '),.groups='drop')

common_ngrams <- lapply(2:4,function(x)
  dat.ngram %>% unnest_tokens(ngram, text, token = "ngrams", n = x, n_min = x) %>%
    group_by(topic_num,ngram) %>% summarise(total_studies = length(unique(doi_num)),.groups='drop') %>% 
    drop_na() %>% mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num,-total_studies))
names(common_ngrams)<-paste('ngram',2:4,sep='_')
common_ngrams.all <- bind_rows(common_ngrams)

#save('results/plos.ngrams.rda')


#targeted searches for boilerplate text. semi-supervised based on common ngrams within a topic (see 8_plos_ngrams.R for saved outputs)
dat.sentences.all <- dat.sentences %>% unnest(c(text_data_clean_s,n_words))

boilerplate <- list()

## topic 1: student t test
common_ngrams.all %>% filter(topic_num==1) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==1,grepl('t-test',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)

dat.sentences.all %>% filter(topic_num==1,grepl('t-test',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()


boilerplate[[1]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1,
                                              search_str_1='student t-test',search_str_2 = 'statistical analysis was performed using student t-test')

## topic 2: general modelling
common_ngrams.all %>% filter(topic_num==2) %>% arrange(-total_studies)

dat.sentences.all %>% filter(topic_num==2,grepl('chi(.*)square',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)
dat.sentences.all %>% filter(topic_num==2,grepl('logistic regression',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)
dat.sentences.all %>% filter(topic_num==2,grepl('continuous variables were',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)

dat.sentences.all %>% filter(topic_num==2,grepl('continuous variables',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()


boilerplate[[2]]<-list()

boilerplate[[2]][[1]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=2,
                                                   search_str_1='categorical variables were compared',search_str_2 = 'categorical variables were compared using the chi-square test or fisher exact test')
boilerplate[[2]][[2]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=2,
                                                 search_str_1='logistic regression',search_str_2 = 'logistic regression was used for multivariate analysis')
boilerplate[[2]][[3]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=2,
                                                   search_str_1='continuous variables(.*)plus-or-minus',search_str_2 = 'continuous variables were expressed as mean plus-or-minus standard deviation')
boilerplate[[2]][[4]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=2,
                                                   search_str_1='continuous variables were compared',search_str_2 = 'continuous variables were compared using student t-test')
boilerplate[[2]][[5]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=2,
                                                   search_str_1='frequencies and percentages',search_str_2 = 'categorical variables were expressed as frequencies and percentages')

## topic 3: graphpad prism
common_ngrams.all %>% filter(topic_num==3) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==3,grepl('graphpad prism',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)
dat.sentences.all %>% filter(topic_num==3,grepl('graphpad prism',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()

boilerplate[[3]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=3,search_str_1='graphpad prism',
                                                 search_str_2 = 'all statistical analyses were performed using graphpad prism software')

## topic 4: one-way anova
common_ngrams.all %>% filter(topic_num==4) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==4,grepl('one-way',text_data_clean_s)) %>% count(text_data_clean_s,sort=T) 
dat.sentences.all %>% filter(topic_num==4,grepl('one-way',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()

boilerplate[[4]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=4,
                          search_str_1='one-way',search_str_2 = 'one-way analysis-of-variance anova was used for multiple comparisons and a tukey post-hoc test was applied where appropriate')


#topic 5: spss
common_ngrams.all %>% filter(topic_num==5) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==5,grepl('spss',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)


boilerplate[[5]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=5,
                          search_str_1='spss',search_str_2 = 'statistical analyses were performed using spss 17.0 spss inc chicago il usa')


#topic 6: descriptive stats
common_ngrams.all %>% filter(topic_num==6) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==6,grepl('plus-or-minus',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()
dat.sentences.all %>% filter(topic_num==6,grepl('frequenc(.*)',text_data_clean_s)) %>% distinct(doi_num) 

boilerplate[[6]]<-list()
boilerplate[[6]][[1]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=6,
                          search_str_1='plus-or-minus',search_str_2 = 'data are expressed as mean plus-or-minus sem')
boilerplate[[6]][[2]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=6,
                                                 search_str_1='plus-or-minus',search_str_2 = 'data are presented as mean plus-or-minus sem')

#topic 7 meta analysis
common_ngrams.all %>% filter(topic_num==7) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==7,grepl('95 percent',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()

boilerplate[[7]] <- list()
boilerplate[[7]][[1]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=7,
                          search_str_1='95 percent',search_str_2 = 'summary estimates including 95 percent confidence intervals cis were calculated')
boilerplate[[7]][[2]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=7,
                                                 search_str_1='95 percent',search_str_2 = 'a 95 percent ci was used for statistical significance test and a 95 percent ci without 1 for or indicating a significant increased or reduced cancer risk')

#topic 8: modelling
common_ngrams.all %>% filter(topic_num==8) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==8,grepl('equal-to',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()
dat.sentences.all %>% filter(topic_num==8,grepl('t-test',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)
dat.sentences.all %>% filter(topic_num==8,grepl('analyses performed',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)

boilerplate[[8]]<-list()
boilerplate[[8]][[1]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=8,
                          search_str_1='equal-to 0.05',search_str_2 = "the significance level was set at p equal-to 0.05")
boilerplate[[8]][[2]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=8,
                                                 search_str_1='t-test',search_str_2 = "student t-test was used for statistical analysis")

#topic 9: statistical significance
common_ngrams.all %>% filter(topic_num==9) %>% arrange(-total_studies)
dat.sentences.all %>% filter(topic_num==9,grepl('less-than',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()
boilerplate[[9]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=9,
                          search_str_1='less-than',search_str_2 = "p less-than 0.05 p less-than 0.01 p less-than 0.001")

# topic 10: experimental study design
common_ngrams.all %>% filter(topic_num==10) %>% arrange(-total_studies)

dat.sentences.all %>% filter(topic_num==10,grepl('independent experiments',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()
dat.sentences.all %>% filter(topic_num==10,grepl('t-test',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)

boilerplate[[10]]<-list()
boilerplate[[10]][[1]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=10,
                          search_str_1='independent experiments',search_str_2 = 'all data are representative of at least three independent experiments')
boilerplate[[10]][[2]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=10,
                                                  search_str_1='t-test',search_str_2 = 'statistical analysis was performed using student t-test')


## all topics
common_ngrams.all %>% group_by(ngram) %>% summarise(n=sum(total_studies)) %>% arrange(-n) %>% View()

dat.sentences.all %>% filter(grepl('less-than',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()
dat.sentences.all %>% filter(grepl('t-test',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()
dat.sentences.all %>% filter(grepl('plus-or-minus',text_data_clean_s)) %>% distinct(doi_num) %>% nrow()

boilerplate[[11]]<-list()
boilerplate[[11]][[1]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,
                                             search_str_1='less-than',search_str_2 = 'a p-value less-than 0.05 was considered statistically significant')
boilerplate[[11]][[2]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,
                                             search_str_1='t-test',search_str_2 = 'statistical analysis was performed using student t-test')
boilerplate[[11]][[3]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,
                                             search_str_1='plus-or-minus',search_str_2 = 'data are presented as mean plus-or-minus sem')
boilerplate[[11]][[4]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,
                                             search_str_1='plus-or-minus',search_str_2 = 'data are expressed as mean plus-or-minus sem')


exact_matches_s = dat.sentences %>% select(doi_num,topic_num,text_data_clean_s,n_words) %>% 
  unnest(c(text_data_clean_s,n_words)) %>%
  filter(text_data_clean_s!="") %>%
  group_by(topic_num,text_data_clean_s) %>% 
  summarise(n=length(unique(doi_num)),number=list(unique(doi_num)),.groups='drop') %>% filter(n>1) 


#save finalised results
#number of studies by topic
n_topic = combined %>% distinct(doi_num,topic_num) %>% count(topic_num)
n_exact_s = exact_matches_s %>% unnest(number) %>% distinct(number,topic_num) %>% count(topic_num,name='total_matches_exact')

summary_matches_s = full_join(n_topic,n_exact_s,by='topic_num')

save(boilerplate,summary_matches_s,file='results/jaccard_sentence_plos.rda')



# Document-level analysis, all topics
# i) exact matches first (i.e. direct cut and paste). with and without stop words, numbers
# as is
exact_matches_doc = combined %>% group_by(topic_num,text) %>% 
  summarise(n = length(unique(doi_num)),total_studies = list(doi_num),.groups='drop') %>% filter(n>1)

#loop over topics
jaccard_doc = lapply(1:10, function(x) calc_document_jaccard(indata=combined,choose.topic=x,minhash = minhash,n.bands = n.bands,dataset='plos',token='words'))

#for a given topic, find the total number of documents with a jaccard score >=0.9 (i.e. unique documents, regardless of pairwise matches)
boilerplate_doc = lapply(1:10,function(x) jaccard_doc[[x]]$similarities %>% filter(score>=0.9))
matches_doc = lapply(1:10, function(x) jaccard_doc[[x]]$dat %>% filter(id %in% boilerplate_doc[[x]]$a|id %in% boilerplate_doc[[x]]$b)) %>% bind_rows()

#make a table showing results
combined = mutate(combined,words=str_count(text,'\\w+'))
word_counts  = combined %>% group_by(topic_num) %>% summarise(med=median(words),q1=quantile(words,.25),q3=quantile(words,.75),.groups='drop')
a<-exact_matches_doc %>% unnest(total_studies) %>% select(topic_num,text,total_studies) %>% rename('doi_num'=total_studies)
b<-matches_doc %>% select(topic_num,text,doi_num)
summary_matches_doc = bind_rows(a,b) %>% distinct(topic_num,doi_num) %>% count(topic_num,name='total_studies')
#add word counts
summary_matches_doc = full_join(word_counts,summary_matches_doc,by='topic_num') %>% mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num)

#add exact matches
ad<- a %>% distinct(topic_num,doi_num) %>% count(topic_num,name='total_studies_exact') %>% mutate(topic_num=as.numeric(topic_num))
summary_matches_doc = summary_matches_doc %>% left_join(ad,by='topic_num')

#add total studies by topic
summary_matches_doc = left_join(n_topic,summary_matches_doc,by='topic_num')


#save selected topics
save(exact_matches_doc,jaccard_doc,matches_doc,summary_matches_doc,file='results/jaccard_section_plos.rda')


#overall word counts
combined %>% filter(words<=50) %>% nrow()
combined %>% filter(words>=500) %>% nrow()

combined %>% summarise(med = median(words),q1=quantile(words,.25),q3=quantile(words,.75))
