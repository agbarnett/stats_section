# look for almost identical sentences using the Jaccard score
#  targeted searches of keywords
# May 2021
# Revised Jan 2022 (R1)
library(tidyverse)
library(tidytext)
library(stringdist) # for similarity scores
library(openxlsx)
library(textreuse) #updated function for jaccard_similarity (compare to stringdist)

#general functions
source('code/99_boilerplate_functions.R')

## data
stats_sections = read.table('data/stats_section_cleaned_anzctr.txt',header=T) 
topic_results = read.csv('results/anzctr_10topics.csv')

topic_results = topic_results %>% rename('number'=DOI,'topic_num'=clusterID) %>%
  mutate(topic_id = paste('Topic',topic_num)) %>% select(number,topic_id,value)

matches = left_join(stats_sections,topic_results,by='number')

#tidy up dataset to help with conversion to sentence level
combined <- cleanup_anzctr_results()

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
  group_by(topic_num,number) %>% summarise(text=str_c(word,collapse=' '),.groups='drop')

common_ngrams <- lapply(2:4,function(x)
  dat.ngram %>% unnest_tokens(ngram, text, token = "ngrams", n = x, n_min = x) %>%
    group_by(topic_num,ngram) %>% summarise(total_studies = length(unique(number)),.groups='drop') %>% 
    drop_na() %>% mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num,-total_studies))
names(common_ngrams)<-paste('ngram',2:4,sep='_')

#save(common_ngrams,file='results/anzctr.ngrams_R1.rda')

dat.sentences.all <- dat.sentences %>% unnest(text_data_clean_s)

common_ngrams_all <- bind_rows(common_ngrams)


#examples of boilerplate text by topic, across topics; updated to reflect examples in manuscript
boilerplate <- list()

common_ngrams_all %>% filter(topic_num==1)
boilerplate[[1]] <-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1,dataset='anzctr',
                                                 search_str_1='intention-to-treat',
                                                 search_str_2 = 'all analyses will be conducted on an intention-to-treat basis')

common_ngrams_all %>% filter(topic_num==2)
boilerplate[[2]] <-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=2,dataset='anzctr',
                                                 search_str_1='sample size',search_str_2 =  "the sample size is adjusted for a 10 percent drop-out rate")


common_ngrams_all %>% filter(topic_num==3)

boilerplate[[3]] <- identify_boilerplate_text(indata=dat.sentences.all,choose.topic=3,dataset='anzctr',
                                                 search_str_1='student t-test',search_str_2 =  "continuous normally distributed variables will be compared using student t-test and reported as means standard deviation while non-normally distributed data will be compared using wilcoxon rank-sum tests and reported as medians inter-quartile-range")


common_ngrams_all %>% filter(topic_num==4)
boilerplate[[4]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=4,dataset='anzctr',
                                                 search_str_1='95 percent',
                                                 search_str_2 = "at a confidence level of 95 percent and a precision around the estimate of 5 percent a minimum of 73 patients will be included")

common_ngrams_all %>% filter(topic_num==5)

boilerplate[[5]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=5,dataset='anzctr',
                                                 search_str_1='sample size calculation',
                                                 search_str_2 = "no formal sample size calculation was performed")

common_ngrams_all %>% filter(topic_num==6)

boilerplate[[6]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=6,dataset='anzctr',
                                                 search_str_1='median minimum and maximum',
                                                 search_str_2 ="continuous variables will be summarized by mean standard deviation median minimum and maximum")

common_ngrams_all %>% filter(topic_num==7)

boilerplate[[7]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=7,dataset='anzctr',
                                                 search_str_1='descriptive statistics',
                                                 search_str_2 = "descriptive statistics will be used")

common_ngrams_all %>% filter(topic_num==8)

boilerplate[[8]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=8,dataset='anzctr',
                                                 search_str_1='intention-to-treat',
                                                 search_str_2 = "all analyses will be conducted on an intention-to-treat basis")


common_ngrams_all %>% filter(topic_num==9)
boilerplate[[9]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=9,dataset='anzctr',
                                            search_str_1='linear mixed models',
                                            search_str_2 = "linear mixed models will be used to analyse the data")

common_ngrams_all %>% filter(topic_num==10)
dat.sentences.all %>% filter(topic_num==10,grepl('repeated measures(.*)anova',text_data_clean_s)) %>% count(text_data_clean_s,sort=T)
boilerplate[[10]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=10,dataset='anzctr',
                                            search_str_1='repeated measures anova',
                                            search_str_2 = "data will be analysed using standardised non-parametric or parametric statistical methods where appropriate  repeated measures anova")

#all topics
boilerplate[[11]]<-list()
boilerplate[[11]][[1]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,dataset='anzctr',
                                                  search_str_1='less-than',search_str_2 = 'a p-value less-than 0.05 will be considered statistically significant')
boilerplate[[11]][[2]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,dataset='anzctr',
                                                  search_str_1='intention-to-treat',search_str_2 = 'analyses will be conducted on an intention-to-treat basis')
boilerplate[[11]][[3]]<-identify_boilerplate_text(indata=dat.sentences.all,choose.topic=1:10,dataset='anzctr',
                                                  search_str_1='descriptive statistics',search_str_2 = 'baseline characteristics will be summarised using descriptive statistics')


#save boilerplate examples as a summary table for manuscript
#summarise boilerplate plate results
summary_boilerplate <- list() #added summary table to save in place of boilerplate (v. large file)
for (b in 1:length(boilerplate)){
  output = boilerplate[[b]]
  if(!is.null(output$summary.stat)){summary_boilerplate[[b]] = output$summary.stat}
  if(is.null(output$summary.stat)){summary_boilerplate[[b]] = lapply(output,function(x) x$summary.stat) %>% bind_rows()}
}
names(summary_boilerplate) <- c(1:10,'All topics')

summary_boilerplate = summary_boilerplate %>% bind_rows(.id='Topic')

## find instances of sentences that are an exact cut-and-paste
exact_matches_s = dat.sentences %>% select(number,topic_num,text_data_clean_s,n_words) %>% 
  unnest(c(text_data_clean_s,n_words)) %>%
  filter(text_data_clean_s!="") %>%
  group_by(topic_num,text_data_clean_s) %>% 
  summarise(n=length(unique(number)),number=list(unique(number)),.groups='drop') %>% filter(n>1) 



n.minhash = 300
n.bands = 50

random.seed = TeachingDemos::char2seed('jaccard',set=F)
minhash <- minhash_generator(n = n.minhash, seed = random.seed)


# Document-level analysis, all topics
# i) exact matches first (i.e. direct cut and paste). with and without stop words, numbers
# as is
exact_matches_doc = combined %>% group_by(topic_num,text) %>% 
  summarise(n = length(unique(number)),total_studies = list(number)) %>% filter(n>1)

exact_matches_doc %>% unnest(total_studies) %>% count(topic_num)


#loop over topics to find close matches at the document level
jaccard_doc = lapply(1:10, function(x) calc_document_jaccard(indata=combined,choose.topic=x,minhash = minhash,n.bands = n.bands,dataset='anzctr',token='words'))

#for a given topic, find the total number of documents with a jaccard score >=0.9 (i.e. unique documents, regardless of pairwise matches)
boilerplate_doc = lapply(1:10,function(x) jaccard_doc[[x]]$similarities %>% filter(score>=0.9))
matches_doc = lapply(1:10, function(x) jaccard_doc[[x]]$dat %>% filter(id %in% boilerplate_doc[[x]]$a|id %in% boilerplate_doc[[x]]$b)) %>% bind_rows()

#make a table showing results
combined = mutate(combined,words=str_count(text,'\\w+'))
combined %>% summarise(med=median(words),q1=quantile(words,.25),q3=quantile(words,.75),.groups='drop')
word_counts  = combined %>% group_by(topic_num) %>% summarise(med=median(words),q1=quantile(words,.25),q3=quantile(words,.75),.groups='drop')

a<-exact_matches_doc %>% unnest(total_studies) %>% select(topic_num,text,total_studies) %>% rename('number'=total_studies)
b<-matches_doc %>% select(topic_num,text,number)
summary_matches_doc = bind_rows(a,b) %>% distinct(topic_num,number) %>% count(topic_num,name='total_studies')
#add word counts
summary_matches_doc = full_join(word_counts,summary_matches_doc,by='topic_num') %>% mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num)

#save selected topics
save(exact_matches_doc,jaccard_doc,matches_doc,summary_matches_doc,file='results/jaccard_section_anzctr.rda')


#for all studies at the sentence level, calculate jaccard similarity for distinct pairs of sentences; tokens at word level
jaccard_s = lapply(1:10, function(x) jaccard_sentence_all(indata=dat.sentences,choose.topic=x,minhash=minhash,n.bands=n.bands,dataset='anzctr'))

#identify all studies with a jaccard score of 0.8 or higher (1+ pairwise comparisons)
boilerplate_s = lapply(1:10,function(x) jaccard_s[[x]]$similarities %>% filter(score>=0.9))
matches_s <- lapply(1:10, function(x) jaccard_s[[x]]$dat %>% filter(id %in% boilerplate_s[[x]]$a|id %in% boilerplate_s[[x]]$b)) %>% bind_rows(.,.id='topic_num')

#make a table showing results
#total_studies: number of studies per topic with at least one matching sentence
a<-exact_matches_s %>% unnest(number) %>% select(topic_num,number,text_data_clean_s) %>% rename(text=text_data_clean_s)
b<-matches_s %>% unnest(number) %>% select(topic_num,text,number,text)
dat.matches_s <- bind_rows(a,b) %>% distinct()

summary_matches_s = dat.matches_s %>% distinct(topic_num,number) %>% count(topic_num,name='total_studies') %>%
  mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num)

#exact matches by topic
ad<- a %>% distinct(topic_num,number) %>% count(topic_num,name='total_studies_exact') %>% mutate(topic_num=as.numeric(topic_num))
summary_matches_s = summary_matches_s %>% left_join(ad,by='topic_num')

#summarise total sentences per topic
total_sentences_bytopic <- dat.sentences %>% unnest(text_data_clean_s) %>% group_by(topic_num,number) %>% summarise(total_sentences = n(),.groups='drop_last')

total_sentences_bytopic = total_sentences_bytopic %>% summarise(med = median(total_sentences),q1=quantile(total_sentences,.25),q3=quantile(total_sentences,.75))

save(summary_boilerplate,summary_matches_s,total_sentences_bytopic,file='results/jaccard_sentence_anzctr.rda')

#plot number of matching sentences per topic

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

to_plot = dat.matches %>% group_by(topic_num) %>% count(number,name='total_sentences') %>% count(total_sentences)

matching.studies <- dat.matches[['number']]
notmatching.studies <- filter(combined,!number %in% matching.studies) %>% distinct(topic_num,number) %>% count(topic_num)
ad <- notmatching.studies %>% add_column(total_sentences=0,.before='n')

to_plot <- bind_rows(to_plot,ad)
to_plot = mutate(to_plot,var = ifelse(total_sentences<3,as.character(total_sentences),'3+'),topic_num=as.numeric(topic_num))
g1 <- ggplot(to_plot,aes(x=topic_num,y=n,fill=forcats::fct_rev(var)))+geom_col(position='fill')+scale_y_continuous('Percentage of studies in topic',breaks = seq(0, 1, .1),labels=scales::label_percent(accuracy=1))+
  scale_x_continuous('Topic',breaks=1:10)+scale_fill_manual(values=cbPalette[c(4,3,2,1)])+
  theme_minimal()+
  guides(fill=guide_legend(title="Matching sentences per section",reverse = TRUE))+
  theme(legend.position='top',legend.direction = 'horizontal',axis.text=element_text(size=12),text = element_text(size=12))

# #save as jpeg
# tiff('manuscript/plos_one/revised figures/S4_Fig.tif',width=480,height=480)
# g1
# dev.off()

# review the most common matching sentences within each topic
# start by looking at ngram to narrow down the search, excluding stop words
dat.ngram_matches = dat.matches %>% mutate(word=lapply(text,function(.) str_split(.,' ') %>% unlist()))
dat.ngram_matches <- dat.ngram_matches %>%
  unnest(word) %>% 
  anti_join(.,tidytext::stop_words,by='word') %>%
  #filter(!grepl('[[:digit:]]+',word)) %>% #keep numbers in
  group_by(topic_num,number) %>% summarise(text=str_c(word,collapse=' '),.groups='drop')

common_ngrams_matches <- lapply(2:4,function(x)
  dat.ngram_matches %>% unnest_tokens(ngram, text, token = "ngrams", n = x, n_min = x) %>%
    group_by(topic_num,ngram) %>% summarise(total_studies = length(unique(number)),.groups='drop') %>% 
    drop_na() %>% mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num,-total_studies))
names(common_ngrams_matches)<-paste('ngram',2:4,sep='_')

common_ngrams_matches[['ngram_2']] %>% group_by(ngram) %>% summarise(n = sum(total_studies)) %>% arrange(-n) %>% View()
#dat.sentences %>% filter(grepl('confidence level of(.*)percent and a precision around the estimate of(.*)percent a minimum of(.*)patients',text)) %>% select(text) %>% View()

summarise_matches<-function(indata_s=dat.sentences,indata_m = dat.matches,search_str){
  #total sentences that match search_str
 total_hits_str = indata_s %>% filter(grepl(search_str,text)) %>% distinct(topic_num,number) %>% count(topic_num,name='ngram')
 total_hits_jaccard = indata_m %>% filter(grepl(search_str,text)) %>% distinct(topic_num,number)
 #out = total_hits_str %>% left_join(.,total_hits_jaccard %>% count(topic_num,name='close_match'),by='topic_num') %>% mutate(close_match=replace_na(close_match,0))
 return(total_hits_jaccard)
}

ngram_matches<- list()
ngram_matches[['Statistical significance']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,search_str= '\\bp(.*)less-than 0.05\\b|\\bstatistical(.*)significan(.*)\\b|
                                                               \\bp(.*)value\\b|\\btwo(.*)tailed\\b|\\btwo(.*)sided\\b|\\bone(.*)tailed\\b|\\bone(.*)sided\\b')
ngram_matches[['Intention to treat/per-protocol analysis']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,search_str= '\\bintention-to-treat|per-protocol\\b')
ngram_matches[['95 percent confidence level']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,search_str= '95 percent')
ngram_matches[['Percentage power']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,search_str= '\\bpercent power\\b|\\b80 percent chance\\b|\\bpower(.*)80 percent\\b')
ngram_matches[['Descriptive statistics']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,
                                                             search_str= '\\bdescriptive statistics\\b|\\bplus-or-minus\\b|\\bmean\\b|\\bmedian\\b|\\bstandard deviation\\b|\\binter-quartile range\\b|\\bminimum and maximum\\b|\\bfrequencies\\b|\\bpercentages\\b')

ngram_matches[['Group-based hypothesis tests']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,
                                                              search_str= '\\bchi-square\\b|\\bfisher exact\\b|\\bt-test\\b|\\bu-test\\b|\\blog-rank test\\b|\\bkruskal-wallis test\\b')

ngram_matches[['Linear modelling/regression']]<-summarise_matches(indata_s=dat.sentences,indata_m = dat.matches,search_str= '\\blinear(.*)model\\b|\\banova\\b|\\bregression\\b')


to_plot_ngram_matching <- bind_rows(ngram_matches,.id='n-gram theme') %>% mutate(topic_num = factor(topic_num,levels=1:10)) %>%
  group_by(topic_num,number) %>% summarise(theme=list(`n-gram theme`),.groups='drop') 

topic_labels <-c('Qualitative methods','Sample size calculations','Students t-test','Efficacy and safety studies','Pilot studies',
                 'Safety and tolerability studies','Descriptive analysis','Intervention studies','Linear models','Analysis of variance')


#study design topics only: 4,5,6,8
to_plot<-to_plot %>% mutate(topic_label = factor(topic_num,levels=1:10,labels=topic_labels))
study_designs<-to_plot %>% filter(topic_num %in% c(2,4,5,6,8))
theme_order<-study_designs %>% ungroup() %>% group_by(theme) %>% summarise(total = sum(n)) %>% arrange(-total) %>% pull(theme)
study_designs <- mutate(study_designs,theme=factor(theme,levels=rev(theme_order)))

g1<-study_designs %>%
  ggplot(aes(x=theme,y=n,fill=topic_label))+geom_col()+coord_flip()+
  scale_fill_manual(values=cbPalette)+
  xlab('')+ylab('Number of close matches, sentence level')+g.theme+theme(legend.title = element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))

#quant/qual methods: 1,3,7,9,10
study_methods<-to_plot %>% filter(topic_num %in% c(1,3,7,9,10))
theme_order<-study_methods %>% ungroup() %>% group_by(theme) %>% summarise(total = sum(n)) %>% arrange(-total) %>% pull(theme)
study_methods <- mutate(study_methods,theme=factor(theme,levels=rev(theme_order)))

g2<-study_methods %>%
  ggplot(aes(x=theme,y=n,fill=topic_label))+geom_col()+coord_flip()+
  scale_fill_manual(values=cbPalette)+
  xlab('')+ylab('Number of close matches, sentence level')+g.theme+theme(legend.title = element_blank())+
  guides(fill=guide_legend(nrow=2,byrow=TRUE))


# tiff('manuscript/plos_one/revised figures/S5_Fig_R1.tif',width=1000,height=900)
# ggpubr::ggarrange(g1,g2,nrow=2,labels=c('A','B'))
# dev.off()
