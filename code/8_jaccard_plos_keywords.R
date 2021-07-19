# look for almost identical sentences using the Jaccard score
#  targeted searches of keywords
# May 2021
library(tidyverse)
library(tidytext)
library(stringdist) # for similarity scores
library(openxlsx)
library(textreuse) #updated function for jaccard_similarity (compare to stringdist)

## data
## a) get the example data  
#load('data/processed_examples.RData') # from 7_example_sentences.R
## b) get all the data
load(file='results/plos.results.10topics.rda')

# data management 
matches = ungroup(matches) %>%
  mutate(topic_num = as.numeric(str_remove(topic_id, pattern='Topic ')), # topic number
         doi_num = str_remove(doi, pattern='10.1371/journal\\.pone\\.')) # just get the number from the DOI

#tidy up a bit more to account for overuse of full stops (affects breakdown by sentence)
#single number followed by full stop (e.g. [space]1.[space]); reomve number
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})([:digit:]{1})(\\.)(\\s{1})",replacement="\\3\\4"))

#a single [a-z] character, predeeced and followed by a full stop[space]
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})([:alpha:]{1})(\\.)(\\s{1})",replacement="\\1\\2\\4"))
#instances where the entire sentence is a number e.g ver. 23.0
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\.)(\\s{1})([:digit:]+)(\\.)",replacement="\\2\\3\\4"))

#remove final full stop
matches = mutate(matches,
                 text_data_clean = str_remove_all(text_data_clean,pattern='\\.$'))

#replace e.g. and i.e.
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern='e.g.\\s+|i\\.e\\.\\s+|\\s+ver\\.\\s+',replacement = ' '))
#statcorp
matches =mutate(matches,
                text_data_clean = str_replace_all(text_data_clean,pattern="(\\.)(\\s{1})(college station tx)",replacement="\\2\\3"))

# sas ibm corp
matches =mutate(matches,
                text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(ibm corp)(\\.)",replacement="\\1\\2"))

#inc. or ver.
matches =mutate(matches,
                text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(inc|ver)(\\.)",replacement="\\1\\2"))

# combine paragraphs from the same paper (with the same DOI)
combined = group_by(matches, doi_num, topic_id, topic_num,rank) %>%
  summarise(text = toString(text_data_clean),.groups='drop')

# end data management

# Document-level analysis, all topics
n.minhash = 1000
n.bands = 100

random.seed = TeachingDemos::char2seed('jaccard',set=F)

minhash <- minhash_generator(n = n.minhash, seed = random.seed)

#review chosen number of minhashes and bands
lsh_threshold(h=n.minhash,b=n.bands) #expected jaccard index
lsh_probability(h=n.minhash,b=n.bands,s=0.75) #probability of a match based on chosen score cutoff

calc_document_jaccard <- function(indata=combined,choose.topic,minhash,n.bands){
  dat.topic = indata %>% filter(topic_num==choose.topic) %>% distinct(doi_num,.keep_all=T) %>%
    mutate(id=row_number())

  text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                tokenizer = tokenize_words,minhash_func = minhash)
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>% 
    arrange(desc(score))  
  
  
  return(list(similarities=jacsim,dat=dat.topic))
}

#loop over topics
jaccard_doc = lapply(1:10, function(x) calc_document_jaccard(indata=combined,choose.topic=x,minhash = minhash,n.bands = n.bands))

#for a given topic, find total exact matches, total with score >=0.75
#then find most frequently matched section as example of boilerplate text

summarise_jaccard_matches = function(indata,cutoff=0.75){
  indata.j = indata$similarities
  indata.t = indata$dat
  
  tab = indata.j %>% summarise(exact_matches = sum(score==1),similar=sum(score>=cutoff),.groups='drop')
  
  topmatches = indata.j %>% 
    filter(score>=cutoff) %>%
    mutate(pair=row_number()) %>% gather(variable,value,-pair,-score) %>% count(value,sort=T) %>%
    mutate(value=as.numeric(str_remove_all(value,pattern='doc-')))
  
  topmatches_example = indata.t %>% filter(id==topmatches[['value']][1]) %>% pull(text)
  topmatches_doi = indata.t %>% filter(id==topmatches[['value']][1]) %>% pull(doi_num)
  tab = tab %>% add_column(doi_num=topmatches_doi,text=topmatches_example)
  return(tab)
}

#results by topic
summary.tab = lapply(jaccard_doc,function(x) summarise_jaccard_matches(indata=x,cutoff = 0.75))
summary.tab = bind_rows(summary.tab,.id='topic_num')

#save selected topics
save(jaccard_doc,summary.tab,file='results/jaccard_section_plos.rda')

#plot numbers and save
g.theme = theme(legend.position = 'top',legend.direction = 'horizontal',legend.text = element_text(size=10),
                           axis.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

to_plot = summary.tab %>% select(topic_num,exact_matches,similar) %>% 
  mutate(similar=similar-exact_matches) %>%
  gather(variable,value,-topic_num) %>%
  mutate(topic_num = factor(topic_num,levels=1:10),
         variable = factor(variable,levels=c('exact_matches','similar'),labels = c('Exact match','Jaccard score = 0.75 or higher')))


g1 = ggplot(to_plot,aes(x=topic_num,y=value,fill=variable))+geom_bar(stat='identity',width=0.9,colour='black')+
  scale_y_continuous('Matching statistical methods sections',breaks=seq(0,500,50))+
  scale_x_discrete('Topic')+scale_fill_grey(guide = guide_legend(reverse = TRUE) )+ g.theme +
  theme(legend.title = element_blank())

jpeg('manuscript/asa_template/figures/plosjaccardsectionlevel.jpg',width=480,height=600,quality=100)
g1
dev.off()

# Sentence-level analysis, all topics
## most common sentences
## sentence matches by words within topic clouds
calc_sentence_jaccard <- function(indata,minhash,n.bands){
  text.corpus = TextReuseCorpus(text=indata$text,meta=list(id=indata$id),
                                tokenizer = tokenize_ngrams,n=3,minhash_func = minhash)
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>% 
    arrange(desc(score))  
  
  
  return(jacsim)
}

jaccard_targeted_search = function(indata=dat.sentences,choose.topic,search.term,minhash=minhash,n.bands=n.bands){
  topic_s = indata %>% filter(topic_num==choose.topic) %>% unnest(text_data_clean_s) %>% select(-text,-n_words) %>% ungroup() %>%
    mutate(id=row_number())
  topic_s.target = topic_s %>% filter(grepl(paste(search.term),text_data_clean_s)) %>% rename(text=text_data_clean_s)
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = mutate(jaccard_s,pair=row_number())
  
  n.matches = jaccard_s %>% summarise(exact_matches = sum(score==1),similar=sum(score>=0.75),.groups='drop')
  
  topmatches = jaccard_s %>% 
    filter(score>=0.75) %>%
    gather(variable,value,-pair,-score) %>% count(value,sort=T) %>%
    mutate(value=as.numeric(str_remove_all(value,pattern='doc-')))
  
  return(list(topic_s.target=topic_s.target,n.matches=n.matches,topmatches=topmatches))
}

jaccard_highest_coherence = function(indata=dat.sentences,choose.topic,max.rank = 1000,minhash=minhash,n.bands=n.bands){
  topic_s = indata %>% filter(topic_num==choose.topic,rank<=max.rank) %>% unnest(text_data_clean_s) %>% select(-text,-n_words) %>% ungroup() %>%
    mutate(id=row_number())
  topic_s.target = topic_s %>% rename(text=text_data_clean_s)
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = mutate(jaccard_s,pair=row_number())
  
  n.matches = jaccard_s %>% summarise(exact_matches = sum(score==1),similar=sum(score>=0.75),.groups='drop')
  
  topmatches = jaccard_s %>% 
    filter(score>=0.75) %>%
    gather(variable,value,-pair,-score) %>% count(value,sort=T) %>%
    mutate(value=as.numeric(str_remove_all(value,pattern='doc-')))
  
  return(list(topic_s.target=topic_s.target,n.matches=n.matches,topmatches=topmatches,max.rank=max.rank))
}


# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% mutate(text_data_clean_s = str_split(text,"\\.\\s+",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))


#keyword searches based on topic cloud results
keywords = tibble(topic_num=1:10,term=c('t-test', #topic 1
                                        'variable', # topic 2
                                        'graphpad', # topic 3
                                        'anova', # topic 4
                                        'spss', # topic 5
                                        'plus-or-minus', # topic 6
                                        'heterogeneity', # topic 7
                                        'model', # topic 8
                                        'less-than', # topic 9
                                        'experiment')) # topic 10

jaccard_keyword = lapply(1:10,function(x) 
  jaccard_targeted_search(indata = dat.sentences,choose.topic=keywords[x,]$topic_num,search.term = keywords[x,]$term,
                          minhash = minhash,n.bands = n.bands))

#upt o here
save(jaccard_keyword,file='results/jaccard_keyword_plos.rda')

jaccard_topranked = lapply(1:10, function(x)
  jaccard_highest_coherence(indata=dat.sentences,choose.topic=x,max.rank = 1000,minhash=minhash,n.bands=n.bands))
save(jaccard_topranked,file='results/jaccard_topranked_plos.rda')



# jaccard_keyword[[1]] = jaccard_targeted_search(indata = dat.sentences,choose.topic=1,search.term = 't-test',minhash = minhash,n.bands = n.bands)
# jaccard_2_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=2,search.term = 'variable',minhash = minhash,n.bands = n.bands)
# jaccard_3_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=3,search.term = 'graphpad',minhash = minhash,n.bands = n.bands)
# jaccard_4_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=4,search.term = 'anova',minhash = minhash,n.bands = n.bands)
# jaccard_5_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=5,search.term = 'SPSS',minhash = minhash,n.bands = n.bands)
# jaccard_6_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=6,search.term = 'mean plus-or-minus',minhash = minhash,n.bands = n.bands)
# jaccard_7_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=7,search.term = 'heterogeneity',minhash = minhash,n.bands = n.bands)
# jaccard_8_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=8,search.term = 'model',minhash = minhash,n.bands = n.bands)
# jaccard_9_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=9,search.term = 'p less-than',minhash = minhash,n.bands = n.bands)
# jaccard_10_s = jaccard_targeted_search(indata = dat.sentences,choose.topic=10,search.term = 'experiment',minhash = minhash,n.bands = n.bands)
# 

#all sentences to identify most common. Top 100 dois within each cluster

# #take sentences with top coherence score (take id from buckets)
# top_coherence = topic3_s %>% filter(rank==1) %>% pull(id)
# top_matches = lsh_query(buckets,paste0('doc-',top_coherence)) %>%
#   add_column(score=as.double(NA))
# candidates <- lsh_candidates(buckets)
# 
# jacsimilarity_top_s = lsh_compare(candidates %>% sample_frac(0.25), 
#                                   topic3.corpus_s, 
#                                   jaccard_similarity, 
#                                   progress = FALSE) %>% 
#   arrange(desc(score))
