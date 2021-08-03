# look for almost identical sentences using the Jaccard score
#  targeted searches of keywords
# May 2021
library(tidyverse)
library(textreuse) #updated function for jaccard_similarity (compare to stringdist)

## data
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

#et al. or vs. or fig. [:lower:]{1}[:digit:]{1}
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(et al)(\\.)",replacement="\\1\\2"))
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(vs)(\\.)",replacement="\\1\\2"))
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(fig|figs)(\\.)",replacement="\\1\\2"))
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(no)(\\.)",replacement="\\1\\2"))

matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern='([:digit:][:lower:])(\\.)',replacement='\\1'))
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern='([:lower:][:digit:])(\\.)',replacement='\\1'))

#remove excess white space
matches = mutate(matches,text_data_clean=textclean::replace_white(text_data_clean))

# combine paragraphs from the same paper (with the same DOI)
combined = group_by(matches, doi_num, topic_id, topic_num,rank) %>%
  summarise(text = toString(text_data_clean),.groups='drop')

combined = combined %>% arrange(topic_num,rank)

# end data management
# Document-level analysis, all topics
n.minhash = 400
n.bands = 50

random.seed = TeachingDemos::char2seed('jaccard',set=F)
minhash <- minhash_generator(n = n.minhash, seed = random.seed)

#review chosen number of minhashes and bands
lsh_threshold(h=n.minhash,b=n.bands) #expected jaccard index
lsh_probability(h=n.minhash,b=n.bands,s=0.8) #probability of a match based on chosen score cutoff

calc_document_jaccard <- function(indata=combined,choose.topic,minhash,n.bands){
  dat.topic = indata %>% filter(topic_num==choose.topic) %>% distinct(doi_num,.keep_all=T) %>%
    mutate(id=row_number())
  
  text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                tokenizer = tokenize_words,minhash_func = minhash,skip_short = F)
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>% 
    arrange(desc(score)) 
  
  jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  
  return(list(similarities=jacsim,dat=dat.topic))
}

#loop over topics
jaccard_doc = lapply(1:10, function(x) calc_document_jaccard(indata=combined,choose.topic=x,minhash = minhash,n.bands = n.bands))

#for a given topic, find total exact matches, total with score >=0.8
boilerplate_doc = lapply(1:10,function(x) jaccard_doc[[x]]$similarities %>% filter(score>=0.8))
exact_doc = lapply(1:10,function(x) jaccard_doc[[x]]$similarities %>% filter(score==1))

matches_doc = exact_matches_doc = list()
for (x in 1:10){
  matches_doc[[x]] <- jaccard_doc[[x]]$dat %>% filter(id %in% boilerplate_doc[[x]]$a|id %in% boilerplate_doc[[x]]$b)
  exact_matches_doc[[x]] <- jaccard_doc[[x]]$dat %>% filter(id %in% exact_doc[[x]]$a|id %in% exact_doc[[x]]$b)  
}

matches_doc.all = bind_rows(matches_doc)
exact_matches_doc.all = bind_rows(exact_matches_doc)

#save selected topics
save(jaccard_doc,matches_doc.all,exact_matches_doc.all,file='results/jaccard_section_plos.rda')

# Sentence-level analysis, all topics
## most common sentences
## sentence matches by words within topic clouds
calc_sentence_jaccard <- function(indata,minhash,n.bands,cutoff=0.8){
  text.corpus = TextReuseCorpus(text=indata$text,meta=list(id=indata$id),
                                tokenizer = tokenize_words,minhash_func = minhash,skip_short=F)
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  ### to_compare <- lsh_query(buckets,"[doc-id]") ## for top down appraoch. could copmute text.corpus once then at each iteration, remove already matched sentences; e.g. buckets %>% filter(!doc %in% 'doc-1')
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>% 
    filter(score>=cutoff)  
  jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  
  return(jacsim)
}

jaccard_targeted_search = function(indata=dat.sentences,choose.topic,search.term,minhash=minhash,n.bands=n.bands){
  #topic_s = indata %>% filter(topic_num==choose.topic) %>% unnest(text_data_clean_s) %>% select(-text,-n_words) %>% ungroup() %>%
  #  mutate(id=row_number())
  #topic_s.target = topic_s %>% filter(grepl(paste(search.term),text_data_clean_s)) %>% rename(text=text_data_clean_s)
  topic_s.target = indata %>% filter(topic_num==choose.topic) %>% unnest(text_data_clean_s) %>%
    ungroup() %>% 
    filter(grepl(paste(search.term),text_data_clean_s)) %>%
    select(doi_num,text_data_clean_s) %>% distinct(doi_num,text=text_data_clean_s) %>%
    group_by(text) %>%
    summarise(doi_num=list(doi_num)) %>% 
    ungroup() %>%
    filter(text!="") %>%
    mutate(id=row_number())
  
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  dat = topic_s.target %>% filter(id %in% jaccard_s$a|id %in% jaccard_s$b) %>% unnest(doi_num)
  return(list(similarities=jaccard_s,dat=dat,keyword=search.term))
}

jaccard_highest_coherence = function(indata=dat.sentences,choose.topic,max.rank = 10000,minhash=minhash,n.bands=n.bands){
  topic_s.target = indata %>% filter(topic_num==choose.topic,rank<=max.rank) %>% 
    unnest(c(text_data_clean_s,n_words)) %>%
    ungroup() %>% 
    select(doi_num,text_data_clean_s,n_words) %>% distinct(doi_num,text=text_data_clean_s,n_words=n_words) %>%
    filter(n_words>3) %>%
    group_by(text) %>%
    summarise(doi_num=list(doi_num)) %>% 
    ungroup() %>%
    filter(text!="") %>%
    mutate(id=row_number())

  
  # topic_s.target = topic_s %>% rename(text=text_data_clean_s) %>% filter(text!="")
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  dat = topic_s.target %>% filter(id %in% jaccard_s$a|id %in% jaccard_s$b) %>% unnest(doi_num)
  return(list(similarities=jaccard_s,dat=dat,max.rank=max.rank))
}

jaccard_all_batched = function(indata=dat.sentences,choose.topic,grp.size=5000,minhash=minhash,n.bands=n.bands){
  topic_s.target = indata %>% filter(topic_num==choose.topic) %>% 
    unnest(c(text_data_clean_s,n_words)) %>%
    ungroup() %>% 
    select(doi_num,text_data_clean_s,n_words) %>% distinct(doi_num,text=text_data_clean_s,n_words=n_words) 
  
  #filter and summarise by distinct sentences
  topic_s.target = topic_s.target %>%
    group_by(text,n_words<=3) %>%
    summarise(doi_num=list(doi_num)) %>% 
    ungroup() %>%
    filter(text!="") %>%
    mutate(id=row_number())
  
  #split into groups of length grp.size
  topic_s.target = topic_s.target %>% mutate(grp=as.numeric(cut_interval(id,length=grp.size)))
  n.grps = max(topic_s.target$grp)
  
  # topic_s.target = topic_s %>% rename(text=text_data_clean_s) %>% filter(text!="")
  jaccard_s = lapply(1:n.grps, function(g) 
    calc_sentence_jaccard(indata=filter(topic_s.target,grp==g),minhash = minhash,n.bands = n.bands)) %>%
    bind_rows()
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  dat = topic_s.target %>% filter(id %in% jaccard_s$a|id %in% jaccard_s$b) %>% unnest(doi_num)
  return(list(similarities=jaccard_s,dat=dat,max.rank=max.rank))
}


# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% group_by(topic_num,rank,doi_num) %>% 
  summarise(text_data_clean_s = str_split(text,"\\.\\s+"),.groups='drop') %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))

# remove .at start of sentence
dat.sentences = mutate(dat.sentences,text_data_clean_s = lapply(text_data_clean_s,function(x) str_remove_all(x,pattern = '^\\.|\\s+\\.$')))

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

boilerplate_keyword = lapply(1:10,function(x) jaccard_keyword[[x]]$similarities %>% filter(score>=0.8))
exact_keyword = lapply(1:10,function(x) jaccard_keyword[[x]]$similarities %>% filter(score==1))

matches_keyword = exact_matches_keyword = list()
for (x in 1:10){
  matches_keyword[[x]] <- jaccard_keyword[[x]]$dat %>% filter(id %in% boilerplate_keyword[[x]]$a|id %in% boilerplate_keyword[[x]]$b)
  exact_matches_keyword[[x]] <- jaccard_keyword[[x]]$dat %>% filter(id %in% exact_keyword[[x]]$a|id %in% exact_keyword[[x]]$b)  
}

matches_keyword.all = bind_rows(matches_keyword)
exact_matches_keyword.all = bind_rows(exact_matches_keyword)

save(jaccard_keyword,matches_keyword.all,exact_matches_keyword.all,file='results/jaccard_keyword_plos.rda')

#memory issues with total sample size
cutoffs = combined %>% group_by(topic_num) %>% summarise(rank.cutoff=0.1*max(rank))
jaccard_topranked = lapply(1:10, function(x)
  jaccard_highest_coherence(indata=dat.sentences,choose.topic=x,max.rank=ceiling(cutoffs[x,]$rank.cutoff),minhash=minhash,n.bands=n.bands))

boilerplate_topranked = lapply(1:10,function(x) jaccard_topranked[[x]]$similarities %>% filter(score>=0.8))
exact_topranked = lapply(1:10,function(x) jaccard_topranked[[x]]$similarities %>% filter(score==1))

matches_topranked = exact_matches_topranked = list()
for (x in 1:10){
  matches_topranked[[x]] <- jaccard_topranked[[x]]$dat %>% filter(id %in% boilerplate_topranked[[x]]$a|id %in% boilerplate_topranked[[x]]$b)
  exact_matches_topranked[[x]] <- jaccard_topranked[[x]]$dat %>% filter(id %in% exact_topranked[[x]]$a|id %in% exact_topranked[[x]]$b)  
}

matches_topranked.all = bind_rows(matches_topranked,.id='topic_num')
exact_matches_topranked.all = bind_rows(exact_matches_topranked,.id='topic_num')

save(jaccard_topranked,matches_topranked.all,exact_matches_topranked.all,file='results/jaccard_topranked10percent_plos.rda')

 
load('results/jaccard_topranked10percent_plos.rda')
#take exact matches for each topic to expand to all doi_num
top.sentences = matches_topranked.all %>% group_by(topic_num) %>% count(text,sort=T) %>% slice(1)
#for each top.sentence, run lsh_query against the rest of the text corpus
#e.g. topic1
top.sentences_all = list()
indata = dat.sentences
for (choose.topic in 1:10){
topic_s.target = indata %>% filter(topic_num==choose.topic) %>% unnest(text_data_clean_s) %>%
  ungroup() %>% 
  select(doi_num,text_data_clean_s) %>% distinct(doi_num,text=text_data_clean_s) %>%
  group_by(text) %>%
  summarise(doi_num=list(doi_num)) %>% 
  ungroup() %>%
  filter(text!="") %>%
  mutate(id=row_number())

text.corpus = TextReuseCorpus(text=topic_s.target$text,meta=list(id=topic_s.target$id),
                              tokenizer = tokenize_words,minhash_func = minhash,skip_short=F)
buckets <- lsh(text.corpus, bands = n.bands)
#lsh_query
top.id = topic_s.target %>% filter(text==top.sentences[choose.topic,]$text) %>% pull(id)
out = lsh_query(buckets,paste0('doc-',top.id)) %>% mutate(score=as.double(NA))

jacsim = lsh_compare(out, 
                     text.corpus, 
                     jaccard_similarity) %>% 
  arrange(desc(score)) 

jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-'))) %>% filter(score>=0.8)
dat = topic_s.target %>% filter(id %in% jacsim$a|id %in% jacsim$b) %>% unnest(doi_num)
top.sentences_all[[choose.topic]] = list(jacsim=jacsim,dat=dat)
}
save(top.sentences_all,top.sentences,jaccard_topranked,matches_topranked.all,exact_matches_topranked.all,file='results/jaccard_topranked10percent_plos.rda')
