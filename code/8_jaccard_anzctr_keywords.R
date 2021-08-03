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
load(file='results/anzctr.results.10topics.rda')

# data management 
matches = ungroup(matches) %>%
  mutate(topic_num = as.numeric(str_remove(topic_id, pattern='Topic '))) # topic number


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
#inc. or ver.
matches =mutate(matches,
                text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(inc|ver)(\\.)",replacement="\\1\\2"))

#et al. or '..'
matches = mutate(matches,
                text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(et al)(\\.)",replacement="\\1\\2"))
matches = mutate(matches,
                 text_data_clean = str_replace_all(text_data_clean,pattern="\\.+",replacement="."))

combined = matches %>% rename(text=text_data_clean) %>%
  mutate(topic_num = str_remove_all(topic_id,'Topic '))
# end data management

# Document-level analysis, all topics
n.minhash = 1000
n.bands = 200

random.seed = TeachingDemos::char2seed('jaccard',set=F)
minhash <- minhash_generator(n = n.minhash, seed = random.seed)

#review chosen number of minhashes and bands
lsh_threshold(h=n.minhash,b=n.bands) #expected jaccard index
lsh_probability(h=n.minhash,b=n.bands,s=0.75) #probability of a match based on chosen score cutoff

calc_document_jaccard <- function(indata=combined,choose.topic,minhash,n.bands){
  dat.topic = indata %>% filter(topic_num==choose.topic) %>% distinct(number,.keep_all=T) %>%
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
save(jaccard_doc,matches_doc.all,exact_matches_doc.all,file='results/jaccard_section_anzctr.rda')


# Sentence-level analysis, all topics
## most common sentences
## sentence matches by words within topic clouds
calc_sentence_jaccard <- function(indata,minhash,n.bands){
  text.corpus = TextReuseCorpus(text=indata$text,meta=list(id=indata$id),
                                tokenizer = tokenize_words,minhash_func = minhash,skip_short=F)
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  ### to_compare <- lsh_query(buckets,"[doc-id]") ## for top down appraoch. could copmute text.corpus once then at each iteration, remove already matched sentences; e.g. buckets %>% filter(!doc %in% 'doc-1')
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>% 
    arrange(desc(score))  
  jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  
  return(jacsim)
}

jaccard_targeted_search = function(indata=dat.sentences,choose.topic,search.term,minhash=minhash,n.bands=n.bands){
  topic_s = indata %>% filter(topic_num==choose.topic) %>% unnest(text_data_clean_s) %>% select(-text,-n_words) %>% ungroup() %>%
    mutate(id=row_number())
  topic_s.target = topic_s %>% filter(grepl(paste(search.term),text_data_clean_s)) %>% rename(text=text_data_clean_s)
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  return(list(similarities=jaccard_s,dat=topic_s.target,keyword=search.term))
}

jaccard_highest_coherence = function(indata=dat.sentences,choose.topic,max.rank = 500,minhash=minhash,n.bands=n.bands){
  topic_s = indata %>% filter(topic_num==choose.topic,rank<=max.rank) %>% unnest(text_data_clean_s) %>% select(-text,-n_words) %>% ungroup() %>%
    mutate(id=row_number())
  topic_s.target = topic_s %>% rename(text=text_data_clean_s) %>% filter(text!="")
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  return(list(similarities=jaccard_s,dat=topic_s.target,max.rank=max.rank))
}

# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% mutate(text_data_clean_s = str_split(text,"\\.\\s+",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))

#keyword searches based on topic cloud results
keywords = tibble(topic_num=1:10,term=c('data will', #topic 1
                                        '80 percent', # topic 2
                                        't-test', # topic 3
                                        'patient', # topic 4
                                        'pilot study', # topic 5
                                        'subject', # topic 6
                                        'descriptive', # topic 7
                                        'intervention group', # topic 8
                                        'model', # topic 9
                                        'anova')) # topic 10

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

save(jaccard_keyword,matches_keyword.all,exact_matches_keyword.all,file='results/jaccard_keyword_anzctr.rda')

jaccard_topranked = lapply(1:10, function(x)
  jaccard_highest_coherence(indata=dat.sentences,choose.topic=x,max.rank = 2000,minhash=minhash,n.bands=n.bands))

boilerplate_topranked = lapply(1:10,function(x) jaccard_topranked[[x]]$similarities %>% filter(score>=0.8))
exact_topranked = lapply(1:10,function(x) jaccard_topranked[[x]]$similarities %>% filter(score==1))

matches_topranked = exact_matches_topranked = list()
for (x in 1:10){
  matches_topranked[[x]] <- jaccard_topranked[[x]]$dat %>% filter(id %in% boilerplate_topranked[[x]]$a|id %in% boilerplate_topranked[[x]]$b)
  exact_matches_topranked[[x]] <- jaccard_topranked[[x]]$dat %>% filter(id %in% exact_topranked[[x]]$a|id %in% exact_topranked[[x]]$b)  
}

matches_topranked.all = bind_rows(matches_topranked)
exact_matches_topranked.all = bind_rows(exact_matches_topranked)

save(jaccard_topranked,matches_topranked.all,exact_matches_topranked.all,file='results/jaccard_topranked_anzctr.rda')

# #top down approach
#using similarities output starting at doc-1, take all matches above threshold -> count -> remove matches -> move to next doc in order

# #run text.corpus
# text.corpus = TextReuseCorpus(text=indata$text,meta=list(id=indata$id),
#                               tokenizer = tokenize_words,minhash_func = minhash,skip_short=F)
# #get the buckets
# buckets <- lsh(text.corpus, bands = n.bands) 
# 
# bucket.order = buckets %>% mutate(doc.n=as.numeric(str_remove_all(doc,pattern='doc-')))
# 
# already_matched <- NULL
# to_compare <- 'doc-1'
# 
# 
# while nrow(candidates)>0
# candidates = lsh_query(filter(buckets,!doc %in% already_matched),to_compare) %>% mutate(score=as.double(NA))
# jacsim = lsh_compare(candidates, 
#                      text.corpus, 
#                      jaccard_similarity) %>% 
#   filter(score>=0.75)
# already_matched<-c(already_matched,to_compare,jacsim$b)
