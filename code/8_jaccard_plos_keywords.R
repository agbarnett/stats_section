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
  summarise(text = toString(text_data_clean),groups='drop')

# end data management

# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% mutate(text_data_clean_s = str_split(text,"\\.\\s+",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))


#targeted searches by topic. full sections based on keyword search

#topic 3: GraphPad
topic3 = filter(combined,topic_num==3,grepl('graphpad',text)) %>% distinct(doi_num,.keep_all=T) 


minhash <- minhash_generator(n = 100, seed = 47896)
topic3.corpus = TextReuseCorpus(text=topic3$text[1:50],meta=list(id=topic3$doi_num[1:50]),tokenizer = tokenize_words,minhash_func = minhash)
head(minhashes(topic3.corpus[[1]]),10)
buckets <- lsh(topic3.corpus, bands = 50)
candidates <- lsh_candidates(buckets)

jacsimilarity_both = lsh_compare(candidates, 
                                 topic3.corpus, 
                                 jaccard_similarity, 
                                 progress = FALSE) %>% 
  arrange(desc(score))

head(jacsimilarity_both)
