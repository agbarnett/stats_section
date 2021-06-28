# 7_jaccard_sentence_plos.R
# look for almost identical sentences using the Jaccard score
# uses the top example sentences from the paper
# May 2021
library(tidyverse)
library(tidytext)
#library(openxlsx)
library(stringr)
library(stringdist) # for similarity scores
library(openxlsx)

## data
## a) get the example data
#load('data/processed_examples.RData') # from 7_example_sentences.R
## b) get all the data
load(file='results/plos.results.10topics.rda')
# data management 
matches = ungroup(matches) %>%
  mutate(topic_num = as.numeric(str_remove(topic_id, pattern='Topic ')), # topic number
         doi_num = str_remove(doi, pattern='10.1371/journal\\.pone\\.')) # just get the number from the DOI
# combine paragraphs from the same paper (with the same DOI)
combined = group_by(matches, doi_num, topic_id, topic_num,rank) %>%
  summarise(text = toString(text_data_clean )) %>%
  ungroup() %>% arrange(topic_num,rank)

#loop over topics
combined_export = list()
counter = 1
for (choose.topic in c(5)){
  {cat('Reviewing Topic',choose.topic,'\r')}
  
# loop through sentences that were used in the paper
  to_export = NULL

  ## just text in the same topic 
  this_topic = filter(combined, 
                      topic_num == choose.topic) 
  
  to_compare = filter(this_topic,rank==1) 
  example_sentences = str_split(to_compare$text, pattern='\\. ')[[1]]
  f = data.frame(topic=to_compare$topic_num,sentences=example_sentences)
  examples_in_sentences = f %>% mutate(row = 1:n())
  

  # split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
  dat.sentences = lapply(1:nrow(this_topic), function(i) str_split(this_topic[i,]$text,"\\. ",simplify=F) %>% unlist() %>% 
                           tibble(text_data_clean_s=.) %>% add_column(doi = this_topic[i,] %>% pull(doi_num),rank = this_topic[i,] %>% pull(rank))) %>%
    bind_rows()
  N = nrow(dat.sentences)
  #add number of words per sentence
  dat.sentences = dat.sentences %>% mutate(n_words = str_count(text_data_clean_s, "\\w+"))

  while(nrow(dat.sentences)>0.5*N){
    results = NULL
  #start with rank 1 and keep going untile nrow(dat.sentences)=0
  examples_in_sentences = dat.sentences %>% filter(rank %in% min(rank)) %>% mutate(row = 1:n())
  
  for (t in 1:nrow(examples_in_sentences)){  
    n_words_example = examples_in_sentences[t,] %>% pull(n_words)
    this_paper_similar = filter(dat.sentences, abs(n_words - n_words_example) <= 3 )
    if(nrow(this_paper_similar)>0){
      
      f = this_paper_similar %>% rowwise() %>%
        mutate(score = stringsim(a = examples_in_sentences$text_data_clean_s[t], # compares example sentence with every sentence in the paper
                        b = text_data_clean_s,
                        method = c("osa"),
                        useBytes = FALSE)) %>% ungroup() %>% add_column(row=t)
      
      results = bind_rows(results, f)
    }
  }
  #get almost exact matches
      almost_exact = filter(results, 
                            score >= 0.9) 
      
      dat.sentences = dat.sentences %>% anti_join(.,almost_exact,by=c('text_data_clean_s','doi')) 
      
      n_matches = full_join(examples_in_sentences,almost_exact %>% group_by(row) %>% tally() %>% ungroup(),by='row') %>%
        select(row, text_data_clean_s,rank, n) %>%
        rename('matches' = 'n','sentence'=text_data_clean_s)
      to_export = bind_rows(to_export,n_matches)
      
      # progress (takes longer for longer sections)
      k = nrow(dat.sentences)
  }  
 combined_export[[counter]] = to_export
 counter = counter+1
 
} #end of topic loop
