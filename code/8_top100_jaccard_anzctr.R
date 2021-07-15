#8_top100_jaccard_anzctr.R

library(tidyverse)
library(tidytext)
library(stringdist) # for similarity scores
library(openxlsx)
library(textreuse) #updated function for jaccard_similarity (compare to stringdist)

## data
## a) get the example data
#load('data/processed_examples_anzctr.RData') # from 7_example_sentences.R
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

# end data management

# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = matches %>% mutate(text_data_clean_s = str_split(text_data_clean,"\\.\\s+",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))

#loop over topics - start with 1 topic
combined_export = list()
cutoff = 100 #top X papers per topic. set to 100 for final run

for (choose.topic in 1:10){
  {cat('Reviewing Topic',choose.topic,'\r')}
  
  counter = 1
  min_rank=1  
  # loop through sentences that were used in the paper
  to_export = NULL
  
  ## just text in the same topic 
  this_topic = filter(dat.sentences,topic_num == choose.topic) %>%
    unnest(cols=c(text_data_clean_s, n_words))
  
  N = nrow(this_topic)
  
  while(min_rank<=cutoff){
    results = NULL
    #start with rank 1 and keep going until nrow(dat.sentences)=0
    examples_in_sentences = this_topic %>% filter(rank %in% min(rank)) %>%
      select(number,topic_id,topic_num,rank,text_data_clean_s,n_words) %>%
      rename(sentence=text_data_clean_s) %>% mutate(row=1:n())

    
    for (t in 1:length(examples_in_sentences)){  
      n_words_example = examples_in_sentences$n_words[t]
      A = tokenize_words(examples_in_sentences$sentence[t])
      
      this_paper_similar = filter(this_topic,number!=examples_in_sentences$number[t]) %>%
        filter(abs(n_words - n_words_example) <= 3)
      if(nrow(this_paper_similar)>0){
        
        f = this_paper_similar %>% rowwise() %>%
          mutate(score = jaccard_similarity(A,tokenize_words(text_data_clean_s))) %>% 
          ungroup() %>% add_column(row=t)
        
        results = bind_rows(results, f)
      }
    }
    
    #get almost exact matches
    almost_exact = filter(results, 
                          score >= 0.75) 
    
    if(nrow(almost_exact)>0){
      this_topic = this_topic %>% anti_join(.,almost_exact,by=c('text_data_clean_s','number')) %>%
        filter(!number %in% examples_in_sentences[['number']])
      
      n_matches = full_join(examples_in_sentences,almost_exact %>% group_by(row) %>% tally() %>% ungroup(),by='row') %>%
        select(row, sentence,rank, n) %>%
        rename('matches' = 'n')
      to_export = bind_rows(to_export,n_matches)
    }
    if(nrow(almost_exact)==0){
      this_topic = this_topic %>% filter(!number %in% examples_in_sentences[['number']]) 
    }
    # progress (takes longer for longer sections)
    k = nrow(this_topic)
    counter = counter+1
    min_rank = this_topic %>% filter(rank==min(rank)) %>% distinct(rank) %>% pull
    
    cat('Reviewing Topic',choose.topic,':',min_rank,'<',cutoff,'\r')
  }  
  to_export = filter(to_export,!is.na(matches))
  combined_export[[choose.topic]] = to_export
  
} #end of topic loop

save(combined_export,file='results/jaccard_anzctr_top100.rda')

results = bind_rows(combined_export,.id='topic_num') %>%
  mutate(topic_num = as.numeric(topic_num))

#rank ==1 matches
top_ranked_bytopic = results %>% filter(rank==1) %>% arrange(topic_num)

#most common sentence within each topic (rank 1 to 100)
most_frequent_bytopic = results %>% group_by(topic_num) %>% arrange(-matches) %>% slice(1)


#top ranked excluding p-value statements
results_nop = results %>% filter(!grepl('p-value|p value|\\bp\\b',sentence))
most_frequent_bytopic_nop = results_nop %>% group_by(topic_num) %>% arrange(-matches) %>% slice(1)

#top 10 sentences per topic
top10_frequent_bytopic = results %>% group_by(topic_num) %>% arrange(-matches) %>% slice(1:10)

#top 10 excluding p value statments
top10_frequent_bytopic_nop = results_nop %>% group_by(topic_num) %>% arrange(-matches) %>% slice(1:10)

