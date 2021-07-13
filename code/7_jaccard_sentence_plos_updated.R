# 7_jaccard_sentence_plos.R
# look for almost identical sentences using the Jaccard score
# uses the top example sentences from the paper
# May 2021
library(tidyverse)
library(tidytext)
library(stringr)
library(stringdist) # for similarity scores
library(openxlsx)
library(textreuse)

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
                 text_data_clean = str_remove_all(text_data_clean,pattern='e.g.\\s+|i.e.\\s+'))



# combine paragraphs from the same paper (with the same DOI)
combined = group_by(matches, doi_num, topic_id, topic_num,rank) %>%
  summarise(text = toString(text_data_clean)) %>%
  ungroup()

# split examples into sentences
# take top ranking result for each topic
examples <- combined %>% filter(rank==1) %>% select(topic_num,doi_num,text) %>%
  rename(topic=topic_num,example = text)


examples_in_sentences = NULL
for (t in 1:nrow(examples)){
  # get the example and split into sentences
  to_compare = examples[t, ]
  example_sentences = str_split(to_compare$example, pattern='\\.\\s+')[[1]]
  f = data.frame(doi_num=to_compare$doi_num,topic=to_compare$topic, sentences = example_sentences)
  examples_in_sentences = bind_rows(examples_in_sentences, f)
}  
examples_in_sentences = mutate(examples_in_sentences, row = 1:n())


# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% mutate(text_data_clean_s = str_split(text,"\\.\\s+",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))


# loop through sentences that were used in the paper
results = NULL
results_byrow = NULL

for (t in 1:nrow(examples_in_sentences)){
  #number of words in target sentence
  n_words_example = str_count(examples_in_sentences$sentence[t], "\\w+")
  
  ## just text in the same topic , filter to similar word counts
  this_topic = filter(dat.sentences, 
                      topic_num == examples_in_sentences$topic[t],doi_num!=examples_in_sentences$doi_num[t]) %>%
    unnest(cols=c(text_data_clean_s, n_words)) %>% filter(abs(n_words - n_words_example) <= 3)

  
  A = tokenize_words(examples_in_sentences$sentence[t])
  
  ## Get the scores for all sentences

  f = this_topic %>% rowwise() %>%
    mutate(score = jaccard_similarity(A,tokenize_words(text_data_clean_s))) %>% 
    ungroup() %>% add_column(row=t)

  results = bind_rows(results, f)
  
  g = results %>% summarise(min_score=min(score,na.rm=T),
                            max_score=max(score,na.rm=T),
                            q1_score=quantile(score,0.25,na.rm=T),
                            q3_score=quantile(score,0.75,na.rm=T),
                            med_score=median(score,na.rm=T),.groups='drop') %>% add_column(row=t)
  
  results_byrow = bind_rows(results_byrow,g)
  cat('Running for row',t,'\r')
  
} # end of sentence loop

# count almost exact matches
almost_exact = filter(results, 
                      score >= 0.9) %>% # high similarity score
  group_by(row) %>%
  tally() %>%
  ungroup()

similar = filter(results, 
                      score >= 0.5) %>% # high similarity score
  group_by(row) %>%
  tally() %>%
  ungroup()

# merge with original data
to_export = full_join(examples_in_sentences, almost_exact, by='row') %>%
  select(row, topic, sentences, n) %>%
  rename('matches_gt0.9' = 'n')

#add similar (0.7 or higher)
to_export = full_join(to_export,similar, by='row') %>%
  select(row, topic, sentences, matches_gt0.9, n) %>%
  rename('matches_gt0.5' = 'n')


# export to Excel
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Matches", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE)
writeDataTable(wb, sheet = 1, x = to_export,
               colNames = TRUE, rowNames = FALSE,
               tableStyle = "TableStyleLight9")
setColWidths(wb, sheet = 1, cols = 1:4, widths = c(5,5,70,5))
saveWorkbook(wb, file = "results/jaccard_matches_plos_v2.xlsx", overwrite = TRUE)
