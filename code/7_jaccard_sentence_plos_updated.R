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
examples <- matches %>% filter(rank==1) %>% select(topic_num,doi_num,words,text_data_clean) %>%
  rename(topic=topic_num,example = text_data_clean)


examples_in_sentences = NULL
for (t in 1:nrow(examples)){
  # get the example and split into sentences
  to_compare = examples[t, ]
  example_sentences = str_split(to_compare$example, pattern='\\.\\s+')[[1]]
  f = data.frame(topic=to_compare$topic, sentences = example_sentences)
  examples_in_sentences = bind_rows(examples_in_sentences, f)
}  
examples_in_sentences = mutate(examples_in_sentences, row = 1:n())



# loop through sentences that were used in the paper
results = NULL
for (t in 1:nrow(examples_in_sentences)){
  #number of words in target sentence
  n_words_example = str_count(examples_in_sentences$sentence[t], "\\w+")
  ## just text in the same topic 
  this_topic = filter(combined, 
                      topic_num == examples_in_sentences$topic[t]) 
  
  # split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
  dat.sentences = lapply(1:nrow(this_topic), function(i) str_split(this_topic[i,]$text,"\\.\\s+",simplify=F) %>% unlist() %>% 
                           tibble(text_data_clean_s=.) %>% 
                           mutate(text_data_clean_s=str_remove_all(text_data_clean_s,pattern='\\.$')) %>%
                           filter(text_data_clean_s!="") %>%
                           add_column(doi = this_topic[i,] %>% pull(doi_num))) %>% bind_rows()

  #add number of words per sentence
  dat.sentences = dat.sentences %>% mutate(n_words = str_count(text_data_clean_s, "\\w+")) %>%
    filter(abs(n_words - n_words_example) <= 3 )
  
  N = length(dat.sentences)
  A = tokenize_words(examples_in_sentences$sentence[t])
  
  ## Get the scores for all sentences
  
  f = dat.sentences %>% rowwise() %>%
    mutate(score = jaccard_similarity(A,tokenize_words(text_data_clean_s))) %>% 
    ungroup() %>% add_column(row=t)

  results = bind_rows(results, f)
  cat('Running for row',t,'\r')
  
} # end of topics loop

# count almost exact matches
almost_exact = filter(results, 
                      score >= 0.9) %>% # high similarity score
  group_by(row) %>%
  tally() %>%
  ungroup()

# merge with original data
to_export = full_join(examples_in_sentences, almost_exact, by='row') %>%
  select(row, topic, sentences, n) %>%
  rename('matches' = 'n')

# export to Excel
wb <- createWorkbook()
addWorksheet(wb, sheetName = "Matches", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE)
writeDataTable(wb, sheet = 1, x = to_export,
               colNames = TRUE, rowNames = FALSE,
               tableStyle = "TableStyleLight9")
setColWidths(wb, sheet = 1, cols = 1:4, widths = c(5,5,70,5))
saveWorkbook(wb, file = "results/jaccard_matches.xlsx", overwrite = TRUE)
