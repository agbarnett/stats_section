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
load('data/processed_examples.RData') # from 7_example_sentences.R
## b) get all the data
load(file='results/plos.results.10topics.rda')
# data management 
matches = ungroup(matches) %>%
  mutate(topic_num = as.numeric(str_remove(topic_id, pattern='Topic ')), # topic number
         doi_num = str_remove(doi, pattern='10.1371/journal\\.pone\\.')) # just get the number from the DOI
# combine paragraphs from the same paper (with the same DOI)
combined = group_by(matches, doi_num, topic_id, topic_num) %>%
  summarise(text = toString(text_data_clean )) %>%
  ungroup()

# split examples into sentences
examples_in_sentences = NULL
for (t in 1:nrow(examples)){
  # get the example and split into sentences
  to_compare = examples[t, ]
  example_sentences = str_split(to_compare$example, pattern='\\. ')[[1]]
  f = data.frame(topic=to_compare$topic, sentences = example_sentences)
  examples_in_sentences = bind_rows(examples_in_sentences, f)
}  
examples_in_sentences = mutate(examples_in_sentences, row = 1:n())

# loop through sentences that were used in the paper
results = NULL
for (t in 1:nrow(examples_in_sentences)){
  
  ## just text in the same topic 
  this_topic = filter(combined, 
                      topic_num == examples_in_sentences$topic[t]) 
  
  # split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
  dat.sentences = lapply(1:nrow(this_topic), function(i) str_split(this_topic[i,]$text,"\\. ",simplify=F) %>% unlist() %>% 
                           tibble(text_data_clean_s=.) %>% add_column(doi = this_topic[i,] %>% pull(doi_num)))
  N = length(dat.sentences)
  
  ## Get the scores for all sentences
  for (k in 1:N){ # loop through papers
    this_paper = dat.sentences[[k]] %>%
      mutate(n_words = str_count(text_data_clean_s, "\\w+")) # number of words per sentence
      n_words_example = str_count(examples_in_sentences$sentence[t], "\\w+")
      # filter on sentences that are a similar length to the example
      this_paper_similar = filter(this_paper, abs(n_words - n_words_example) <= 3 ) # within 3 words
      if(nrow(this_paper_similar)>0){
        score = stringsim(a = examples_in_sentences$sentence[t], # compares example sentence with every sentence in the paper
                          b = this_paper_similar$text_data_clean_s,
                          method = c("jaccard"),
                          useBytes = FALSE)
        f = data.frame(doi_num = this_topic$doi_num[k], score=score, row=t)
        results = bind_rows(results, f)
    } # end of sentence loop
    # progress (takes longer for longer sections)
    if(k%%500 == 0){cat('Up to',k,'\r')}
  }
  
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
