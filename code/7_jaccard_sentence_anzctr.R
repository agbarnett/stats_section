# 7_jaccard_sentence_anzctr.R
# look for almost identical sentences using the Jaccard score
# uses the top example sentences from the paper
# Last updated July 12 2021
library(tidyverse)
library(tidytext)
library(stringr)
library(stringdist) # for similarity scores
library(textreuse)
library(openxlsx)

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
                 text_data_clean = str_remove_all(text_data_clean,pattern='e.g.\\s+|i.e.\\s+')
                 )


# split examples into sentences
# take top ranking result for each topic
examples <- matches %>% filter(rank==1) %>% select(topic_num,number,words,text_data_clean) %>%
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
  
  ## just text in the same topic 
  this_topic = filter(matches, 
                      topic_num == examples_in_sentences$topic[t]) 
  
  # split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
  dat.sentences = lapply(1:nrow(this_topic), function(i) str_split(this_topic[i,]$text_data_clean,"\\.\\s+",simplify=F) %>% unlist() %>% 
                           tibble(text_data_clean_s=.) %>% 
                           mutate(text_data_clean_s=str_remove_all(text_data_clean_s,pattern='\\.$')) %>%
                           filter(text_data_clean_s!="") %>%
                           add_column(doi = this_topic[i,] %>% pull(number)))

  N = length(dat.sentences)
  A = tokenize_words(examples_in_sentences$sentence[t])
  
  ## Get the scores for all sentences
 
  for (k in 1:N){ # loop through papers
    this_paper = dat.sentences[[k]] %>%
      mutate(n_words = str_count(text_data_clean_s, "\\w+")) # number of words per sentence
      n_words_example = str_count(examples_in_sentences$sentence[t], "\\w+")
      
      # filter on sentences that are a similar length to the example
      this_paper_similar = filter(this_paper, abs(n_words - n_words_example) <= 3 ) # within 3 words

      if(nrow(this_paper_similar)>0){
        score = lapply(1:nrow(this_paper_similar), function(b) 
          jaccard_similarity(A,tokenize_words(this_paper_similar$text_data_clean_s[b]))) %>% unlist()
        
        

        #score = (length(intersect(A,B))/length(union(A,B)))
        # score = 1-seq_dist(a = hash(A), # compares example sentence with every sentence in the paper. Updated on 12 7 to word level
        #                   b = hash(B),
        #                   method = c("jaccard"),q=1)
        f = data.frame(number = this_topic$number[k], score=score, row=t,sentence=this_paper_similar$text_data_clean_s)
        results = bind_rows(results, f)
    } # end of sentence loop
    # progress (takes longer for longer sections)
    if(k%%500 == 0){cat('Example',t,': Up to',k,'\r')}
  }
  
} # end of topics loop

# count almost exact matches
almost_exact = filter(results, 
                      score >= 0.7) %>% # high similarity score. changed from 0.9
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
saveWorkbook(wb, file = "results/jaccard_matches_anzctr_v3.xlsx", overwrite = TRUE)


#take maximum score per number and plot distribution
g.theme = theme_bw()+theme(legend.position = 'top',legend.direction = 'horizontal',legend.text = element_text(size=10),
                           axis.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

max_scores = results %>% group_by(number) %>% summarise(max_score=max(score,na.rm=T))
max_scores = max_scores %>% full_join(.,select(matches,number,topic_id),by='number') %>% 
  mutate(max_score = replace_na(max_score,0))
         
         
g = ggplot(max_scores,aes(max_score,group=topic_id,colour=topic_id)) + stat_ecdf(size=1) + theme_minimal()+
  scale_x_continuous('Maximum Jaccard similarity (per section)',breaks=seq(0,1,0.1))+
  scale_y_continuous('Cumulative proportion of statistical methods sections',breaks=seq(0,1,0.1))+g.theme+
  theme(panel.grid.minor = element_blank(),legend.title = element_blank())

jpeg('manuscript/asa_template/figures/anzctr_jaccard.jpg',width=480,height=600,quality=100)
g
dev.off()
