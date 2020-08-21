#5_find_stopwords.R
library(tidyverse)
library(stringr)
library(stringi)

load('./data/stats_section_info.rda')

stats_section = bind_rows(stats_section)

#change to native encoding
stats_section = stats_section %>% mutate(text_data_clean = enc2native(text_data))
#remove unicodes
stats_section = stats_section %>% mutate(text_data_clean = str_remove_all(text_data_clean,
                                                                          pattern="\\s*<U\\+\\w+>\\s*|\\<\\S\\S\\S\\S\\S\\S\\>"))
#unnest to individual words
all_words = stats_section %>% unnest(text_data_clean) %>% 
  mutate(y=strsplit(text_data_clean,' ')) %>% pull(y) %>% unlist()

#word frequencies
word_freq = as_tibble(all_words) %>% count(value) %>% arrange(-n)
