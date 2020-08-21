library(tidyverse)
library(stringr)
library(stringi)

load('./data/stats_section_info.rda')

stats_section = bind_rows(stats_section)

#change to native encoding
stats_section = stats_section %>% mutate(text_data_clean = enc2native(text_data))

#1. remove formatting/special characters
all_words = stats_section %>% unnest(text_data_clean) %>% 
  mutate(y=strsplit(text_data_clean,' ')) %>% pull(y) %>% unlist()

#find all unicode characters
unicode_lookup = str_extract_all(string=all_words,pattern=regex("<U\\+\\w+>")) %>% 
  unlist() %>%
  as_tibble() %>% count(value)

#mutate, rename, arrange
unicode_lookup = unicode_lookup %>% mutate(value = gsub('000','',value)) %>%
  rename('unicode'=value) %>%
  arrange(-n)

#convert to uft8
unicode_lookup = unicode_lookup %>% mutate(utf8 = gsub("<U\\+(\\w+)>", "\\\\u\\1", unicode))

#add symbols
unicode_lookup = unicode_lookup %>% mutate(symbol = stri_unescape_unicode(utf8))

#select
unicode_lookup = unicode_lookup %>% select(unicode,utf8,symbol,n)

#write
save(unicode_lookup,file='./data/unicode_characters.rda')
