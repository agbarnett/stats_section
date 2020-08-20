library(tidyverse)
library(tidytext)
library(stringr)
library(textclean)
library(tm)
library(spelling)
library(readxl)

load('stats_section_info.rda')

stats_section = bind_rows(stats_section)

#change to native encoding
stats_section = stats_section %>% mutate(text_data_clean = enc2native(text_data))

#1. remove formatting/special characters
all_words = stats_section %>% unnest(text_data_clean) %>% 
  mutate(y=strsplit(text_data_clean,' ')) %>% pull(y) %>% unlist()

#find all unicode characters
unicode_all = grep('\\s*<U\\+\\w+>\\s*|"\\s*\\<\\S\\S\\S\\S\\S\\S\\>\\s*"',all_words,value=T)
unicode_unique = str_extract_all(string=unicode_all,pattern=regex("<U\\+\\w+>")) %>% unlist() %>% unique()

#write
save(unicode_unique,file='unicode_characters.rda')
