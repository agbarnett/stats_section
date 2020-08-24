library(tidyverse)
library(stringi)
library(Unicode)
library(readr)

load('./data/stats_section_info.rda')

stats_section = bind_rows(stats_section)

#change to native encoding
stats_section = stats_section %>% mutate(text_data_clean = enc2native(text_data))

#unnest to individual words
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

#u_char_name for text label
unicode_lookup = unicode_lookup %>% mutate(label = u_char_name(gsub("<(.*)>",'\\1',unicode)))
#mutate label
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub(' ','-',label) %>% tolower())
#update 'small-letters' e.g greek-small-letter-chi to chi
unicode_lookup = unicode_lookup %>% mutate(label_clean = gsub('.*small-letter-|greek-|-symbol|slanted-','',label_clean))

#convert to uft8
unicode_lookup = unicode_lookup %>% mutate(utf8 = gsub("<U\\+(\\w+)>", "\\\\u\\1", unicode))

#add symbols
unicode_lookup = unicode_lookup %>% mutate(symbol = stri_unescape_unicode(utf8))


#select
unicode_lookup = unicode_lookup %>% select(unicode,utf8,symbol,label,label_clean,n)

#write
save(unicode_lookup,file='./data/unicode_characters.rda')
write_excel_csv(unicode_lookup,path='./data/unicode_characters.csv')
