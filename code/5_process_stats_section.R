library(tidyverse)
library(textclean)
library(tm)
library(spelling)
library(readxl)

load('./data/stats_section_info.rda')
load('./data/unicode_characters.rda')

stat_terms_hyphen = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'hyphen_terms')
stat_terms_singular = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'single_terms')
other_terms = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'other')


stats_section = bind_rows(stats_section)

#1. remove formatting/special characters

#change to native encoding
stats_section = stats_section %>% mutate(text_data = enc2native(text_data))
stats_section = stats_section %>% mutate(text_data_clean = text_data)

#numbered references eg [23]
#stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*\\[\\d+\\]\\s*",""))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,pattern="\\[\\S{1,3}\\]",replacement = ""))

#remove () including text within brackets  "\\s*\\([^\\)]+\\)"
#if want to keep text inside brackets, use "[()]"
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"[()]",""))

#centered equations/other
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,pattern="\\s*(\n)(.*)(\n)\\s*",replacement = " "))


#2. replace/standardise common symbols
#unicode
#format unicode characters (e.g. hair space <U+200A>)
#remove general punctuation unicodes U+20xxx (no dashes identified)
unicode_spaces = unicode_lookup %>% filter(grepl("U\\+20(\\w+)",unicode)) %>% pull(unicode) 
unicode_spaces = gsub("U\\+(\\w+)", "\\U\\\\+\\1",unicode_spaces) %>% str_c(.,collapse='|')

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,pattern=unicode_spaces,replacement=" "))

#change unicode with label_clean
unicode_set = gsub("U\\+(\\w+)", "\\U\\\\+\\1",unicode_lookup[['unicode']])
unicode_set = str_c(unicode_set,collapse='|')

#add white space around unicode labels
unicode_to_text = function(input){
  out = unicode_lookup %>% filter(unicode==input) %>% pull(label_clean)
  paste0(' ',out,' ')
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           unicode_set,
                                                                           unicode_to_text))

#standardise dashes 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,pattern="\\s*(–+)\\s*",replacement = "-"))

#standardise text and spacing for >,<,= 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*[<]\\s*"," less-than "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*[>]\\s*"," greater-than "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*[=]\\s*"," equal-to "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*[<=]\\s*"," less-than-or-equal-to "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*[>=]\\s*"," greater-than-or-equal-to "))

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\bless than\\b","less-than"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\bequal to\\b","equal-to"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\bgreater than\\b","greater-than"))

#plus or minus 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(±)\\s*|\\s*(\\+/-+)\\s*"," plus-or-minus "))

#common symbols covered by text clean ($,%,#,@,&,w/)
stats_section = stats_section %>% mutate(text_data_clean = replace_symbol(text_data_clean))

#3. remove any remaining non-ascii characters, curly quotes
stats_section = stats_section %>% mutate(text_data_clean = replace_non_ascii(text_data_clean)) 
stats_section = stats_section %>% mutate(text_data_clean = replace_curly_quote(text_data_clean))

#remove punctuation except for '.','-'
stats_section$text_data_clean = strip(stats_section$text_data_clean,char.keep = c("~~",".","-"),apostrophe.remove=T,digit.remove=F)

#4. make common statistical terms and methods consistent

#unnest to individual words
all_words = stats_section %>% unnest(text_data_clean) %>% 
  mutate(y=strsplit(text_data_clean,' ')) %>% pull(y) %>% unlist() 
#word frequencies
word_freq = tibble::enframe(all_words) %>% count(value) %>% arrange(-n)

#for each hyphenated term, create combined and unique plural terms
stat_terms_hyphen = stat_terms_hyphen %>% mutate(combined_term = str_remove_all(term,' '))
plural_terms  = unique(paste0(c(stat_terms_hyphen[['term']],stat_terms_hyphen[['combined_term']],stat_terms_hyphen[['update']]),'s'))

#filter word_freq to find variants of common methods
combined_found = word_freq %>% filter(value %in% stat_terms_hyphen[['combined_term']])
hyphen_found = word_freq %>% filter(value %in% stat_terms_hyphen[['update']])
plural_found = word_freq %>% filter(value %in% plural_terms)


#str_c
plural_terms_all = str_c("\\b",plural_found[['value']],"\\b",collapse="|") #to turn plural to singular
stats_terms_all = str_c("\\b",stat_terms_hyphen[['term']],"\\b",collapse="|") #to join method words by hyphen
stats_combined_all = str_c("\\b",combined_found[['value']],"\\b",collapse="|") #to split method words by hyphen
other_terms_all = str_c("\\b",other_terms[['term']],"\\b",collapse='|') #incorrect spellings identified

#change plural terms to singular first
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           plural_terms_all,
                                                                           function(x) gsub('s$','',x)))


#other (include common us/uk spelling)
change_other = function(input){
  other_terms %>% filter(term==input) %>% pull(update) 
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           other_terms_all,
                                                                           change_other))

#standardise common stats terms with hyphen
change_stats_terms = function(input){
  stat_terms_hyphen %>% filter(term==input) %>% pull(update)
}
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           stats_terms_all,
                                                                           change_stats_terms))

#combined stats terms; split by hypen
change_stats_combined = function(input){
  stat_terms_hyphen %>% filter(combined_term==input) %>% pull(update)
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           stats_terms_combined,
                                                                           change_stats_combined))


#redo unnest
all_words = stats_section %>% unnest(text_data_clean) %>% 
  mutate(y=strsplit(text_data_clean,' ')) %>% pull(y) %>% unlist() 

#word frequencies
word_freq = tibble::enframe(all_words) %>% count(value) %>% arrange(-n)

#test hyphenate terms
word_freq %>% filter(value %in% plural_found[['value']]) #didnt work TODO
word_freq %>% filter(value %in% stat_terms_hyphen[['term']])
word_freq %>% filter(value %in% combined_found[['value']])
word_freq %>% filter(value %in% other_terms[['term']])
word_freq %>% filter(value %in% stat_terms_hyphen[['update']])

#one-word single versus plural terms - take dominant spelling
stat_terms_singular = stat_terms_singular %>% mutate(plural = paste0(term,'s'))
singular_found = word_freq %>% filter(value %in% stat_terms_singular[['term']]) %>% rename('singular'=value,'n_singular'=n)
singular_found = singular_found %>% mutate(plural = paste0(singular,'s'))

singular_found = word_freq %>% filter(value %in% stat_terms_singular[['plural']]) %>% 
  rename('plural'=value,'n_plural'=n) %>% right_join(singular_found,by='plural') %>%
  select(singular,plural,n_singular,n_plural)

#find most common form
singular_found = singular_found %>% rownames_to_column('id') %>%  # creates an ID number
  gather(form,n, n_singular:n_plural) %>% 
  group_by(id) %>% 
  slice(which.max(n)) %>% ungroup()
#mutate:least_common,most_common
singular_found = singular_found %>% mutate(least_common = if_else(form=='n_singular',plural,singular),
                                           most_common = if_else(form=='n_singular',singular,plural))
#write to .csv file
#replace least_common with most_common
least_common_terms = str_c("\\b",singular_found[['least_common']],"\\b",collapse="|")

change_least_common = function(input){
  singular_found %>% filter(least_common==input) %>% pull(most_common)
}
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           least_common_terms,
                                                                           change_least_common))


#remove excess whitespace
stats_section = stats_section %>% mutate(text_data_clean = replace_white(text_data_clean))

#final spell check TODO

#remove stop words? TODO
#remove_stopwords = str_c("\\b",stopwords('en'),"\\b",collapse='|')
#stats_section = stats_section %>% mutate(text_data_clean = str_remove_all(text_data_clean,remove_stopwords))


stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'doi_',''))
stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'.journal','/journal'))

#choose 100 dois to check cleaning
sample_dois = stats_section %>% distinct(doi) %>% sample_n(.,100)

sample_data = stats_section %>% filter(doi %in% sample_dois[['doi']])

#split stats_section into batches to reduce file size
#save(stats_section,file='data/stats_section_cleaned.rda')

ngrps = 5
out = stats_section %>% mutate(batch= (row_number()-1) %/% (n()/ngrps)) 
out_list = split(out,out$batch)

lapply(1:ngrps,function(b){
  output = out_list[[b]] %>% select(-batch)
  save(output,file=paste0('data/stats_section_cleaned_',b,'.rda'))
})
#write.table(stats_section %>% select(-text_data),file='data/stats_section_cleaned.txt',sep='\t',row.names = F)

#merge with meta-data
#meta_dat = readRDS('./data/plos_searchresults_metadata.rda')

# out = right_join(meta_dat,stats_section,by='doi') %>%
#   rename('counter_total_all' = citations) %>%
#   select(-text_data)

#not run - save on git repo?
#split into 5 batches to reduce file size
# ngrps = 5
# out = out %>% mutate(batch= (row_number()-1) %/% (n()/ngrps)) 
# out_list = split(out,out$batch)
# 
# lapply(1:5,function(b) write.table(out_list[[b]] %>% 
#                                      select(-batch),file=paste0('data/stats_section_cleaned_',b,'.txt'),
#                                    sep='\t',row.names=F))
# 
# 
# 
# meta_dat = meta_dat %>% filter(doi %in% doi_list)
# write.table(meta_dat,file='data/stats_section_metadata.txt',sep='\t',row.names = F)
