library(tidyverse)
library(stringr)
library(textclean)
library(tm)
library(spelling)
library(readxl)

load('stats_section_info.rda')

stats_section = bind_rows(stats_section)

#remove all unicode characters
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data,"\\s*<U\\+\\w+>\\s*|<U+\\w+>$"," "))

#remove equations
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(\n)(.*)(\n)\\s*"," "))

#replace >,<,= with text
#\\s* - zero or more whitespaces

#less than
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(<)\\s*"," less than "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"[<]"," less than "))

#greater than
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(>)\\s*"," greater than "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"[>]"," greater than "))

#equal to
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(=)\\s*"," equal to "))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"[=]"," equal to "))

#plus or minus
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(±)\\s*|\\s*(\\+/-)\\s*"," plus or minus "))

#remove dashes 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*(–)\\s*|\\s*(—)\\s*|\\s*(-)\\s*"," "))

#p value (doesn't resolve p less-than, p equal to)
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"p value","p-value"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"p less than","p-value less than"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"p equal to","p-value equal to"))
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"p greater than","p-value greater than"))

#common symbols covered by text clean ($,%,#,@,&,w/)
stats_section = stats_section %>% mutate(text_data_clean = replace_symbol(text_data_clean))

#remove references eg [23]
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*\\[\\d+\\]\\s*",""))

#remove () including text within brackets 
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,"\\s*\\([^\\)]+\\)",""))

#remove punctuation
stats_section$text_data_clean = strip(stats_section$text_data_clean,char.keep = c("~~","."),apostrophe.remove=T,digit.remove=F)

#replace other non-ascii characters
stats_section = stats_section %>% mutate(text_data_clean = replace_non_ascii(text_data_clean))

#make common statistical terms and methods consistent
common_stat_tests = read_xlsx('methods_dictionary.xlsx',sheet = 'common_stat_tests')
other_terms = read_xlsx('methods_dictionary.xlsx',sheet = 'other')

#for each common method, create combined and unique plural terms
common_stat_tests = common_stat_tests %>% mutate(combined_term = str_remove_all(term,' '))
unique_terms = unique(unlist(str_split(common_stat_tests$term,' ')))
plural_terms  = paste0(unique_terms,'s')

#str_c
stats_terms_all = str_c("\\b",c(common_stat_tests$term,common_stat_tests$combined_term),"\\b",collapse="|")
plural_terms_all = str_c("\\b",plural_terms,"\\b",collapse="|")
other_terms_all = str_c("\\b",other_terms$term,"\\b",collapse='|')

change_stats_terms = function(input){
  common_stat_tests %>% filter(combined_term==input) %>% pull(term) %>% gsub(' ','-',.)
}

change_other = function(input){
  other_terms %>% filter(term==input) %>% pull(update) %>% gsub(' ','-',.)
}

#change plural to singlar
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           plural_terms_all,
                                                                           function(x) gsub('.$','',x)))

#hyphenate common stats terms
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           stats_terms_all,
                                                                           change_stats_terms))


#other (include common us/uk spelling)
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           other_terms_all,
                                                                           change_other))

#remove stop words
remove_stopwords = str_c("\\b",stopwords('en'),"\\b",collapse='|')
stats_section = stats_section %>% mutate(text_data_clean = str_remove_all(text_data_clean,remove_stopwords))


#remove excess whitespace
stats_section = stats_section %>% mutate(text_data_clean = replace_white(text_data_clean))


stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'doi_',''))
stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'.journal','/journal'))

save(stats_section,file='data/stats_section_cleaned.rda')
#write.table(stats_section %>% select(-text_data),file='data/stats_section_cleaned.txt',sep='\t',row.names = F)

#merge with meta-data
meta_dat = readRDS('./data/plos_searchresults_metadata.RDS')

out = right_join(meta_dat,stats_section,by='doi') %>%
  rename('counter_total_all' = citations) %>%
  select(-text_data)

#split into 5 batches to reduce file size
ngrps = 5
out = out %>% mutate(batch= (row_number()-1) %/% (n()/ngrps)) 
out_list = split(out,out$batch)

lapply(1:5,function(b) write.table(out_list[[b]] %>% 
                                     select(-batch),file=paste0('data/stats_section_cleaned_',b,'.txt'),
                                   sep='\t',row.names=F))



meta_dat = meta_dat %>% filter(doi %in% doi_list)
write.table(meta_dat,file='data/stats_section_metadata.txt',sep='\t',row.names = F)
