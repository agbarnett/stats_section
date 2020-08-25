library(tidyverse)
library(textclean)
library(tm)
library(spelling)
library(readxl)

load('./data/stats_section_info.rda')
load('./data/unicode_characters.rda')


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
common_stat_terms = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'stats_terms')
other_terms = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'other')

#for each common method, create combined and unique plural terms
common_stat_terms = common_stat_terms %>% mutate(combined_term = str_remove_all(term,' '))
plural_terms  = unique(paste0(c(common_stat_terms$term,common_stat_terms$combined_term,common_stat_terms$update),'s'))

#str_c
stats_terms_all = str_c("\\b",common_stat_terms[['term']],"\\b",collapse="|")
stats_terms_combined = str_c("\\b",common_stat_terms[['combined_term']],"\\b",collapse="|")
plural_terms_all = str_c("\\b",plural_terms,"\\b",collapse="|")
other_terms_all = str_c("\\b",other_terms[['term']],"\\b",collapse='|')

change_stats_terms = function(input){
  common_stat_terms %>% filter(term==input) %>% pull(update)
}
change_stats_combined = function(input){
    common_stat_terms %>% filter(combined_term==input) %>% pull(update)
}

change_other = function(input){
  other_terms %>% filter(term==input) %>% pull(update) 
}

#other (include common us/uk spelling)
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           other_terms_all,
                                                                           change_other))

#change plural to singular terms
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           plural_terms_all,
                                                                           function(x) gsub('s$','',x)))

#standardise common stats terms
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           stats_terms_all,
                                                                           change_stats_terms))

#combined stats terms
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           stats_terms_combined,
                                                                           change_stats_combined))

#choose random 1% sample for check progress of text cleaning
test_data= sample_frac(stats_section, 0.01)
View(test_data)

#remove excess whitespace
stats_section = stats_section %>% mutate(text_data_clean = replace_white(text_data_clean))

#remove stop words? TODO
#remove_stopwords = str_c("\\b",stopwords('en'),"\\b",collapse='|')
#stats_section = stats_section %>% mutate(text_data_clean = str_remove_all(text_data_clean,remove_stopwords))


stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'doi_',''))
stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'.journal','/journal'))

save(stats_section,file='data/stats_section_cleaned.rda')
#write.table(stats_section %>% select(-text_data),file='data/stats_section_cleaned.txt',sep='\t',row.names = F)

#merge with meta-data
#meta_dat = readRDS('./data/plos_searchresults_metadata.rda')

out = right_join(meta_dat,stats_section,by='doi') %>%
  rename('counter_total_all' = citations) %>%
  select(-text_data)


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
