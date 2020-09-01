library(tidyverse)
library(textclean)
library(tm)
library(spelling)
library(readxl)

load('./data/stats_section_info.rda')
load('./data/unicode_characters.rda')

stat_terms_hyphen = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'hyphen_terms')
stat_terms_single = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'single_terms')
stat_terms_model = read_xlsx('./data/methods_dictionary.xlsx',sheet = 'models')
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
#option to keep text inside brackets is "[()]"
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

#repalce unicode with plain text description (label_clean)
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
#for each hyphenated term, create combined and unique plural terms
#plurals includes entries from stat_terms_model eg regressions to regression
stat_terms_hyphen = stat_terms_hyphen %>% mutate(combined_term = str_remove_all(term,' '))
plural_terms  = unique(c(paste0(c(stat_terms_hyphen[['term']],
                                  stat_terms_hyphen[['combined_term']],
                                  stat_terms_hyphen[['update']]),'s'),
                                  stat_terms_model[['term']],
                                  stat_terms_single[['term']]))

#str_c: define search strings
plural_terms_all = str_c("\\b",plural_terms,"\\b",collapse="|") #to turn plural to singular
stats_terms_all = str_c("\\b",stat_terms_hyphen[['term']],"\\b",collapse="|") #to join method words by hyphen
stats_combined_all = str_c("\\b",stat_terms_hyphen[['combined_term']],"\\b",collapse="|") #to split method words by hyphen
stats_terms_single = str_c("\\b",stat_terms_single[['term']],"\\b",collapse="|")

#change plural terms to singular
stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                           plural_terms_all,
                                                                           function(x) gsub('s$','',x)))

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
                                                                           stats_combined_all,
                                                                           change_stats_combined))

#update common US to GB spelling
other_terms_all = str_c(other_terms[['term']],collapse='|') #incorrect spellings identified
change_other = function(input){
  other_terms %>% filter(term==input) %>% pull(update)
}

stats_section = stats_section %>% mutate(text_data_clean = str_replace_all(text_data_clean,
                                                                            other_terms_all,
                                                                            change_other))

#check for remaining instances
all_words = stats_section %>% unnest(text_data_clean) %>% 
  mutate(y=strsplit(text_data_clean,' ')) %>% pull(y) %>% unlist() 

#word frequencies
word_freq = tibble::enframe(all_words) %>% count(value) %>% arrange(-n)

word_freq %>% filter(value %in% stat_terms_hyphen[['combined_term']])
word_freq %>% filter(value %in% stat_terms_hyphen[['term']])
word_freq %>% filter(value %in% plural_terms)
word_freq %>% filter(value %in% stat_terms_hyphen[['update']])

word_freq %>% filter(value %in% other_terms[['term']])
word_freq %>% filter(value %in% other_terms[['update']])


#remove excess whitespace
stats_section = stats_section %>% mutate(text_data_clean = replace_white(text_data_clean))

stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'doi_',''))
stats_section = stats_section %>% mutate(doi = str_replace_all(doi,'.journal','/journal'))

#choose 100 dois to check quality for data cleaning
sample_dois = stats_section %>% distinct(doi) %>% sample_n(.,100)
sample_data = stats_section %>% filter(doi %in% sample_dois[['doi']])

write_rds(sample_data,path='data/stats_section_sample100.rds',"xz", compression = 9L)
write_rds(stats_section,path='data/stats_section_cleaned.rds',"xz", compression = 9L)

# save as text files (5 batches)
stats_section_txt = stats_section %>% select(-text_data)
#split stats_section into batches to reduce file size
 ngrps = 5
 out = stats_section_txt %>% mutate(batch= (row_number()-1) %/% (n()/ngrps)) 
 out_list = split(out,out$batch)

lapply(1:ngrps,function(b){
  output = out_list[[b]] %>% select(-batch)
  write.table(output,file=paste0('data/stats_section_cleaned_',b,'.txt'),sep='\t',row.names = F)
})


#run spell check (lang=en_GB)
 spelling_errors = lapply(1:nrow(stats_section),function(x) 
   spell_check_text(stats_section[x,]$text_data_clean,lang="en_GB")) %>%
   bind_rows(.id = 'index') %>%
   group_by(word) %>% summarise(n=sum(as.numeric(found))) %>%
   arrange(-n)
 save(spelling_errors,file='./data/common_spelling_errors_gb.rda')

