#7_summarise_stats_sections.R
library(tidyverse)
library(tidytext)
dat = readRDS('data/stats_section_cleaned.rds')

tidy_dat = dat %>%
  unnest_tokens(word,text_data_clean)

wordcounts <- tidy_dat %>%
  group_by(doi) %>%
  summarise(words = n(),.groups='drop')

#join with metadata
#load meta data for all records found
load('./data/plos_meta_data.rda')

wordcounts = left_join(wordcounts,meta_dat_allrecords,by='doi')
wordcounts %>% group_by(volume) %>% summarise(med=median(words),
                                              Min=min(words),Max=max(words),
                                              q1=quantile(words,.25),q3=quantile(words,.75),.groups='drop')


ggplot(wordcounts,aes(y=log(words),x=volume,group=volume))+geom_boxplot()  
?geom_boxplot
