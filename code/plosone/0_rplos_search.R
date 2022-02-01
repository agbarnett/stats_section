library(rplos)
library(rentrez)
library(stringr)
library(XML)
library(tidyverse)


#search all full-text research articles in PLoS One and return meta data only
results_all  =  searchplos("*:*",
             fl=c('id','volume','subject','subject_level_1'),
             fq=list('journal_key:PLoSONE','article_type:"Research Article"','doc_type:full'),
             limit=0)

#number of records by volume (current volume = 15). meta data only
q = paste0("volume:",1:15)
results_by_volume = lapply(q, function(x) searchplos(q=paste(x),
                                                     fq=list('journal_key:PLoSONE','article_type:"Research Article"','doc_type:full'),
                                                     limit=0)) %>%
  setNames(q)

save(results_by_volume,file='data/results_by_volume_info.RData')

#by analysis-related search term
#search terms
#'method' picks up 'methods'
#'modelling' and 'modelling' return same number of records. assume UK spelling
prefix = c('data','statistical')
suffix = c('analysis','analyses','method','methodology','modelling')
search_terms = apply(expand.grid(prefix,suffix),1,paste,collapse=' ')
q = paste0(search_terms,'~1')

results_by_searchterm = lapply(q, function(x) searchplos(q=paste(x),
                                                     fl=c('id','title','volume','counter_total_all','subject','subject_level_1'),
                                                     fq=list('journal_key:PLoSONE','article_type:"Research Article"','doc_type:full'),
                                                     limit=90000)) %>%
  setNames(q)


#meta-data
meta_dat = lapply(results_by_searchterm,function(x) x$data %>% select(id,volume,counter_total_all,subject,subject_level_1))
meta_dat = do.call(rbind.data.frame,meta_dat)
meta_dat = distinct(meta_dat)                            

meta_dat = meta_dat %>% rename('doi'=id,
                               'citations'=counter_total_all)

save(meta_dat,file='data/plos_meta_data.rda')

