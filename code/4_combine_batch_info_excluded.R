#combine_batch_info_excluded
library(tidyverse)

#original search terms
prefix = c('data','statistical')
suffix = c('analysis','analyses','method','methodology','modelling')
search_terms = apply(expand.grid(prefix,suffix),1,paste,collapse=' ')

load('../data/plos_meta_data.rda')
stats_sections = readRDS('../data/stats_section_cleaned.rds')
excluded_dois = anti_join(meta_dat_allrecords,stats_sections,by='doi') %>% pull(doi)

#loop over batches
batch = 1:5
#stats_section = list()
stats_section_excluded = list()
for (b in batch){
  folder_name = paste0('Z:/rplos/data/full text records/batch_',b) #full text on hpc drive
  results_by_doi_orig = list.files(folder_name) %>% str_remove_all(.,'doi_|.rda')
  results_by_doi = intersect(results_by_doi_orig,excluded_dois  %>% str_replace_all(.,pattern='/',replacement='.')) #filter to excluded dois
  text_results = lapply(results_by_doi,
                        function(x) {load(paste0(folder_name,'/doi_',x,'.rda'))
                          return(dat %>% mutate_if(is.factor,as.character))}
  )
  names(text_results) <- results_by_doi
  text_results = bind_rows(text_results,.id='doi')
  #stats_section[[b]] = text_results %>% filter(grepl(paste(analysis_keywords,collapse='|'),text_heading,
  #                                                   ignore.case=T,fixed=F))
  stats_section_excluded[[b]] = text_results %>% 
    filter(grepl(str_c(search_terms,collapse='|'),text_heading,ignore.case=T,fixed=F)|
             grepl(str_c(search_terms,collapse='|'),text_data,ignore.case=T,fixed=F))
}

#save(stats_section,file='data/stats_section_info.rda')
save(stats_section_excluded,file='../data/stats_section_excluded_plos.rda')
