#combine_batch_info
library(tidyverse)

#filter: identify dois with statitical analyses subsection based on text_heading (partial string matching)
analysis_keywords = c('statistical analysis','statistical analyses',
                      'statistical method','statistics',
                      'data analysis', 'data analyses')
#loop over batches
batch = 1:5
stats_section = list()
stats_section_excluded = list()
for (b in batch){
  folder_name = paste0('data/full text records/batch_',b)
  results_by_doi = list.files(folder_name)
  doi_list  = gsub('.rda','',results_by_doi)
  text_results = lapply(results_by_doi,
                        function(x) {load(paste(folder_name,x,sep='/'))
                          return(dat %>% mutate_if(is.factor,as.character))}
  )
  names(text_results) <- doi_list
  text_results = bind_rows(text_results,.id='doi')
  stats_section[[b]] = text_results %>% filter(grepl(paste(analysis_keywords,collapse='|'),text_heading,
                                       ignore.case=T,fixed=F))
  stats_section_excluded[[b]] = text_results %>% filter(!grepl(paste(analysis_keywords,collapse='|'),text_heading,
                                                              ignore.case=T,fixed=F))
}

save(stats_section,file='data/stats_section_info.rda')
save(stats_section_excluded,file='data/stats_section_excluded_plos.rda')
