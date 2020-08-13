#6_process_meta
#explore meta data for final set of plos records
library(tidyverse)
#load meta data for all records found
load('plos_meta_data.rda')

#load final stats_section
load('stats_section_info.rda')

#bind_rows
stats_section = bind_rows(stats_section)

#pull doi from stats_section; mutate doi
doi_list = stats_section %>% distinct(doi) %>% mutate(doi = str_replace_all(doi,'.*10.1371.','10.1371/'))


#filter meta data
meta_stats_section = meta_dat_allrecords %>% filter(doi %in% doi_list[['doi']])

#split subject_level_1; gather/filter
stats_section_subject = separate(meta_stats_section %>% select(doi,subject_level_1), subject_level_1, into = paste0("subject", 1:10), sep = ',')
stats_section_subject = gather(stats_section_subject,subject_level_number,subject_level_name,-doi) %>% filter(!is.na(subject_level_name))

#str_remove
stats_section_subject = stats_section_subject %>% mutate(subject_level_number = str_remove_all(subject_level_number,'subject'))

#count total subjects per doi
stats_section_subject %>% count(subject_level_number)

#count subject_level_name
stats_section_subject %>% count(subject_level_name)

#create indicator matrix for subjects by doi
doi_subject_matrix = stats_section_subject %>% group_by(doi) %>% count(subject_level_name) %>% 
  ungroup() %>% spread(.,subject_level_name,n,fill=0) 

#number of dois per subject area
doi_subject_matrix %>% summarise_at(vars("Biology and life sciences":"Social sciences"),sum)