#6_process_meta
#explore meta data for final set of plos records
library(tidyverse)
library(ggalluvial)
#load meta data for all records found
load('data/plos_meta_data.rda')

g.theme = theme_bw() + theme(legend.title = element_blank(),legend.position = 'top',
                             legend.direction = 'horizontal',axis.text = element_text(size=12),
                             axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

#number of unique API search records
n_api = meta_dat_allrecords %>% distinct(doi,.keep_all = F) %>% nrow()


#load final stats_section
load('data/stats_section_info.rda')

#bind_rows
stats_section = bind_rows(stats_section)

#pull doi from stats_section; mutate doi
doi_list = stats_section %>% distinct(doi) %>% mutate(doi = str_replace_all(doi,'.*10.1371.','10.1371/'))


#filter meta data
meta_stats_section = meta_dat_allrecords %>% filter(doi %in% doi_list[['doi']])

n_stats.section = meta_stats_section %>% distinct(doi,.keep_all = F) %>% nrow()

#compare % of records that passed filter by volume
to_plot_1 = meta_dat_allrecords %>% distinct(doi,.keep_all = T) %>% count(volume)
to_plot_2 = meta_stats_section %>% distinct(doi,.keep_all = T) %>% count(volume)

to_plot = bind_rows('API' = to_plot_1,'Included in analysis' = to_plot_2,.id='stage') %>%
  mutate(stage = factor(stage,levels=c('API','Included in analysis')))

g = ggplot(to_plot,aes(x=volume,y=n,fill=stage)) + geom_bar(stat='identity',position='dodge') + 
  scale_fill_grey(guide = guide_legend(reverse = TRUE) ) + 
  expand_limits(y=c(0,20000))+
  scale_y_continuous('Total records',breaks = seq(0,20000,2500)) + 
  scale_x_continuous('Volume',breaks = 1:15) + g.theme
jpeg('manuscript/figures/searchresults.plos.jpg',width=600,height=480,units='px')
g
invisible(dev.off())
png('manuscript/figures/searchresults.plos.png',width=600,height=480)
g
invisible(dev.off())

#find peak
ttab_byvolume  = to_plot %>% spread(stage,n) %>% arrange(-API)
#percentage of records that met partial matching filters
ttab_byvolume = mutate(ttab_byvolume,percentage = round(100*`Included in analysis`/API,1))

ttab_byvolume %>% filter(percentage==min(percentage)|percentage==max(percentage))

#save 
total.records = list(to_plot,ttab_byvolume)
save(total.records,file='manuscript/total.records.plos.rda')


#split subject_level_1; gather/filter
stats_section_subject = separate(meta_stats_section %>% select(doi,subject_level_1), subject_level_1, into = paste0("subject", 1:10), sep = ',')
stats_section_subject = gather(stats_section_subject,subject_level_number,subject_level_name,-doi) %>% filter(!is.na(subject_level_name))

#str_remove
stats_section_subject = stats_section_subject %>% mutate(subject_level_number = str_remove_all(subject_level_number,'subject'))

#count total subjects per doi
stats_section_subject %>% count(subject_level_number)

#count subject_level_name
stats_section_subject %>% count(subject_level_name) %>% arrange(-n)

save(stats_section_subject,file='manuscript/stats_section_subject.classifications.rda')



#top 3 subject classifications - overall
to_plot = stats_section_subject %>% filter(subject_level_number %in% c(1,2,3)) %>%
  select(doi,subject_level_number,subject_level_name) %>% 
  mutate(subject_level_name = factor(subject_level_name)) %>%
  spread(subject_level_number,subject_level_name) %>% mutate(pattern=as.numeric(factor(paste0(`1`,`2`,`3`)))) %>% 
  arrange(pattern)

to_plot = to_plot %>% gather(subject_level_number,value,-doi,-pattern) %>% 
  count(subject_level_number,pattern,value)
ggplot(filter(to_plot,!is.na(value)),
       aes(x = subject_level_number, stratum = value, alluvium = pattern,
           y = n,
           fill = value)) +
  scale_x_discrete('Subject classification',expand = c(.1, .1)) +
  scale_y_continuous('Number of full-text records')+
  geom_flow() +
  geom_stratum(alpha = .5) +
 g.theme

