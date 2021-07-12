#3_plot_excluded
# CONSORT style diagram of included/excluded studies. PLOS One case study
# May 2021
library(tidyverse)
library(diagram)

#load metadata
load('results/total.records.plos.rda')

#load excluded sections
load('data/stats_section_excluded_plos.rda')
stats_section_excluded = bind_rows(stats_section_excluded)

#number downloaded through target search queries
n.api = total.records[[1]] %>% filter(stage=='API') %>% pull(n) %>% sum()
#number of stats sections included following partial matching
n.include = total.records[[1]] %>% filter(stage=='Included in analysis') %>% pull(n) %>% sum()


#original search terms
prefix = c('data','statistical')
suffix = c('analysis','analyses','method','methodology','modelling')
search_terms = apply(expand.grid(prefix,suffix),1,paste,collapse=' ') %>% str_c(.,collapse='|')

excluded_text_heading = stats_section_excluded %>% filter(grepl(search_terms,text_heading))
excluded_text_data = stats_section_excluded %>% filter(grepl(search_terms,text_data))

#view most common sections
excluded_headings_n = excluded_text_data %>% count(text_heading) %>% arrange(-n)

generic_methods_heading = excluded_headings_n %>% filter(grepl('^materials$|^methods$|material(.*)method|method(.*)material',text_heading,fixed=F,ignore.case = T))
analysis_heading = excluded_headings_n %>% filter(grepl('Analysis',text_heading,ignore.case = T)) %>% pull(n) %>% sum()
results_discussion_headings = excluded_headings_n %>% filter(grepl('results|discussion',text_heading,ignore.case=T)) %>% pull(n) %>% sum()
intro_headings = excluded_headings_n %>% filter(grepl('introduction',text_heading,ignore.case=T)) %>% pull(n) %>% sum()

#materials and methods section
materials_methods_n = generic_methods_heading %>% pull(n) %>% sum()
materials_methods_doi = excluded_text_data %>% filter(grepl('^materials$|^methods$|material(.*)method|method(.*)material',
                                    text_heading,fixed=F,ignore.case = T)) %>% pull(doi) %>% unique()

#section headings excluded materials and methods dois
introduction_n = excluded_text_data %>% filter(!doi %in% materials_methods_doi,grepl('introduction',text_heading,ignore.case=T)) %>% distinct(doi) %>% nrow()
results_discussion_n = excluded_text_data %>% filter(!doi %in% materials_methods_doi,grepl('results|discussion',text_heading,ignore.case=T)) %>% distinct(doi) %>% nrow()
analysis_n = excluded_text_data %>% filter(!doi %in% materials_methods_doi,grepl('analys(.*)s',text_heading,ignore.case=T),!grepl('statistic|method',text_heading,ignore.case=T)) %>% distinct(doi) %>% nrow()


count_exclude = tibble(reason=c('Materials & Methods','Introduction','Results &/or Discussion','Non-specific analysis'),
                       n=c(materials_methods_n,introduction_n,results_discussion_n,analysis_n))

## a) function to make diagram ANZCTR
make_diagram_plos = function(){
  par(mai=c(0,0,0,0))
  labels = c(paste('Downloaded\n n = ', format(n.api, big.mark = ','), sep=''),
             paste('Excluded by second stage filter\n- ', count_exclude$reason[1], ', n = ', format(count_exclude$n[1],big.mark=','),
                   '\n- ', count_exclude$reason[2], ', n = ', count_exclude$n[2],
                   '\n- ', count_exclude$reason[3], ', n = ', count_exclude$n[3],
                   '\n- ', count_exclude$reason[4], ', n = ', format(count_exclude$n[4],big.mark=','), sep=''),
             paste('Analysed\n n =', format(n.include, big.mark = ',')))
  n_labels = length(labels)
  M = matrix(nrow=n_labels, ncol=n_labels)
  M[3,1] = "' '" 
  pos = matrix(data=c(0.29,0.8,
                      0.7,0.55,
                      0.29,0.3), ncol=2, byrow=TRUE)
  sizes=c(1.5,2.41,1.5) / 10
  props = c(0.5,0.5,0.5) # narrower for first and last
  plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
  shape::Arrows(x0=0.29, x1=0.45, y0=0.55, y1=0.55, arr.width=0.2, arr.length=0.22, arr.type='triangle')
  # heading
  #text(0.5, 0.95, "PLOS ONE", font=2)
}

# export
jpeg('manuscript/asa_template/figures/excluded_plosone.jpg', width=6, height=5, units='in', res=300)
make_diagram_plos()
dev.off()



#load data from excluded dois
load('data/stats_section_excluded_plos.rda')
stats_section_excluded = bind_rows(stats_section_excluded)


