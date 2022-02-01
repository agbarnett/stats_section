#FIG_PLOS_plot_excluded
# CONSORT style diagram of included/excluded studies. PLOS One case study
# May 2021
library(tidyverse)
library(diagram)

#load aggregate data on total records downloaded, analysed
load('results/total.records.plos.rda')
n_downloaded = filter(total.records[[1]],stage=='API') %>% pull(n) %>% sum()
n_analysed = filter(total.records[[1]],stage=='Included in analysis') %>% pull(n) %>% sum()
#load section included/excluded from analysis
load('data/stats_section_info.rda')
load('data/stats_section_excluded_plos.rda')
stats_section_included = bind_rows(stats_section)
stats_section_excluded = bind_rows(stats_section_excluded)

#number downloaded through target search queries
n.api = total.records[[1]] %>% filter(stage=='API') %>% pull(n) %>% sum()
#number of stats sections included following partial matching
n.include = total.records[[1]] %>% filter(stage=='Included in analysis') %>% pull(n) %>% sum()


#original search terms
prefix = c('data','statistical')
suffix = c('analysis','analyses','method','methods','methodology','modelling')
search_terms = apply(expand.grid(prefix,suffix),1,paste,collapse=' ') %>% str_c(.,collapse='|')

excluded_text_data = stats_section_excluded %>% filter(grepl(search_terms,text_data))

#view most commonly excluded sections as examples for main text
excluded_headings_n = excluded_text_data %>% count(text_heading) %>% arrange(-n)
intro_headings = excluded_headings_n %>% filter(grepl('introduction',text_heading,ignore.case=T)) 
material_methods_heading = excluded_headings_n %>% filter(grepl('^materials$|^methods$|material(.*)method|method(.*)material',text_heading,fixed=F,ignore.case = T))
nonspecific_analysis_heading = excluded_headings_n %>% filter(grepl('analys(.*)s',text_heading,ignore.case = T)) 
results_discussion_headings = excluded_headings_n %>% filter(grepl('results|discussion',text_heading,ignore.case=T))


#determine number of studies that were excluded based on the appearance of original search terms in different parts of an article
#materials and methods section first, then expand to other common section headings (introduction, results, discussion)
count_exclude = list()
## i) under generic material and methods section header
materials_methods_doi = excluded_text_data %>% filter(grepl('^materials$|^methods$|material(.*)method|method(.*)material',
                                                            text_heading,fixed=F,ignore.case = T)) %>% distinct(doi) %>% pull(doi)
count_exclude[['Materials & Methods']] = length(materials_methods_doi)

## ii) non-specific analysis heading; e.g. 'microarray analysis' cannot tell if statistical methods are included
nonspecific_analysis_doi = excluded_text_data %>% filter(!doi %in% c(materials_methods_doi),grepl('analys(.*)s',text_heading,ignore.case=T)) %>% distinct(doi) %>% pull(doi)
count_exclude[['Non-specific analysis']] = length(nonspecific_analysis_doi)
  
# other parts of the article, excluding the materials and methods section
#Introduction 
introduction_doi = excluded_text_data %>% filter(!doi %in% c(materials_methods_doi,nonspecific_analysis_doi),grepl('introduction',text_heading,ignore.case=T)) %>% distinct(doi) %>% pull(doi)
count_exclude[['Introduction']] = length(introduction_doi)

#results/discussion (excl. intro to avoid double counts)
results_discussion_doi = excluded_text_data %>% filter(!doi %in% c(materials_methods_doi,nonspecific_analysis_doi,introduction_doi),grepl('results|discussion',text_heading,ignore.case=T)) %>% distinct(doi) %>% pull(doi)
count_exclude[['Results &/or Discussion']] = length(results_discussion_doi)

#other. includes non-matching section headings, records skipped by xml step (e.g different xml format, no section headings to determine eligibility)
other_n = n_downloaded-n_analysed-sum(unlist(count_exclude))
count_exclude[['Other']] = other_n

count_exclude = bind_rows(count_exclude) %>% gather(key=reason,value=n)


#end exclusions

#tally most common section headings for inclusion in flowchart
common_headings = c('statistical analysis','statistical analyses','statistical method','statistical methods','statistical methodology','statistics','data analysis','data analyses')

#get exact matches to common_headings
included_studies = stats_section_included %>%
  filter(grepl(str_c(common_headings,collapse='|'),ignore.case = T,text_heading)) %>%
  mutate(text_heading = tolower(text_heading))

total_matches = included_studies %>% distinct(doi) %>% nrow()

#determine how many studies returned a full/exact match to common_headings
matching_section_headings = tibble(text=common_headings,n=0)

for (x in 1:length(common_headings)){
  section_heading = paste0('^',common_headings[x],'$')
  matching_section_headings[x,'n'] = filter(included_studies,grepl(section_heading,text_heading)) %>% distinct(doi) %>% nrow()
}

#remaidner will be number of partial matches
total_exact_matches = sum(matching_section_headings[['n']])
matching_section_headings = matching_section_headings %>% add_row(tibble('text'='partial string match',n=n_analysed - total_exact_matches))

matching_section_headings = matching_section_headings %>% mutate(label = str_to_title(text)) 

#end inclusions

## a) function to make diagram ANZCTR
make_diagram_plos = function(){
  par(mai=c(0,0,0,0))
  labels = c(paste('Downloaded\n n = ', format(n.api, big.mark = ','), sep=''),
             paste('Excluded by second stage filter: n = ',format(n_downloaded-n_analysed,big.mark=','),
                   '\n- ', count_exclude$reason[1], ', n = ', format(count_exclude$n[1],big.mark=','),
                   '\n- ', count_exclude$reason[2], ': n = ', format(count_exclude$n[2],big.mark=','),
                   '\n- ', count_exclude$reason[3], ': n = ', format(count_exclude$n[3],big.mark=','),
                   '\n- ', count_exclude$reason[4], ': n = ', format(count_exclude$n[4],big.mark=','), 
                   '\n- ', count_exclude$reason[5], ': n = ', format(count_exclude$n[5],big.mark=','), 
                   sep=''),
             paste('Analysed: n =', format(n.include, big.mark = ','),
                   '\n ', matching_section_headings$label[1],': n = ', format(matching_section_headings$n[1],big.mark = ','),
             ';\t ', matching_section_headings$label[2],': n = ', format(matching_section_headings$n[2],big.mark = ','),
             '\n ', matching_section_headings$label[3],': n = ', format(matching_section_headings$n[3],big.mark = ','),
             ';\t ', matching_section_headings$label[4],': n = ', format(matching_section_headings$n[4],big.mark = ','),
             '\n ', matching_section_headings$label[5],': n = ', format(matching_section_headings$n[5],big.mark = ','),
             ';\t ', matching_section_headings$label[6],': n = ', format(matching_section_headings$n[6],big.mark = ','),
             '\n ', matching_section_headings$label[7],': n = ', format(matching_section_headings$n[7],big.mark = ','),
             ';\t ', matching_section_headings$label[8],': n = ', format(matching_section_headings$n[8],big.mark = ','),
             '\n ', matching_section_headings$label[9],': n = ', format(matching_section_headings$n[9],big.mark = ','),
             sep=''))
  n_labels = length(labels)
  M = matrix(nrow=n_labels, ncol=n_labels)
  M[3,1] = "' '" 

  pos = matrix(data=c(0.29,0.8,
                      0.66,0.55,
                      0.29,0.2), ncol=2, byrow=TRUE)
  sizes=c(1.3,2.01,2.55) / 10
  props = c(0.4,0.5,0.27) # narrower for first and last
  plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
  shape::Arrows(x0=0.29, x1=0.45, y0=0.55, y1=0.55, arr.width=0.2, arr.length=0.22, arr.type='triangle')
  # heading
  #text(0.5, 0.95, "PLOS ONE", font=2)
}

# # export
# tiff('manuscript/plos_one/revised figures/S1_Fig_R1_plosone.tif', width=10, height=8, units='in', res=300)
# make_diagram_plos()
# dev.off()

