library(rplos)
library(rentrez)
library(stringr)
library(XML)
library(dplyr)
library(lubridate)

#search terms
search_terms = c('data analysis','statistical analysis','statistical method','statistical methodology')
q = paste0('materials_and_methods:"',search_terms,'"')

results = lapply(q, function(x) searchplos(x,
                                       fl=c('id','title','publication_date','counter_total_all','subject','subject_level_1'),
                                       fq=list('journal_key:PLoSONE','article_type:"Research Article"','doc_type:full'),
                                       limit=1000)) %>%
  setNames(q)


numFound = sapply(results,function(x) x$meta$numFound)
search_n = data.frame(search_term = search_terms, n = numFound)
#publication meta-data
meta_dat = lapply(results,function(x) x$data %>% select(id,publication_date,counter_total_all,subject,subject_level_1))
meta_dat = do.call(rbind.data.frame,meta_dat)
meta_dat = distinct(meta_dat)
meta_dat = meta_dat %>% mutate(year = year(publication_date),
                               month = month(publication_date))                                   

meta_dat = meta_dat %>% rename('doi'=id,
                    'citations'=counter_total_all)
                  

#pull dois and select random sample of 100
doi_list = pub_details %>% pull(doi)
sample_dois = sample(length(doi_list),100)
full_text <- plos_fulltext(doi_list[sample_dois])
full_text_xml <- lapply(full_text,function(x) xmlParse(x,asText=T,useInternalNodes = T))


#extract subsection headings in materials and methods (for all subheadings, change /sec to //sec)
subsection_headings = lapply(full_text_xml, function(x) xpathApply(x,"//sec[@sec-type='materials|methods']/sec/title",xmlValue) %>%
                                ldply()) %>% bind_rows(.,.id='doi')
subsection_headings = dplyr::rename(subsection_headings,label=V1)
View(subsection_headings)

#filter subsection headings to isolate data analysis section - needs work to return results for every paper
analysis_keywords = c('data analysis','statistical analysis','statistical method')
dplyr::filter(subsection_headings,grepl(paste(analysis_keywords,collapse='|'),label,ignore.case = T))

#main function to pull of headings and text within material and methods section
#can also use susbsubheadings (//sec)
out = list()
for (x in 1:length(full_text_xml)){
  methods_text = list()
  node_info = getNodeSet(full_text_xml[[x]],"//sec[@sec-type='materials|methods']/sec")
  methods_text[['text_heading']] = sapply(node_info,xpathSApply,"./title",xmlValue)
  text_out = sapply(node_info,xpathSApply,"./p",xmlValue)
  methods_text[['text_data']] = sapply(text_out,function(s) tolower(paste(s,collapse='')))
  dat = do.call(cbind.data.frame,methods_text)
  rownames(dat) <- NULL
  dat = as_tibble(dat) %>% mutate_if(is.factor,as.character)
  out[[x]] = dat
}
#bind rows to get full dataset by doi
names(out) = doi_list[sample_dois]
dat = bind_rows(out,.id='doi')

#remove references '[]' - needs works as removes everything in [] - process later
#dat = dat %>% mutate(text_norefs = str_remove_all(text_data,'\\[.*\\]'))

#join with meta data
dat = dat %>% left_join(.,meta_dat,by='doi')

save(dat,subsection_headings,
     meta_dat,search_n,file='sample_plos_data.RData')
