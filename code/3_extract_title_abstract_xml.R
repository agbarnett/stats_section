#3_extract_titles_abstracts

library(rplos)
library(XML)
library(tidyverse)

#plos full text - loop over batches

for (batch.number in 1:5){
meta_dat = readRDS(paste0('./data/plos_meta_',batch.number,'.RDS'))
doi_list = meta_dat %>% pull(doi)

dat.batch_titleabs = list()
dat.batch_subjects = list()

for(i in 1:length(doi_list)){
  #extract xml of full text. if no xml available, skip to the next doi
  full_text <- tryCatch(
    plos_fulltext(doi_list[i]),
    error=function(e) e
  )
  
  if(inherits(full_text, "error")) next

  full_text_xml <- xmlParse(full_text,asText=T,useInternalNodes = T,encoding='UTF-8')
  
  #title and abstract
  title_abstract = list()
  node_info.title = getNodeSet(full_text_xml,"//front//title-group//article-title")
  node_info.abstract = getNodeSet(full_text_xml,"//front//abstract")
  
  #subject classifications
  subjects = list()
  node_info.subjects = getNodeSet(full_text_xml,"//front//article-categories/subj-group")
    
    
  title_abstract[['title']] = xmlValue(node_info.title)
  #first try extracting abstract minus section headings (eg aim, methods)
  title_abstract[['abstract']] = paste(unlist(as.character(sapply(node_info.abstract,xpathSApply,"./sec/p",xmlValue))),collapse=' ')
  #if nothing returned due to no section headings available, extract abstract as single block of text
  if (title_abstract[['abstract']]=='list()'){
    title_abstract[['abstract']] = xmlValue(node_info.abstract)
  }
  
  #level 1 classification (e..g biology and life sciences)
  #use as.characters() to force blank cells
  subjects[['level1']] = unlist(as.character(sapply(node_info.subjects,xpathSApply,"./subject",xmlValue)))
  subjects[['level2']] = unlist(as.character(sapply(node_info.subjects,xpathSApply,"./subj-group/subject",xmlValue)))
  subjects[['level3']] = unlist(as.character(sapply(node_info.subjects,xpathSApply,"./subj-group/subj-group/subject",xmlValue)))
    
  if (!is.null(title_abstract[['title']]) & !is.null(title_abstract[['abstract']])){
    dat.batch_titleabs[[i]] = bind_cols(title_abstract) %>% add_column(doi=doi_list[i],.before = 1) %>% mutate_if(is.factor,as.character)
  }
  
  if (!is.null(subjects[['level1']])){ #must have level 1 subject classification at a minimum
    dat.batch_subjects[[i]] = bind_cols(subjects) %>% distinct() %>% 
      arrange(level1,level2,level3) %>% 
      mutate_at(c("level1","level2","level3"),function(z) str_remove_all(z,'list\\(\\)')) %>% 
      add_column(doi=doi_list[i],.before = 1) %>% mutate_if(is.factor,as.character)
    #collapse level 3 by ';', then create one line for levels 1-3
    dat.batch_subjects[[i]] = dat.batch_subjects[[i]] %>% group_by(doi,level1,level2) %>% 
      summarise(level3=str_c(level3,collapse=';'),.groups='drop') %>%
      mutate(classifications = paste(level1,level2,level3,sep='/')) %>% select(doi,classifications) 
    
  }
  
}

dat.title.abstract = bind_rows(dat.batch)
dat.subject.class = bind_rows(dat.batch_subjects)

#save
save(dat.title.abstract,file=paste0('./data/title_abstract_plos_batch.',batch.number,'.rda'))
save(dat.subject.class,file=paste0('./data/subjects.',batch.number,'.rda'))

}


