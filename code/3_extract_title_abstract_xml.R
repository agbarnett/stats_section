#3_extract_titles_abstracts

library(rplos)
library(XML)
library(tidyverse)

#plos full text - e,g, batch 1
batch.number = 1
meta_dat = readRDS(paste0('./data/plos_meta_',batch.number,'.RDS'))

doi_list = meta_dat %>% pull(doi)
dat = list()

for(i in 1:100){
  #extract xml of full text. if no xml available, skip to the next doi
  full_text <- tryCatch(
    plos_fulltext(doi_list[i]),
    error=function(e) e
  )
  
  if(inherits(full_text, "error")) next

  full_text_xml <- xmlParse(full_text,asText=T,useInternalNodes = T,encoding='UTF-8')
  methods_text = list()
  title_abstract = list()
  node_info.title = getNodeSet(full_text_xml,"//front//title-group//article-title")
  node_info.abstract = getNodeSet(full_text_xml,"//front//abstract")
  
  title_abstract[['title']] = xmlValue(node_info.title)
  #first try extracting abstract minus section headings (eg aim, methods)
  title_abstract[['abstract']] = paste(unlist(as.character(sapply(node_info.abstract,xpathSApply,"./sec/p",xmlValue))),collapse=' ')
  #if nothing returned due to no section headings available, extract abstract as single block of text
  if (title_abstract[['abstract']]=='list()'){
    title_abstract[['abstract']] = xmlValue(node_info.abstract)
  }
  
  
  if (!is.null(title_abstract[['title']]) & !is.null(title_abstract[['abstract']])){
    dat[[i]] = bind_cols(title_abstract) %>% add_column(doi=doi_list[i]) %>% mutate_if(is.factor,as.character)

  }
}
dat = bind_rows(dat)
save(dat,file=paste0('./data/title_abstract_batch_',batch.number,'.rda'))