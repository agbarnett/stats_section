library(rplos)
library(XML)
library(tidyverse)

#this script extract full text as xml at the DOI level

#notes: 
## full set of records before applying second stage filter are available in 'data/full text records/batch_[1-5]'
## some records did not return xml as part of loop. reasons for this include no section headings to extract under standarised node structure.
## combined dataset for analysis after applying the second stage filter are available in 'data/stats_section_info.rda'
## combined dataset of records that did not pass the second stage filter are available in 'data/stats_section_excluded_plos.rda'


#plos full text - e,g, batch 1
meta_dat = readRDS('./data/plos_meta_1.RDS')

doi_list = meta_dat %>% pull(doi)

for(i in 1:length(doi_list)){

  full_text <- plos_fulltext(doi_list[i])
  full_text_xml <- xmlParse(full_text,asText=T,useInternalNodes = T,encoding='UTF-8')
  methods_text = list()
  node_info = getNodeSet(full_text_xml,"//sec")
  
  methods_text[['text_heading']] = sapply(node_info,xpathSApply,"./title",xmlValue)
  methods_text[['text_heading']] =  unlist(as.character(methods_text[['text_heading']]))

  text_out = sapply(node_info,xpathSApply,"./p",xmlValue)
  methods_text[['text_data']] = sapply(text_out,function(s) tolower(paste(s,collapse='')))
  
  if (!is.null(methods_text[['text_heading']]) & !is.null(methods_text[['text_data']])){
    dat = do.call(cbind.data.frame,methods_text)
    rownames(dat) <- NULL
    dat = as_tibble(dat) %>% mutate_if(is.factor,as.character)
    
    #remove empty text fields
    dat = dat %>% filter(text_data!="")
    
    #save as text file. save non-empty tibbles only
    if (nrow(dat)>0){
      fileOut = gsub('/','.',doi_list[i])
      save(dat,file=paste0('./data/full text records/batch_1/doi_',fileOut,'.rda'))
    }
  }
}



