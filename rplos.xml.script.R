library(rplos)
library(rentrez)
library(stringr)
library(XML)
library(dplyr)
library(lubridate)

#search terms
search_terms = c('statistical method','statistical methodology')
q = paste0('materials_and_methods:"',search_terms,'"')

out = lapply(q, function(x) searchplos(x, 
                                       fl=c('id','title','publication_date','counter_total_all'),
                                       fq=list('journal_key:PLoSONE','article_type:"Research Article"','doc_type:full'),
                                       limit=100))

numFound = sapply(out,function(x) x$meta$numFound)
search_n = data.frame(search_term = q, n = numFound)

#example doi for xml code - NW paper
doi = "10.1371/journal.pone.0182455"
res <- plos_fulltext(doi) # standard
tmp <- xmlParse(res,asText=T,useInternalNodes = T)

#publication meta-data
meta_dat = lapply(out,function(x) x$data %>% select(id,publication_date,counter_total_all))
meta_dat = do.call(rbind.data.frame,meta_dat)
meta_dat = meta_dat %>% mutate(year = year(publication_date),
                                     month = month(publication_date))                                   
                  
#extract text only
methods_text = xpathApply(tmp,"//sec[@sec-type='materials|methods']//p",xmlValue) %>% unlist()
methods_text = paste(methods_text,collapse=' ')
                     
#lower case
methods_text = tolower(methods_text)

#remove references '[]'
methods_text = str_remove_all(methods_text,'\\[.*\\]')
