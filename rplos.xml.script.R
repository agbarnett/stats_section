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

#extract text
methods_text = xpathApply(tmp,"//sec[@sec-type='materials|methods']",xmlValue)

#publication details
pub_details = lapply(out,function(x) x$data %>% select(id,publication_date,counter_total_all))
pub_details = do.call(rbind.data.frame,pub_details)

#extract month and year of publication
pub_details = pub_details %>% mutate(year = year(publication_date),
                                     month = month(publication_date))
