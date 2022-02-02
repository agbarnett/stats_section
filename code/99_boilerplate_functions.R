#99_boilerplate_functions.R
cleanup_plos_results<-function(...){
  # data management 
  matches = ungroup(matches) %>%
    mutate(topic_num = as.numeric(str_remove(topic_id, pattern='Topic ')), # topic number
           doi_num = str_remove(doi, pattern='10.1371/journal\\.pone\\.')) # just get the number from the DOI
  
  #tidy up a bit more to account for overuse of full stops (affects breakdown by sentence)
  #single number followed by full stop (e.g. [space]1.[space]); reomve number
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})([:digit:]{1})(\\.)(\\s{1})",replacement="\\3\\4"))
  
  #a single [a-z] character, predeeced and followed by a full stop[space]
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})([:alpha:]{1})(\\.)(\\s{1})",replacement="\\1\\2\\4"))
  #instances where the entire sentence is a number e.g ver. 23.0
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\.)(\\s{1})([:digit:]+)(\\.)",replacement="\\2\\3\\4"))
  
  #remove final full stop
  matches = mutate(matches,
                   text_data_clean = str_remove_all(text_data_clean,pattern='\\.$'))
  
  #replace e.g. and i.e.
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern='e.g.\\s+|i\\.e\\.\\s+|\\s+ver\\.\\s+',replacement = ' '))
  #statcorp
  matches =mutate(matches,
                  text_data_clean = str_replace_all(text_data_clean,pattern="(\\.)(\\s{1})(college station tx)",replacement="\\2\\3"))
  
  # sas ibm corp
  matches =mutate(matches,
                  text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(ibm corp)(\\.)",replacement="\\1\\2"))
  
  #inc. or ver.
  matches =mutate(matches,
                  text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(inc|ver)(\\.)",replacement="\\1\\2"))
  
  #et al. or vs. or fig. [:lower:]{1}[:digit:]{1}
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(et al)(\\.)",replacement="\\1\\2"))
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(vs)(\\.)",replacement="\\1\\2"))
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(fig|figs)(\\.)",replacement="\\1\\2"))
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(no)(\\.)",replacement="\\1\\2"))
  
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern='([:digit:][:lower:])(\\.)',replacement='\\1'))
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern='([:lower:][:digit:])(\\.)',replacement='\\1'))
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern='([:lower:][:digit:])(\\.)',replacement='\\1'))
  
  #remove excess white space
  matches = mutate(matches,text_data_clean=textclean::replace_white(text_data_clean))
  
  # combine paragraphs from the same paper (with the same DOI)
  combined = group_by(matches, doi_num, topic_id, topic_num) %>%
    summarise(text = toString(text_data_clean),.groups='drop')
  
  combined = combined %>% arrange(topic_num)
  return(combined)
}

cleanup_anzctr_results<-function(...){
  # data management 
  matches = ungroup(matches) %>%
    mutate(topic_num = as.numeric(str_remove(topic_id, pattern='Topic '))) # topic number
  
  
  #tidy up a bit more to account for overuse of full stops (affects breakdown by sentence)
  #single number followed by full stop (e.g. [space]1.[space]); reomve number
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})([:digit:]{1})(\\.)(\\s{1})",replacement="\\3\\4"))
  
  #a single [a-z] character, predeeced and followed by a full stop[space]
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})([:alpha:]{1})(\\.)(\\s{1})",replacement="\\1\\2\\4"))
  
  #instances where the entire sentence is a number e.g ver. 23.0
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\.)(\\s{1})([:digit:]+)(\\.)",replacement="\\2\\3\\4"))
  
  #remove final full stop
  matches = mutate(matches,
                   text_data_clean = str_remove_all(text_data_clean,pattern='\\.$'))
  
  #replace e.g. and i.e.
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern='e.g.\\s+|i\\.e\\.\\s+|\\s+ver\\.\\s+',replacement = ' '))
  #statcorp
  matches =mutate(matches,
                  text_data_clean = str_replace_all(text_data_clean,pattern="(\\.)(\\s{1})(college station tx)",replacement="\\2\\3"))
  #inc. or ver.
  matches =mutate(matches,
                  text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(inc|ver)(\\.)",replacement="\\1\\2"))
  
  #et al. or '..'
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="(\\s{1})(et al|et. al)(\\.)",replacement="\\1\\2"))
  matches = mutate(matches,
                   text_data_clean = str_replace_all(text_data_clean,pattern="\\.+",replacement="."))
  
  combined = matches %>% rename(text=text_data_clean) %>%
    mutate(topic_num = str_remove_all(topic_id,'Topic '))
  
  return(combined)
}

library(assertthat) #for custom tokenize function
tokenize_words_ngrams<-function (string, lowercase = TRUE, n = 2) 
{
  assert_that(is.count(n), assertthat::is.string(string))
  words <- tokenize_words(string, lowercase = lowercase)
  if (n < length(words)) tokenize_ngrams(string, lowercase = lowercase,n=n)
  else words
  
}


calc_document_jaccard <- function(indata=combined,choose.topic,minhash,n.bands,dataset='plos',token='words_ngram'){
  
  if(dataset=='plos'){
  dat.topic = indata %>% filter(topic_num==choose.topic) %>% distinct(doi_num,.keep_all=T) %>%
    mutate(id=row_number())
  }
  if(dataset=='anzctr'){
    dat.topic = indata %>% filter(topic_num==choose.topic) %>% distinct(number,.keep_all=T) %>%
      mutate(id=row_number())    
  }
  if(token=='words'){
    text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                  tokenizer = tokenize_words,minhash_func = minhash,skip_short = F)
  }  
  if(token=='words_ngram'){
  text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                tokenizer = tokenize_words_ngrams,minhash_func = minhash,skip_short = F)
  }
  if(token=='bigram'){
    text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                  tokenizer = tokenize_ngrams,n=2,minhash_func = minhash,skip_short = T)
  }
  if(token=='trigram'){
    text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                  tokenizer = tokenize_ngrams,n=3,minhash_func = minhash,skip_short = T)
  }
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>% 
    arrange(desc(score)) 
  
  jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  
  return(list(similarities=jacsim,dat=dat.topic))
}


calculate_corpus_buckets <- function(indata=dat.sentences,choose.topic,grp.size=5000,minhash=minhash,n.bands=n.bands){
  topic_s.target = indata %>% filter(topic_num==choose.topic) %>% 
    unnest(c(text_data_clean_s,n_words)) %>%
    ungroup() %>% 
    select(doi_num,text_data_clean_s,n_words) %>% distinct(doi_num,text=text_data_clean_s,n_words=n_words) 
  
  #filter and summarise by distinct sentences
  topic_s.target = topic_s.target %>%
    group_by(text) %>%
    summarise(doi_num=list(doi_num)) %>% 
    ungroup() %>%
    filter(text!="") %>%
    mutate(id=row_number())
  
  text.corpus = TextReuseCorpus(text=topic_s.target$text,meta=list(id=topic_s.target$id),
                                tokenizer = tokenize_words,minhash_func = minhash,skip_short=F)
  
  #split into grps before calculating buckets
  corpus.grps = topic_s.target %>% select(id) %>% mutate(grps = as.numeric(cut_interval(id,length=grp.size)))
  n.grps = max(unique(corpus.grps$grps))
  buckets <- vector('list',n.grps)
  
  for (x in 1:n.grps){
    select.grp = filter(corpus.grps,grps==x) %>% pull(id)
    buckets[[x]] <- lsh(text.corpus[select.grp], bands = n.bands)
  }
  
  return(list(text.corpus=text.corpus,buckets=buckets))
  
}

calc_sentence_jaccard <- function(dat.topic,minhash,n.bands,cutoff=0.8){
  text.corpus = TextReuseCorpus(text=dat.topic$text,meta=list(id=dat.topic$id),
                                tokenizer = tokenize_words,minhash_func = minhash,skip_short = F)
  buckets <- lsh(text.corpus, bands = n.bands)
  candidates <- lsh_candidates(buckets)
  ### to_compare <- lsh_query(buckets,"[doc-id]") ## for top down appraoch. could copmute text.corpus once then at each iteration, remove already matched sentences; e.g. buckets %>% filter(!doc %in% 'doc-1')
  jacsim = lsh_compare(candidates, 
                       text.corpus, 
                       jaccard_similarity) %>%  
    filter(score>=cutoff)  
  jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  
  return(jacsim)
}

jaccard_all_batched = function(indata=dat.sentences,choose.topic,grp.size=5000,cutoff=0.8,minhash=minhash,n.bands=n.bands){
  topic_s.target = indata %>% filter(topic_num==choose.topic) %>% 
    unnest(c(text_data_clean_s,n_words)) %>%
    ungroup() %>% 
    select(doi_num,text_data_clean_s,n_words) %>% distinct(doi_num,text=text_data_clean_s,n_words=n_words) 
  
  #filter and summarise by distinct sentences
  topic_s.target = topic_s.target %>%
    group_by(text) %>%
    summarise(doi_num=list(doi_num)) %>% 
    ungroup() %>%
    filter(text!="") %>%
    mutate(id=row_number())
  
  text.corpus = TextReuseCorpus(text=topic_s.target$text,meta=list(id=topic_s.target$id),
                                tokenizer = tokenize_words_ngram,minhash_func = minhash,skip_short=T)
  
  #split tokenised corpus into chunks of length grp.size
  corpus.grps = topic_s.target %>% select(id) %>% mutate(grps = as.numeric(cut_interval(id,length=grp.size)))
  n.grps = max(unique(corpus.grps$grps))
  
  jacsim <- vector('list',n.grps)
  
  for (x in 1:n.grps){
    select.grp = filter(corpus.grps,grps==x) %>% pull(id)
    buckets <- lsh(text.corpus[select.grp], bands = n.bands)
    candidates <- lsh_candidates(buckets)
    if(!is.null(candidates)){
      jacsim[[x]] = lsh_compare(candidates, 
                                text.corpus, 
                                jaccard_similarity) %>% 
        filter(score>=cutoff)  
      jacsim[[x]] = jacsim[[x]] %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
    }
  }
  #bind_rows 
  jaccard_s = bind_rows(jacsim); rm(jacsim)
  dat = topic_s.target %>% filter(id %in% jaccard_s$a|id %in% jaccard_s$b)
  return(list(similarities=jaccard_s,dat=dat))
}


jaccard_targeted_search = function(indata=dat.sentences,choose.topic,search.term,minhash=minhash,n.bands=n.bands){
  topic_s = indata %>% filter(topic_num==choose.topic) %>% unnest(text_data_clean_s) %>% select(-text,-n_words) %>% ungroup() %>%
    mutate(id=row_number())
  topic_s.target = topic_s %>% filter(grepl(paste(search.term),text_data_clean_s)) %>% rename(text=text_data_clean_s)
  jaccard_s = calc_sentence_jaccard(indata=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  return(list(similarities=jaccard_s,dat=topic_s.target,keyword=search.term))
}


jaccard_sentence_all = function(indata=dat.sentences,choose.topic,minhash=minhash,n.bands=n.bands,dataset='plos'){
  
  if(dataset=='plos'){
    topic_s.target = indata %>% filter(topic_num==choose.topic) %>% 
      unnest(c(text_data_clean_s,n_words)) %>%
      ungroup() %>% 
      select(doi_num,text_data_clean_s,n_words) %>% distinct(doi_num,text=text_data_clean_s,n_words=n_words) 
    
    #filter and summarise by distinct sentences
    topic_s.target = topic_s.target %>%
      group_by(text) %>%
      summarise(doi_num=list(doi_num)) %>% 
      ungroup() %>%
      filter(text!="") %>%
      mutate(id=row_number())
  }
  if(dataset=='anzctr'){
    topic_s.target = indata %>% filter(topic_num==choose.topic) %>% 
      unnest(c(text_data_clean_s,n_words)) %>%
      ungroup() %>% 
      select(number,text_data_clean_s,n_words) %>% distinct(number,text=text_data_clean_s,n_words=n_words) 
    
    #filter and summarise by distinct sentences
    topic_s.target = topic_s.target %>%
      group_by(text) %>%
      summarise(number=list(number)) %>% 
      ungroup() %>%
      filter(text!="") %>%
      mutate(id=row_number())
  }
  
  jaccard_s = calc_sentence_jaccard(dat.topic=topic_s.target,minhash = minhash,n.bands = n.bands)
  jaccard_s = jaccard_s %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  dat = topic_s.target %>% filter(id %in% jaccard_s$a|id %in% jaccard_s$b)
  return(list(similarities=jaccard_s,dat=dat))
}




identify_boilerplate_text <- function(indata=dat.sentences.all,choose.topic=1,dataset='plos',search_str_1,search_str_2,cutoff=0.9){

  dat.target <- indata %>% filter(topic_num %in% choose.topic,grepl(search_str_1,text_data_clean_s))
  dat.target <- dat.target %>% mutate(id=row_number())
  if(dataset=='plos'){n_possible <- dat.target %>% distinct(doi_num) %>% nrow()}
  if(dataset=='anzctr'){n_possible <- dat.target %>% distinct(number) %>% nrow()}
  
  to_query <- dat.target %>% filter(text_data_clean_s==search_str_2) %>% slice(1) %>% pull(id)
  
  text.corpus = TextReuseCorpus(text=dat.target$text_data_clean_s,meta=list(id=dat.target$id),
                                tokenizer = tokenize_words,skip_short = F)

  candidates <- text.corpus[-to_query]
  out<-sapply(1:length(candidates),function(x) ratio_of_matches(a=candidates[[x]],b=text.corpus[[to_query]])) #i.e. how much b borrows from a; b embedded within a
  word_diff <- abs(sapply(1:length(candidates),function(x) str_count(candidates[[x]]$content,'\\w+')) - str_count(text.corpus[[to_query]]$content,'\\w+'))
  jacsim = tibble(a=paste0('doc-',to_query),b=names(candidates),score=out,word_diff=word_diff)

  jacsim = jacsim %>% mutate_at(c('a','b'),~as.numeric(str_remove_all(.,pattern='doc-')))
  boilerplate = jacsim %>% filter(score>=cutoff)
  dat = dat.target %>% filter(id %in% boilerplate$a|id %in% boilerplate$b)
  #n.boilerplate -> revised to total number of uniques studies, not pairs.
  n_boilerplate = filter(jacsim,score>=cutoff) %>% gather(variable,value,-c(score,word_diff)) %>% distinct(value) %>% nrow()
  summary.stat<- jacsim %>% summarise(med=median(score),q1=quantile(score,.25),q3=quantile(score,0.75),n.boilerplate = n_boilerplate)
  summary.stat <- summary.stat %>% add_column('example.text'=search_str_2,'n.possible'=n_possible,.before = 1)
  return(list(summary.stat=summary.stat,jacsim=jacsim,dat.target=dat.target))
}

