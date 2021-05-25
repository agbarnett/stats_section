#7_summarise_stats_sections.R
library(tidyverse)
library(tidytext)
library(openxlsx)
dat = readRDS('data/stats_section_cleaned.rds')
load('data/plos_meta_data.rda')
#ten topics, PLOS ONE
matches = read.csv('results/plos_one_10topics.csv',header=T)
matches = left_join(dat,matches,by=c('doi'='DOI')) %>%
  mutate(value = as.numeric(value)) %>%
  distinct(doi,text_heading,.keep_all = T)

 tidy_matches = matches %>%
   unnest_tokens(word,text_data_clean)
 
wordcounts <- tidy_matches %>%
   group_by(topic_id,doi) %>%
   summarise(words = n(),.groups='drop') 
 
matches = left_join(matches,wordcounts,by=c('doi','topic_id')) %>%
  arrange(topic_id,-value) %>%
  group_by(topic_id) %>% mutate(rank=row_number()) %>% ungroup() %>%
  mutate(topic_id = paste('Topic',topic_id))

#add volume to wordcounts
wordcounts = wordcounts %>% left_join(select(meta_dat_allrecords,doi,volume),by='doi')

save(matches,file='manuscript/plos.results.10topics.rda')
save(wordcounts,file='manuscript/wordcount.plos.rda')

#example - topics 3 and 5
matches %>% filter(topic_id %in% c('Topic 1', 'Topic 3','Topic 5')) %>% ggplot(.,aes(x=rank,y=words)) + 
  geom_point(alpha=0.1)+ geom_smooth(method='loess',se=T,colour='blue')+ facet_wrap(~topic_id,scales = 'free') + 
  scale_x_continuous('Rank (1 = strongest topic match)')+scale_y_continuous('Word count (cleaned text)',breaks=seq(0,1500,100))+theme_minimal()


#example - topics 1 and 9
matches %>% filter(topic_id %in% c('Topic 1','Topic 9')) %>% ggplot(.,aes(x=rank,y=words)) + 
  geom_point(alpha=0.1)+ geom_smooth(method='loess',se=T,colour='blue')+ facet_wrap(~topic_id,scales='free') + 
  scale_x_continuous('Rank (1 = strongest topic match)')+scale_y_continuous('Word count (cleaned text)')+theme_minimal()


cos.sim <- function(ix,distances.mat) 
{
  A = distances.mat[,ix[1]]
  B = distances.mat[,ix[2]]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
} 

distances = tidy_matches %>% left_join(.,matches %>% select(doi,rank),by='doi') %>% filter(rank<=100)

calc_dist_mat <- function(indata=distances,topicNumber){
 to_stat = indata %>% filter(topic_id==topicNumber) %>%
    distinct(doi,word,.keep_all=F) %>%
    group_by(doi) %>% count(word) %>%
    spread(doi,n,fill=0) 
  
  distances.mat = as.matrix(to_stat[,-1])
  distances.doi = colnames(to_stat[,-1])
  
  #calc cos similarity
  n <- ncol(distances.mat) 
  cmb <- expand.grid(i=1:n, j=1:n) #%>% filter(i<j)

  cos.sim.vec <- apply(cmb,1,function(x) cos.sim(x,distances.mat))
  
  to_plot = cmb %>% add_column(sim=cos.sim.vec)
  
  return(list(to_plot=to_plot,doi.list=distances.doi))
}

#example with hierarchical clustering
test = calc_dist_mat(topicNumber=9)

cdistr = as.dist(1-matrix(test$to_plot[,3],100,100))
hcr <- hclust(cdistr, "ward.D")
cdistc = as.dist(1-t(matrix(test$to_plot[,3],100,100)))
hcc <- hclust(cdistc, "ward.D")
heatmap(1-matrix(test$to_plot[,3],100,100), Rowv=as.dendrogram(hcr), Colv=as.dendrogram(hcc))


clustering <- cutree(hcr, 10)

plot(hcr, main = "Hierarchical clustering of DOIS within topic",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hcr, 10, border = "red")

boilerplate.text = test$doi.list[clustering==2]
matches %>% filter(doi %in% boilerplate.text) %>% View()

stats_section.sim = lapply(1:10, function(tt)calc_dist_mat(topicNumber=tt))
save(stats_section.sim,file='manuscript/plos.cosinesim.10topics.rda')

#n-grams
tidy_matches = matches %>%
  unnest_tokens(word,text_data_clean,token='ngrams',n=2)

#compare words frequencies between topics
freq.counts = tidy_matches %>% group_by(topic_id) %>%
  count(word,sort=T) %>%
  mutate(freq=n/sum(n))

frequency <- freq.counts %>% 
  select(topic_id, word, freq) %>% 
  spread(topic_id, freq) 
ggplot(frequency,aes(`Topic 1`,`Topic 9`)) + 
  geom_jitter(alpha = 0.1) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5)+
  geom_abline(colour='red')

#split text_data_clean into sentence as alternative for boilerplate text

dat.sentences = lapply(1:nrow(matches), function(i) str_split(matches[i,]$text_data_clean,"\\. ",simplify=F) %>% unlist() %>% 
  tibble(text_data_clean_s=.) %>% add_column(doi = matches[i,] %>% pull(doi)))

matches.bysentence = dat.sentences %>% bind_rows() %>% left_join(.,matches %>% select(doi,topic_id,rank),by='doi')

#starting at rank=1 within each topic (e.g. topic 1 as starting point)
target_s = matches.bysentence %>% filter(topic_id=='Topic 3',rank==1) %>% pull(text_data_clean_s)
compare_s_all = matches.bysentence %>% filter(topic_id=='Topic 3',rank!=1) %>% 
  mutate(rowid = row_number())

compare_s = filter(compare_s_all,rank>1) 


calculate_jaccard_index = function(target_sentence,compare_sentences){
words_target = target_sentence %>% str_split(.,pattern=' ') %>% unlist()#test sentence

jaccard.index = matrix(0,nrow(compare_sentences),2)
for (x in 1:nrow(compare_sentences)){
words_compare =  compare_sentences[x,'text_data_clean_s'] %>% str_split(.,pattern=' ') %>% unlist()
intersection_ab = length(intersect(words_target,words_compare))
union_ab = length(words_target)+length(words_compare) - intersection_ab
jaccard.index[x,1] = intersection_ab/union_ab
words_abs.diff = abs(length(words_target)-length(words_compare))
jaccard.index[x,2] = words_abs.diff
}
colnames(jaccard.index) = c('jaccard.index','diff.words')
jaccard.index = data.frame(jaccard.index) %>% add_column(doi = compare_sentences[['doi']],rowid = compare_sentences[['rowid']],.before = 'jaccard.index')
return(jaccard.index)
}

test_jaccard = lapply(target_s,function(s) calculate_jaccard_index(target_sentence = s,compare_sentences = compare_s)) 
names(test_jaccard) = target_s

test_result = bind_rows(test_jaccard,.id='target_sentence') %>% filter(jaccard.index>=0.9,diff.words<=3)
test_result

#example sentence
test_str = 'student t-test was used for statistical analysis'
matches.bysentence %>% filter(topic_id=='Topic 1',grepl(test_str,text_data_clean_s))
