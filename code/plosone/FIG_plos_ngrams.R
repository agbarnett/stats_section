#8_plos_ngrams.R
library(tidyverse)
library(tidytext)
library(textreuse)
library(ggupset)

#general functions
source('code/99_boilerplate_functions.R')

#define settings for textreuse -> local senstivity hashing
n.minhash = 300
n.bands = 50
random.seed = TeachingDemos::char2seed('jaccard',set=F)
minhash <- minhash_generator(n = n.minhash, seed = random.seed)

g.theme = theme_bw(base_size=12)+theme(legend.position = 'top',legend.direction = 'horizontal',legend.text = element_text(size=10),
                                       strip.background = element_rect(fill='white'))

## data
stats_sections = readRDS('data/stats_section_cleaned.rds')
topic_results <- read.csv('results/plos_one_10topics.csv')

topic_results = topic_results %>% rename('doi'=DOI,'topic_num'=topic_id) %>%
  mutate(topic_id = paste('Topic',topic_num)) %>% select(doi,topic_id,value)

matches = left_join(stats_sections,topic_results,by='doi') 


#data management
combined <- cleanup_plos_results()

# split text_data_clean into sentence as alternative for boilerplate text (takes a while); in list form
dat.sentences = combined %>% mutate(text_data_clean_s = str_split(text,"(?<=\\.)\\s(?=[a-z])",simplify=F)) %>%
  mutate(n_words = lapply(text_data_clean_s,function(x) str_count(x,"\\w+")))

#tidy up sentence formatting
dat.sentences = mutate(dat.sentences,text_data_clean_s = lapply(text_data_clean_s,function(x) str_remove_all(x,pattern = '^[[:digit:]]\\.|\\.$')))
dat.sentences = mutate(dat.sentences,text_data_clean_s = lapply(text_data_clean_s,function(x) str_remove_all(x,pattern = '^-\\s?|\\.\\s?-\\s+')))

#split each sentence into individual words, before starting n-gram analysis by topic
dat.sentences = dat.sentences %>% mutate(word=lapply(text_data_clean_s,function(.) str_split(.,' ') %>% unlist()))

#n-gram analysis by topic, excluding stop words
dat.ngram = dat.sentences %>%
  unnest(word) %>%
  anti_join(.,tidytext::stop_words,by='word') %>%
  #filter(!grepl('[[:digit:]]+',word)) %>% #keep numbers in
  group_by(topic_num,doi_num) %>% summarise(text=str_c(word,collapse=' '),.groups='drop')

common_ngrams <- lapply(2:4,function(x)
  dat.ngram %>% unnest_tokens(ngram, text, token = "ngrams", n = x, n_min = x) %>%
    group_by(topic_num,ngram) %>% summarise(total_studies = length(unique(doi_num)),.groups='drop') %>%
    drop_na() %>% mutate(topic_num=as.numeric(topic_num)) %>% arrange(topic_num,-total_studies))
names(common_ngrams)<-paste('ngram',2:4,sep='_')


#review most common ngrams and merge with methods dictionary. use 'update' column
methods_hyphen <- openxlsx::read.xlsx(xlsxFile='data/methods_dictionary.xlsx',sheet='hyphen_terms')
methods_single <- openxlsx::read.xlsx(xlsxFile='data/methods_dictionary.xlsx',sheet='single_terms')
methods_models <- openxlsx::read.xlsx(xlsxFile='data/methods_dictionary.xlsx',sheet='models')
methods_list <- bind_rows(methods_hyphen,methods_single,methods_models) %>% select(update) %>%
  mutate(update = str_replace_all(update,pattern='-',replacement = ' '))

#bind_rows to combine all ngram results
common_ngrams.all <- bind_rows(common_ngrams)

#join ngram text with common stat methods to help with coding
common_methods_ngram <- inner_join(common_ngrams.all,methods_list,by=c('ngram'='update')) %>% distinct(ngram)
#write.xlsx(common_methods_ngram,file='data/common_ngrams_methods.xlsx')


#using the results of n-gram analysis, identify the number of documents, sentences that include keywords/phrases
#themes are: Statistical significance, presentation of descriptive statistics, parametric hypothesis tests, nonparameteric hypothesis tests, linear modelling, software
#filter on text in data.sentences first before working out which 

targeted_ngram_search <- function(search.str,indata=dat.sentences){
  result_str <- indata %>% filter(grepl(search.str,text))
  return(result_str)
}

results_ngram_search <- list()
#1. statistical significance'
stat.sig <- c('p(.*)less(.*)than','statistical(.*)significan(.*)','p(.*)value','two(.*)tailed','two(.*)sided','one(.*)tailed','one(.*)sided')
results_ngram_search[['Statistical significance']]<- targeted_ngram_search(search.str=paste0('\\b',str_c(stat.sig,collapse = '\\b|\\b'),'\\b'))

#2. descriptive statistics/data presentation'
descriptive<- c('data(.*)plus-or-minus','mean(.*)plus-or-minus','median','inter(.*)quartile','confidence(.*)interval')
results_ngram_search[['Descriptive statistics']]<- targeted_ngram_search(search.str=paste0('\\b',str_c(descriptive,collapse = '\\b|\\b'),'\\b'))

#3. parametric hypothesis tests
parametric <- c('t(.*)test','chi(.*)square(.*)test','f(.*)test','z(.*)test','f(.*)statistic','z(.*)statistic','t(.*)statistic','cochran(.*)armitage','dagostino(.*)pearson','hosmer(.*)lemeshow','cochrans(.*)c','cochran(.*)mantel(.*)haenszel')
results_ngram_search[['Parametric hypothesis tests']]<- targeted_ngram_search(search.str=paste0('\\b',str_c(parametric,collapse = '\\b|\\b'),'\\b'))

#4. nonparametric hypothesis tests
nonparametric <- c('non(.*)parametric','mann(.*)whitney','signed(.*)rank(.*)test','u(.*)test','wilcoxon(.*)test','fisher(.*)exact','kolmogorov(.*)smirnov','log(.*)rank(.*)test','kruskal(.*)wallis','shapiro(.*)wilk','spearman(.*)rank',
                   'mantel(.*)cox','gehan(.*)wilcoxon','anderson(.*)darling','mantel(.*)haenszel','jonckheere(.*)terpstra','cochrans(.*)q','wald(.*)wolfowitz')
results_ngram_search[['Nonparametric hypothesis tests']]<- targeted_ngram_search(search.str=paste0('\\b',str_c(nonparametric,collapse = '\\b|\\b'),'\\b'))

#5. linear models
## includes meta-analysis, post hoc tests
linear.model <- c('one(.*)way','two(.*)way','repeated(.*)measures','analysis(.*)of(.*)variance','anova','post(.*)hoc(.*)','tukey','bonferroni','dunnett',
                  'newman(.*)keuls','brown(.*)forsythe','log(.*)linear','greenhouse(.*)geisser','meta(.*)regression','dunn(.*)sidak','log(.*)logistic','kenward(.*)roger',
                  'fixed effect(.*)model','random effect(.*)model','linear(.*)model','meta(.*)analysis','regression','cox(.*)proportional(.*)hazard')
results_ngram_search[['Linear modelling/Regression']]<- targeted_ngram_search(search.str=paste0('\\b',str_c(linear.model,collapse = '\\b|\\b'),'\\b'))

#6. software
software <- c('sas','graphpad','spss','stata','excel','r','s(.*)plus','g(.*)power','medcalc')
results_ngram_search[['Software']]<- targeted_ngram_search(search.str=paste0('\\b',str_c(software,collapse = '\\b|\\b'),'\\b'))

#figure of results - for each targeted string, tally the % of articles within each topic that returned a match
total_hits_ngram <- lapply(results_ngram_search, function(x) 
  x %>% distinct(doi_num,topic_num) %>% count(topic_num,name='total_studies')) %>% bind_rows(.,.id='theme')

#join with total dois per topic to get percentages
total_hits_ngram <- left_join(total_hits_ngram,combined %>% count(topic_num),by='topic_num') %>% 
  mutate(percent_total = 100*total_studies/n,topic_label = factor(topic_num,levels=1:10,labels=paste('Topic',1:10)),
         theme = factor(theme,levels=unique(theme),labels=str_replace_all(unique(theme),pattern='_',replacement=' ')))



#upset plots to show most common combinations of text
themes_by_doi <- results_ngram_search %>% bind_rows(.,.id='theme')
#collapse themes by doi_num, topic_num
themes_by_doi_c <- themes_by_doi %>% group_by(topic_num,doi_num) %>% summarise(theme=list(theme),.groups='drop') 
studies_pertopic <- combined %>% count(topic_num)

#save plot by topic
# 
# for(choose.topic in 1:10){
#   ggplot(
#     data = filter(themes_by_doi_c %>% filter(topic_num==choose.topic)),
#     mapping = aes(x = theme)) +
#     geom_bar() +
#     scale_x_upset(
#       reverse = FALSE,
#       n_intersections = 10,
#       sets = names(results_ngram_search)) + #
#     labs(
#       title = paste('Topic',choose.topic,':', filter(studies_pertopic,topic_num==choose.topic) %>% pull(n) %>% format(.,big.mark = ','),'papers'),
#       subtitle = "Top 10 combinations of statistical methods text",
#       x = "Combination of n-gram themes",
#       y = "")
#   ggsave(paste0('plos_upset_themes_topic',choose.topic,'.png'),width=11,height=7)
# 
# }
