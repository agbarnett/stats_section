library(dplyr)
library(tidyr)
library(pander)
library(ggplot2)
library(ggpubr)
library(flextable)
library(tidytext)
load('../results/total.records.plos.rda')
load('../results/wordcount.plos.rda')
load('../results/stats_section_subject.classifications.rda')

g.theme = theme_bw()+theme(legend.position = 'top',legend.direction = 'horizontal',
                           axis.text = element_text(size=12),axis.text.x = element_text(size=10),axis.text.y = element_text(size=10))

plot.records = total.records[[2]] %>% select(-percentage) %>% 
  rename('Retrieved from search'=API) %>%
  gather(variable,value,-volume) %>%
  mutate(variable=factor(variable,levels=c('Retrieved from search','Included in analysis')))

g1 = ggplot(plot.records,aes(x=volume,y=value,fill=variable))+geom_bar(stat='identity',position='dodge',width=0.9,colour='black')+scale_y_continuous('Total records',breaks=seq(0,20000,2500))+
  scale_x_continuous('Volume',breaks=1:15)+scale_fill_grey(guide = guide_legend(reverse = TRUE) )+g.theme+theme(legend.title = element_blank())

g2 = ggplot(wordcounts,aes(y=log10(words),x=volume,group=volume))+geom_boxplot()+
  scale_x_continuous('Volume',breaks=1:14)+scale_y_continuous('Word count\n(log10 transformed)',breaks=seq(0,4,0.5)) + g.theme


stats_section_subject = mutate(stats_section_subject,
                               top3 = ifelse(subject_level_number %in% 1:3,1,0))

plot.subjects = stats_section_subject %>% group_by(subject_level_name) %>% summarise(n=n(),Yes=sum(top3),No=sum(top3==0),.groups='drop')

plot.subjects = plot.subjects %>% arrange(-n) %>% select(-n) %>% gather('Top 3 subject classification',value,-subject_level_name) %>%
  mutate(subject_level_name=factor(subject_level_name,levels=rev(unique(subject_level_name))),
         'Top 3 subject classification'=factor(`Top 3 subject classification`,levels = c('No','Yes')))


g3 = ggplot(plot.subjects,aes(x=subject_level_name,y=value/10000,fill=`Top 3 subject classification`))+
  geom_bar(stat='identity',colour='black',width=0.75)+scale_y_continuous('Total records (x 10,000)',breaks=seq(0,12,1))+
  xlab('')+coord_flip()+scale_fill_manual(values = c('grey75','grey15'))+ g.theme + theme(legend.position = c(0.55,0.2),
                                                                                                                                                                                                                                                                                                                      legend.direction = 'horizontal')


jpeg('../manuscript/asa_template/figures/plos.summary.jpg',width=480,height=600,quality=100)
ggarrange(g1,g2,g3,nrow=3,labels=LETTERS[1:3])
dev.off()
