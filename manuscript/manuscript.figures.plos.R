library(dplyr)
library(tidyr)
library(pander)
library(ggplot2)
library(ggpubr)
library(flextable)
library(tidytext)
load('manuscript/total.records.plos.rda')
load('manuscript/wordcount.plos.rda')

to_plot = total.records[[2]] %>% select(-percentage) %>% 
  rename('Retrieved from search'=API) %>%
  gather(variable,value,-volume) %>%
  mutate(variable=factor(variable,levels=c('Retrieved from search','Included in analysis')))

g1 = ggplot(to_plot,aes(x=volume,y=value,fill=variable))+geom_bar(stat='identity',position='dodge',width=0.9,colour='black')+scale_y_continuous('Total records',breaks=seq(0,20000,2500))+
  scale_x_continuous('Volume',breaks=1:15)+scale_fill_grey(guide = guide_legend(reverse = TRUE) )+g.theme+theme(legend.title = element_blank())

g2 = ggplot(wordcounts,aes(y=log10(words),x=volume,group=volume))+geom_boxplot()+
  scale_x_continuous('Volume',breaks=1:14)+scale_y_continuous('Word count\n(log10 transformed)',breaks=seq(0,4,0.5)) + g.theme

load('manuscript/stats_section_subject.classifications.rda')
stats_section_subject = mutate(stats_section_subject,
                               top3 = ifelse(subject_level_number %in% 1:3,1,0))

to_plot = stats_section_subject %>% group_by(subject_level_name) %>% summarise(n=n(),Yes=sum(top3),No=sum(top3==0),.groups='drop')

to_plot.bar = to_plot %>% arrange(-n) %>% select(-n) %>% gather('Top 3 subject classification',value,-subject_level_name) %>%
  mutate(subject_level_name=factor(subject_level_name,levels=rev(unique(subject_level_name))),
         'Top 3 subject classification'=factor(`Top 3 subject classification`,levels = c('No','Yes')))


g3 = ggplot(to_plot.bar,aes(x=subject_level_name,y=value/10000,fill=`Top 3 subject classification`))+
  geom_bar(stat='identity',colour='black',width=0.75)+scale_y_continuous('Total records (x 10,000)',breaks=seq(0,12,1))+
  xlab('')+coord_flip()+scale_fill_manual(values = c('grey75','grey15'))+ theme_bw() + theme(legend.position = c(0.55,0.2),
                                                                                                                                                                                                                                                                                                                      legend.direction = 'horizontal')


jpeg('manuscript/asa_template/figures/figure1.jpg',width=480,height=600,quality=100)
ggarrange(g1,g2,g3,nrow=3,labels=LETTERS[1:3])
dev.off()
