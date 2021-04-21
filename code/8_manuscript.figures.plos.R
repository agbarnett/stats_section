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

#FIGURE: summary of search results: number found, word count, subject classifications
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

g.theme = theme_bw()+theme(legend.position = 'top',legend.direction = 'horizontal',legend.text = element_text(size=10),
                           axis.text = element_text(size=12),axis.text.x = element_text(size=12),axis.text.y = element_text(size=12))

plot.records = total.records[[2]] %>% select(-percentage) %>% 
  rename('Retrieved from search'=API) %>%
  gather(variable,value,-volume) %>%
  mutate(variable=factor(variable,levels=c('Retrieved from search','Included in analysis')))

g1 = ggplot(plot.records,aes(x=volume,y=value,fill=variable))+geom_bar(stat='identity',position='dodge',width=0.9,colour='black')+scale_y_continuous('Total records',breaks=seq(0,20000,2500))+
  scale_x_continuous('Volume',breaks=1:15)+scale_fill_grey(guide = guide_legend(reverse = TRUE) )+theme(legend.title = element_blank(),legend.position = 'top',legend.direction = 'horizontal')

g2 = ggplot(wordcounts,aes(y=log10(words),x=volume,group=volume))+geom_boxplot()+
  scale_x_continuous('Volume',breaks=1:14)+scale_y_continuous('Word count\n(log10 transformed)',breaks=seq(0,4,0.5))

stats_section_subject = mutate(stats_section_subject,
                               top3 = ifelse(subject_level_number %in% 1:3,1,0))
plot.subjects = stats_section_subject %>% group_by(subject_level_name) %>% summarise(n=n(),Yes=sum(top3),No=sum(top3==0),.groups='drop')

plot.subjects = plot.subjects %>% arrange(-n) %>% select(-n) %>% gather('Top 3 subject classification',value,-subject_level_name) %>%
  mutate(subject_level_name=factor(subject_level_name,levels=rev(unique(subject_level_name))),
         'Top 3 subject classification'=factor(`Top 3 subject classification`,levels = c('No','Yes')))


g3 = ggplot(plot.subjects,aes(x=subject_level_name,y=value/10000,fill=`Top 3 subject classification`))+
  geom_bar(stat='identity',colour='black',width=0.75)+scale_y_continuous('Total records (x 10,000)',breaks=seq(0,12,1))+
  xlab('')+coord_flip()+scale_fill_manual(values = c('grey75','grey15'))+ theme(legend.position = 'bottom',legend.direction = 'horizontal')
#save
jpeg('../manuscript/asa_template/figures/plos.summary.jpg',width=480,height=600,quality=100)
ggarrange(g1,g2,g3,nrow=3,labels=LETTERS[1:3])
dev.off()

#FIGURE: PLOS 10 topics, summarise topic values
topic.values.plos = read.csv('../results/plos_one_10topics.csv',header=T)
#arrange by value within topic add rowid
topic.values.plos = topic.values.plos %>% arrange(topic_id,-value) %>%
  group_by(topic_id) %>% mutate(id=row_number(),
                                topic_label = factor(topic_id,levels = 1:10,labels=paste('Topic',1:10)),
                                value_scaled = value/max(value),
                                prop_records = id/max(id)) #scaled number of records between 0 and 1
  
#define cutoffs
cutoff = data.frame(x=c(100,500,1000),grp=c('Top 100','Top 500','Top 1000'))

g.plos.values = ggplot(topic.values.plos,aes(id,value,group=topic_label))+
  geom_line(size=1)+geom_vline(data=cutoff,aes(xintercept=x,colour=grp),linetype='dashed',size=1)+
  facet_wrap(~topic_label,scales='free')+g.theme +
  scale_x_continuous(name = "Total records",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~./max(.), name="Proportion of records in topic")
  ) +
  scale_y_continuous('Topic value') + theme(legend.title=element_blank())+
  scale_colour_manual(values=cbPalette[2:4])

jpeg('../manuscript/asa_template/figures/supplementary/plos.topicvalues.jpg',width=1000,height=800,quality=100)
g.plos.values
dev.off()

#FIGURE: ANZCTR 10 topics, summarise topic values
topic.values.anzctr = read.csv('../results/anzctr_10topics.csv',header=T)
#arrange by value within topic add rowid
topic.values.anzctr = topic.values.anzctr %>% arrange(topic_id,-value) %>%
  group_by(topic_id) %>% mutate(id=row_number(),
                                topic_label = factor(topic_id,levels = 1:10,labels=paste('Topic',1:10)),
                                value_scaled = value/max(value),
                                prop_records = id/max(id)) #scaled number of records between 0 and 1

g.anzctr.values = ggplot(topic.values.anzctr,aes(id,value,group=topic_label))+
  geom_line(size=1)+geom_vline(data=cutoff,aes(xintercept=x,colour=grp),linetype='dashed',size=1)+
  facet_wrap(~topic_label,scales='free')+g.theme +
  scale_x_continuous(name = "Total records",
                     # Add a second axis and specify its features
                     sec.axis = sec_axis( trans=~./max(.), name="Proportion of records in topic")
  ) +
  scale_y_continuous('Topic value') + theme(legend.title=element_blank())+
  scale_colour_manual(values=cbPalette[2:4])

jpeg('../manuscript/asa_template/figures/supplementary/anzctr.topicvalues.jpg',width=1000,height=800,quality=100)
g.anzctr.values
dev.off()
