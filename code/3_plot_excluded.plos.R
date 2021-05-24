#3_plot_excluded
# CONSORT style diagram of included/excluded studies. PLOS One case study
# May 2021
library(dplyr)
library(ggplot2)
library(diagram)

#load metadata
load('results/total.records.plos.rda')
#number downloaded through target search queries
n.api = total.records[[1]] %>% filter(stage=='API') %>% pull(n) %>% sum()
#number of stats sections included following partial matching
n.include = total.records[[1]] %>% filter(stage=='Included in analysis') %>% pull(n) %>% sum()

count_exclude = tibble(reason=c('Did not pass second\nstage filter'),n=c(n.api -  n.include))


## a) function to make diagram ANZCTR
make_diagram_plos = function(){
  par(mai=c(0,0,0,0))
  labels = c(paste('Downloaded\n n = ', format(n.api, big.mark = ','), sep=''),
             paste('Excluded\n- ', count_exclude$reason[1], ', n = ', format(count_exclude$n[1],big.mark=',')),
             paste('Analysed\n n =', format(n.include, big.mark = ',')))
  n_labels = length(labels)
  M = matrix(nrow=n_labels, ncol=n_labels)
  M[3,1] = "' '" 
  pos = matrix(data=c(0.29,0.8,
                      0.7,0.55,
                      0.29,0.3), ncol=2, byrow=TRUE)
  sizes=c(1.5,2.3,1.5) / 10
  props = c(0.5,0.5,0.5) # narrower for first and last
  plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
  shape::Arrows(x0=0.29, x1=0.45, y0=0.55, y1=0.55, arr.width=0.2, arr.length=0.22, arr.type='triangle')
  # heading
  text(0.5, 0.95, "PLOS ONE", font=2)
}

# export
jpeg('figures/excluded_plosone.jpg', width=5, height=4, units='in', res=300)
make_diagram_plos()
dev.off()
