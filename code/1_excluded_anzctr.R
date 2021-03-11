# 1_excluded_anzctr.R
# graph of excluded studies for ANZCTR (for appendix)
# March 2021
library(dplyr)
library(stringr)
library(diagram)

# get the data and process ready for the model
load('data/AnalysisReady_Stats_Section.RData') # 0_read_data_anzctr_stats_section.R

N_start = nrow(excluded) + nrow(studies)
counts = group_by(excluded, reason) %>%
  tally()

# exclude NCT, stats section all missing
n0 = nrow(studies)
studies = filter(studies, !str_detect(number, '^NCT'))
n1 = n0 - nrow(studies) # excluded number

# exclude studies prior to 2013
n0 = nrow(studies)
studies = mutate(studies,
                 year = as.numeric(format(submitted, '%Y'))) %>%
  filter(year >= 2013)
n2 = n0 - nrow(studies)

## function to make diagram ANZCTR
make_diagram_anzctr = function(){
  par(mai=c(0,0,0,0))
  labels = c(paste('Downloaded\n n = ', format(N_start, big.mark = ','), sep=''),
             paste('Excluded\n- ', counts$reason[1], ', n = ', counts$n[1],
                   '\n- ', counts$reason[2], ', n = ', counts$n[2],
                   '\n- ', 'clinicaltrials.gov', ', n = ', n1,
                   '\n- ', 'Pre 2013', ', n = ', n2, sep=''),
             paste('Analysed\n n =', format(nrow(studies), big.mark = ',')))
  n_labels = length(labels)
  M = matrix(nrow=n_labels, ncol=n_labels)
  M[3,1] = "' '" 
  pos = matrix(data=c(0.29,0.8,
                      0.7,0.5,
                      0.29,0.2), ncol=2, byrow=TRUE)
  sizes=c(1.5,2.3,1.5) / 10
  props = c(0.4,0.5,0.4) # narrower for first and last
  plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
  shape::Arrows(x0=0.29, x1=0.45, y0=0.5, y1=0.5, arr.width=0.2, arr.length=0.22, arr.type='triangle')
}
make_diagram_anzctr()
# export
jpeg('figures/excluded_anzctr_missing.jpg', width=5, height=4, units='in', res=300)
make_diagram_anzctr()
dev.off()

