# FIG_excluded_anzctr.R
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

# missing vs non-missing stats sections
## count sections as null if they are virtually empty
null_stats = c('NA', 'N/A', '\n    ', 'Nil', 'NIl ', 'None', 'None.','Pending') 
studies = filter(studies, 
                 !str_detect(number, '^NCT')) %>% # none of these have stats section
  mutate( # binary outcome:
    stats_section = ifelse(stats_section %in% null_stats, NA, stats_section),
    missing = as.numeric(is.na(stats_section)),
    # length outcome (number of characters):
    length = nchar(stats_section),
    length = ifelse(missing==TRUE, 0, length),
    l_length = log(length+1), # log-transformed
    # number of words
    stats_section = str_replace_all(string=stats_section, pattern='  ', replacement = ' '),
    stats_section = str_replace_all(string=stats_section, pattern='\n', replacement = ' '),
    words = str_count(string=stats_section, pattern=' ') + 1,
    # time
    year = as.numeric(format(submitted, '%Y')),
    date = as.numeric(submitted - as.Date('2015-01-01')) / (1*365.25)) # standardised to one year


n.missing = filter(studies,missing==1) %>% nrow()
n.nonmissing = filter(studies,missing==0) %>% nrow()

## function to make diagram ANZCTR
make_diagram_anzctr = function(){
  par(mai=c(0,0,0,0))
  labels = c(paste('Downloaded\n n = ', format(N_start, big.mark = ','), sep=''),
             paste('Excluded: n = ',format(N_start-nrow(studies),big.mark = ','),
                   '\n- ', counts$reason[1], ', n = ', format(counts$n[1],big.mark=','),
                   '\n- ', 'clinicaltrials.gov', ', n = ', format(n1,big.mark = ','),
                   '\n- ', 'Pre 2013', ', n = ', format(n2 + counts$n[2],big.mark=','), sep=''), #includes initial pre-2005 from initial reading of data
             paste('Analysed: n =', format(nrow(studies), big.mark = ','),
                   '\n- Missing statistical methods section: n = ',format(n.missing,big.mark=','),
                   '\n- Non-missing statistical methods section: n = ',format(n.nonmissing,big.mark=','),sep=''))
  n_labels = length(labels)
  M = matrix(nrow=n_labels, ncol=n_labels)
  M[3,1] = "' '" 
  pos = matrix(data=c(0.29,0.8,
                      0.7,0.5,
                      0.29,0.2), ncol=2, byrow=TRUE)
  sizes=c(1.5,1.9,2.1) / 10
  props = c(0.4,0.35,0.3) # narrower for first and last
  plotmat(M, name=labels, pos=pos, box.type = 'rect', box.size=sizes, box.prop = props, curve = 0, arr.pos=0.85)
  shape::Arrows(x0=0.29, x1=0.5, y0=0.5, y1=0.5, arr.width=0.2, arr.length=0.22, arr.type='triangle')
}
# export
tiff('manuscript/plos_one/revised figures/S1_Fig_R1_anzctr.tif',width=10, height=8, units='in', res=300)
make_diagram_anzctr()
dev.off()

