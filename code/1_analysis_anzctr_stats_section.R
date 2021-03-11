# 1_analysis_anzctr_stats_section.R
# analysis of whether there's a stats section or not
# March 2021
library(dplyr)
library(INLA)
library(stringr)
library(inlatools)
library(flextable)

### section 1 ###
# get the data and process ready for the model
load('data/AnalysisReady_Stats_Section.RData') # 0_read_data_anzctr_stats_section.R

# count sections as null if they are virtually empty
null_stats = c('NA', 'N/A', '\n    ', 'Nil', 'Pending', 'NIl ', 'None', 'None.')
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
# check
filter(studies, words==10) %>% sample_n(3) %>%pull(stats_section)

# table of counts by year
tab = group_by(studies, year, missing) %>%
  tally() %>%
  group_by(year) %>%
  mutate(p = round(prop.table(n)*100)) %>%
  ungroup() %>%
  filter(missing==1)
tab

# it is clear that stats section was only available from 2013 onwards - confirmed on wayback machine that it was included then -- no earlier documentation
studies = filter(studies, 
                 year >= 2013)

### section 2 ###
## Bayesian regression model of word count with selected variables ##
count_model <- inla(words ~ study_type + date + n_funding + log2(samplesize_target), family='poisson', data=studies, 
                  control.compute=list(dic=TRUE))
summary(count_model)
# add residuals
studies = mutate(studies, 
            f= fitted(count_model),
            res = l_length - f)
hist(studies$res) # second mode in residuals is missing
boxplot(res ~ missing, data=studies)

## Bayesian logistic regression model with selected variables ##
missing_model <- inla(missing ~ study_type + date + n_funding + log2(samplesize_target), data=studies, family='binomial',
                     control.compute=list(dic=TRUE))
summary(missing_model)

# tabulate results
table = mutate(missing_model$summary.fixed,
               var = rownames(missing_model$summary.fixed) )%>%
        filter(var != '(Intercept)') %>%
          mutate(OR = exp(mean), # odds ratio
                 lower = exp(`0.025quant`),
                 upper = exp(`0.975quant`)) %>%
  select(var, OR, lower, upper)
rownames(table) = NULL
ftab = flextable(table) %>%
  autofit() %>%
  colformat_num(j=2:4, digits=2)
ftab   

## number of words
# median and inter-quartile range for non-missing sections
stats = filter(studies, missing==0) %>%
  summarise(median(words), q1=quantile(words, 0.25), q3=quantile(words, 0.75))
stats
