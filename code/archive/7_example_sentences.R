# 7_example_sentences.R
# example sentences from paper for PLOS and ANZCTR, was easier to cut and paste to here than use data
# sentences are cleaned as per data
# used by 7_jaccard_sentence.R
# May 2021
library(tidyverse)
library(textclean)
library(tm)
library(spelling)
load('data/unicode_characters.rda') # from nicole
load('data/stats_section_info.rda')

# ANZCTR
examples = read.table(header=TRUE, sep='&', text="
topic&example
3&Comparisons between categorical variables will be made either using chi square or Fisher exact test. Continuous data will be compared using the Student`s t-test or Mann-Whitney U test. Two sided p values of less than 0.05 will be considered statistically signficant.
3&The Mann-Whitney U, Student t, 1-way ANOVA, and Kruskal-Wallis tests will be used to compare continuous variables where relevant. The Fisher exact and Pearsons Chi-square test will be used to compare proportions as appropriate.
5&Pilot study
5&No formal sample size calculation was performed
7&Descriptive statistics
7&Descriptive statistics used
9&Linear mixed models will be used to analyse the data.
10&Repeated measures of ANOVA
10&Pre-, during, post- and follow-up variables will be subjected to mixed methods and repeated measures analyses to determine signficant changes over (group and) time.")

# PLOS ONE
examples_plos = read.table(header=TRUE, sep='&', text='
topic&example
1&Student’s t-test was used for statistical analysis. A p value of <0.05 was considered statistically significant.
1&Statistical analysis was performed using the Students t-test. P values of less than 0.05 were considered significant.
3&GraphPad Prism (Graphpad Software, San Diego, CA) was used for all analyses.
3&All statistical analysis was performed using Graphpad Prism software.
4&Significant differences were determined using analysis of variance (ANOVA) followed by Tukey post-hoc tests for multiple comparisons.
4&Data are expressed as the mean ± SEM. Statistical analysis was performed using one-way analysis of variance (ANOVA) test, followed by Dunnett’s multiple comparison post hoc test. P values less than 0.05 were considered significant.
5&SPSS software version 17.0 (SPSS, Chicago, IL, USA) was used for statistical analysis.
5&The results are presented as the mean ± SEM. Data were analyzed by one-way ANOVA and LSD tests using the SPSS 13.0 software (SPSS Inc, Chicago, IL, USA). The difference was considered statistically significant at P<0.05.
6&All results are expressed as means ± standard deviation (± SD).
6&Data are expressed as mean ± Standard error of the mean (SEM). Statistical analysis of the data was performed by the Student’s t-test. A value of P<0.05 was considered statistically significant.
9&Statistical significance was determined by Students t-tests. * P<0.05***, ** P = <0.01***, *** P<0.001***.
9&Data are presented as the mean ± SD. Statistical analysis was performed using the Students unpaired t-test. Differences were considered statistically significant at *p<0.05, **p<0.01 and ***p<0.001.')

# process, see 5_process_stats_section.R
#change to native encoding
examples = mutate(examples, example = enc2native(example))

#numbered references eg [23]
examples = examples %>% mutate(example = str_replace_all(example,pattern="\\[\\S{1,3}\\]",replacement = ""))

#remove () including text within brackets  "\\s*\\([^\\)]+\\)"
#option to keep text inside brackets is "[()]"
examples = examples %>% mutate(example = str_replace_all(example,"[()]",""))

#centered equations/other
examples = examples %>% mutate(example = str_replace_all(example,pattern="\\s*(\n)(.*)(\n)\\s*",replacement = " "))

#
#2. replace/standardise common symbols
#unicode
#format unicode characters (e.g. hair space <U+200A>)
#remove general punctuation unicodes U+20xxx (no dashes identified)
unicode_spaces = unicode_lookup %>% filter(grepl("U\\+20(\\w+)",unicode)) %>% pull(unicode) 
unicode_spaces = gsub("U\\+(\\w+)", "\\U\\\\+\\1",unicode_spaces) %>% str_c(.,collapse='|')

examples = examples %>% mutate(example = str_replace_all(example,pattern=unicode_spaces,replacement=" "))

#repalce unicode with plain text description (label_clean)
unicode_set = gsub("U\\+(\\w+)", "\\U\\\\+\\1",unicode_lookup[['unicode']])
unicode_set = str_c(unicode_set,collapse='|')

#add white space around unicode labels
unicode_to_text = function(input){
  out = unicode_lookup %>% filter(unicode==input) %>% pull(label_clean)
  paste0(' ',out,' ')
}

examples = examples %>% mutate(example = str_replace_all(example,
                                                                           unicode_set,
                                                                           unicode_to_text))

#standardise dashes 
examples = examples %>% mutate(example = str_replace_all(example,pattern="\\s*(–+)\\s*",replacement = "-"))

#standardise text and spacing for >,<,= 
examples = examples %>% mutate(example = str_replace_all(example,"\\s*[<]\\s*"," less-than "))
examples = examples %>% mutate(example = str_replace_all(example,"\\s*[>]\\s*"," greater-than "))
examples = examples %>% mutate(example = str_replace_all(example,"\\s*[=]\\s*"," equal-to "))
examples = examples %>% mutate(example = str_replace_all(example,"\\s*[<=]\\s*"," less-than-or-equal-to "))
examples = examples %>% mutate(example = str_replace_all(example,"\\s*[>=]\\s*"," greater-than-or-equal-to "))

examples = examples %>% mutate(example = str_replace_all(example,"\\bless than\\b","less-than"))
examples = examples %>% mutate(example = str_replace_all(example,"\\bequal to\\b","equal-to"))
examples = examples %>% mutate(example = str_replace_all(example,"\\bgreater than\\b","greater-than"))

#plus or minus 
examples = examples %>% mutate(example = str_replace_all(example,"\\s*(±)\\s*|\\s*(\\+/-+)\\s*"," plus-or-minus "))

#common symbols covered by text clean ($,%,#,@,&,w/)
examples = examples %>% mutate(example = replace_symbol(example))

#3. remove any remaining non-ascii characters, curly quotes
examples = examples %>% mutate(example = replace_non_ascii(example)) 
examples = examples %>% mutate(example = replace_curly_quote(example))

#remove punctuation except for '.','-'
examples$example = strip(examples$example,char.keep = c("~~",".","-"),apostrophe.remove=T,digit.remove=F)

save(examples, file='data/processed_examples_anzctr.RData')