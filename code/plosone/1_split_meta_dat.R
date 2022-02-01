#split meta_dat
#splits meta_dat into equal sized datasets to help with computation in batches
library(tidyverse)
load('data/plos_meta_data.rda')
num_groups = 5
meta_dat = meta_dat_allrecords %>% mutate(bin = (row_number()-1) %/% (n()/num_groups)) 

meta_dat_list = split(meta_dat,meta_dat$bin)

#store as batches and save
meta_dat_1 = meta_dat_list[[1]]
meta_dat_2 = meta_dat_list[[2]]
meta_dat_3 = meta_dat_list[[3]]
meta_dat_4 = meta_dat_list[[4]]
meta_dat_5 = meta_dat_list[[5]]

saveRDS(meta_dat_1,file='data/plos_meta_1.RDS')
saveRDS(meta_dat_2,file='data/plos_meta_2.RDS')
saveRDS(meta_dat_3,file='data/plos_meta_3.RDS')
saveRDS(meta_dat_4,file='data/plos_meta_4.RDS')
saveRDS(meta_dat_5,file='data/plos_meta_5.RDS')