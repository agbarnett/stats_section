# 0_read_data_anzctr_stats_section.R
# read the ANZCTR data from XML files; takes a while to run
# handy document: https://www.anzctr.org.au/docs/ANZCTR%20Data%20field%20explanation.pdf
# data downloaded from https://www.anzctr.org.au/TrialSearch.aspx using empty search, then "download all to XML" button 
# search on 1-Feb-2021 returned 28,589 studies
# version for stats section
# March 2021
library(XML)
library(stringr)
library(dplyr)
source('99_functions.R')
censor.date = as.Date('2020-02-01') # date that these data were downloaded by me from ANZCTR

# institution data, from here https://www.grid.ac/ (not yet using)
inst = read.csv('data/grid-2019-10-06/grid.csv', stringsAsFactors = FALSE) %>%
  filter(Country == 'Australia') %>%
  mutate(Name = str_replace_all(Name, pattern='[^a-zA-Z0-9]', replace=' ')) # replace all non-numbers and letters
   
# list of files, one file per study
list.of.files = dir('data/anzdata', pattern='.xml')
N = length(list.of.files) # number of files

# big loop
studies  = ethics_data = funding_data = excluded = NULL
for (k in 1:N){ # loop through files
  data <- xmlParse(paste('data/anzdata/', list.of.files[k], sep=''))
  xml_data <- xmlToList(data)
  
  ## get study type
  study_type = ifelse(is.null(xml_data$trial_design$studytype), NA, xml_data$trial_design$studytype)
  if(is.na(study_type) == TRUE){
    eframe = data.frame(number=number, reason='Missing study type')
    excluded = bind_rows(excluded, eframe)
    next # skip to next study
  }

  ## trial number and dates
  number = xml_data$actrnumber
  if(is.null(number) == TRUE) { 
    number = xml_data$nctid # use secondary ID
  } 
  submitted = ifelse(is.null(xml_data$submitdate), NA, as.Date(xml_data$submitdate, '%d/%m/%Y'))
  approved = ifelse(is.null(xml_data$approvaldate), NA, as.Date(xml_data$approvaldate, '%d/%m/%Y'))
  update = ifelse(is.null(xml_data$dateLastUpdated), NA, as.Date(xml_data$dateLastUpdated, '%d/%m/%Y'))
  
  ## stats section (added 2021-03-09)
  stats_section = ifelse(is.null(xml_data$trial_design$statisticalmethods), NA, xml_data$trial_design$statisticalmethods)

  ## exclude small number of studies with missing date or retrospectively registered before database started
  if(is.na(submitted) == TRUE){
    eframe = data.frame(number=number, reason='Missing date')
    excluded = bind_rows(excluded, eframe)
    next # skip to next study
  }
  if(submitted < as.Date('2005-01-01')){
    eframe = data.frame(number=number, reason='Pre 2005')
    excluded = bind_rows(excluded, eframe)
    next # skip to next study
  }
  

  ## extract affiliations - but not used in analysis so far
  # get contact addresses
  contacts = unique(str_replace_all(string = unlist(unique(lapply(xml_data$contact, '[[', 3))), pattern='[^a-zA-Z0-9]', replace=' '))
  # just first address, could improve at later date
  contacts = contacts[1]
  contacts = str_replace_all(string = contacts, pattern='  ', replace=' ') # remove double spaces
  contacts = str_remove_all(string = contacts, pattern='^The ') # remove "The " to help combine results (e.g. uni of sydney)
  
  match = sapply(inst$Name, function (y) sapply(contacts, function (x) grepl(y, x)))
  id_number = NA
  if(sum(match) > 0){
    match = as.numeric(which(match)) # find matching number
    id_number = inst$ID[match] # get number from database
    contacts = inst$Name[match] # replace with nicer name if it is available
    if(length(contacts)>1){contacts = paste(contacts, collapse='; ')} # avoid duplicate rows
    if(length(id_number)>1){id_number = paste(id_number, collapse='; ')}
  } 
  # TO DO: could change to approximate match?
  
  ## variables on who is included in study
  # gender
  gender = ifelse(is.null(xml_data$eligibility$inclusivegender), NA, xml_data$eligibility$inclusivegender)
  # age, convert age to number
  if(is.null(xml_data$eligibility$inclusiveminagetype) == TRUE){
    age_min_type = NA
    age_min = NA
  }
  if(is.null(xml_data$eligibility$inclusiveminagetype) == FALSE){
    res_min = convert_age(type = xml_data$eligibility$inclusiveminagetype, number = xml_data$eligibility$inclusiveminage)
    age_min_type = res_min$type
    age_min = res_min$num
  }
  if(is.null(xml_data$eligibility$inclusivemaxagetype) == TRUE){
    age_max_type = NA
    age_max = NA
  }
  if(is.null(xml_data$eligibility$inclusivemaxagetype) == FALSE){
    res_max = convert_age(type = xml_data$eligibility$inclusivemaxagetype, number = xml_data$eligibility$inclusivemaxage)
    age_max_type = res_max$type
    age_max = res_max$num
  }
  volunteers = null_na(xml_data$eligibility$healthyvolunteer)
  
  ## funding (may by multiple), does not include sponsors
  n_funding = 0  # start with zero
  if(any(names(xml_data$sponsorship) == 'fundingsource') == TRUE){ # 
    funding = xml_data$sponsorship
    interim = sapply(funding, function(x) lapply(x, function(x) ifelse(is.null(x),NA,x))) # replace NULL with NA
    funding_frame <- data.frame(t(sapply(interim[names(interim) == 'fundingsource'], unlist)), 
                               row.names = NULL) 
    funding_frame = filter(funding_frame, !is.na(fundingname)) %>% # remove those with missing name
      mutate(number = number) %>% # add trial number
      select(-fundingaddress) # do not need these
    n_funding = nrow(funding_frame)
  }
  funding = ifelse(is.null(xml_data$sponsorship$fundingsource$fundingtype), NA, xml_data$sponsorship$fundingsource$fundingtype)
  if(n_funding==0){funding_frame = NULL}
  
  ## count the number of outcomes
  n_primary = sum(names(xml_data$outcomes)=="primaryOutcome")
  n_secondary = sum(names(xml_data$outcomes)=="secondaryOutcome")
  
  ## condition
  condition = ifelse(is.null(xml_data$conditions$healthcondition) == TRUE, NA, xml_data$conditions$healthcondition) # free text
  ccode1 = ifelse(is.null(xml_data$conditions$conditioncode$conditioncode1) == TRUE, NA, xml_data$conditions$conditioncode$conditioncode1)
  ccode2 = ifelse(is.null(xml_data$conditions$conditioncode$conditioncode2) == TRUE, NA, xml_data$conditions$conditioncode$conditioncode2)

  ## design
  allocation = ifelse(is.null(xml_data$trial_design$allocation), NA, xml_data$trial_design$allocation)
  purpose = ifelse(is.null(xml_data$trial_design$purpose), NA, xml_data$trial_design$purpose)
  masking = ifelse(is.null(xml_data$trial_design$masking), NA, xml_data$trial_design$masking)
  assignment = ifelse(is.null(xml_data$trial_design$assignment), NA, xml_data$trial_design$assignment)
  endpoint = ifelse(is.null(xml_data$trial_design$endpoint), NA, xml_data$trial_design$endpoint)
  phase = ifelse(is.null(xml_data$recruitment$phase), NA, xml_data$recruitment$phase)
  control = ifelse(is.null(xml_data$interventions$control), NA, xml_data$interventions$control)
  intervention_code = ifelse(is.null(xml_data$interventions$interventioncode), NA, xml_data$interventions$interventioncode)
  
  ## observational only design 
  purposeobs = ifelse(is.null(xml_data$trial_design$purposeobs), NA, xml_data$trial_design$purposeobs)
  duration = ifelse(is.null(xml_data$trial_design$duration), NA, xml_data$trial_design$duration)
  selection = ifelse(is.null(xml_data$trial_design$selection), NA, xml_data$trial_design$selection)
  timing = ifelse(is.null(xml_data$trial_design$timing), NA, xml_data$trial_design$timing)
  
  # sample size
  samplesize_target = ifelse(is.null(xml_data$recruitment$samplesize), NA, xml_data$recruitment$samplesize)
  samplesize_actual = ifelse(is.null(xml_data$recruitment$actualsamplesize), NA, xml_data$recruitment$actualsamplesize)
  
  ## dates 
  start_anticipated = ifelse(is.null(xml_data$recruitment$anticipatedstartdate), NA, xml_data$recruitment$anticipatedstartdate)
  end_date = ifelse(is.null(xml_data$recruitment$actualenddate), NA, xml_data$recruitment$actualenddate)
  start_anticipated = as.Date(start_anticipated, format = "%d/%m/%Y")
  end_date = as.Date(end_date, format = "%d/%m/%Y")
  
  ## study status and any publication (not used publication so far)
  study_status = ifelse(is.null(xml_data$recruitment$recruitmentstatus), NA, xml_data$recruitment$recruitmentstatus)
  pub = is.null(xml_data$ethicsAndSummary$publication) == FALSE
  pub = factor(as.numeric(pub), levels=0:1, labels=c('No','Yes'))
  
  ## ethics - potentially multiple
  ethics_review = ifelse(is.null(xml_data$ethicsAndSummary$ethicsreview), NA, xml_data$ethicsAndSummary$ethicsreview) # just one
  n_ethics_committees = 0  # start with zero
  if(any(names(xml_data$ethicsAndSummary) == 'ethicscommitee') == TRUE){ # note spelling error for committee
    ethics = xml_data$ethicsAndSummary
    interim = sapply(ethics, function(x) lapply(x, function(x) ifelse(is.null(x),NA,x))) # replace NULL with NA
    ethics_frame <- data.frame(t(sapply(interim[names(interim) == 'ethicscommitee'], unlist)), 
                     row.names = NULL) 
    ethics_frame = filter(ethics_frame, !is.na(ethicname)) %>% # remove those with missing review committee name
      mutate(ethicapprovaldate = as.Date(ethicapprovaldate, '%d/%m/%Y'), # convert dates
             ethicsubmitdate = as.Date(ethicsubmitdate, '%d/%m/%Y'),
             number = number) %>% # add trial number
      select(-ethicaddress, -hrec) # do not need these
    n_ethics_committees = nrow(ethics_frame)
  }
  if(n_ethics_committees==0){ethics_frame = NULL} # helps with bind_rows

  ## store data 
  # separate data frames for ethics and funding because need to collect multiple
  ethics_data = bind_rows(ethics_data, ethics_frame)
  funding_data = bind_rows(funding_data, funding_frame)
  frame = data.frame(id=id_number, 
                     study_type = study_type,
                     address=contacts, 
                     number=number, 
                     submitted =submitted, approved=approved, update=update,
                     condition = condition, ccode1=ccode1, ccode2=ccode2,
                     intervention_code = intervention_code, control=control,
                     n_primary = n_primary, n_secondary = n_secondary,
                     gender=gender, age_min_type=age_min_type, age_max_type=age_max_type,
                     age_min=age_min, age_max=age_max,
                     volunteers = volunteers,
                     purpose = purpose,
                     allocation = allocation, masking = masking, assignment=assignment, endpoint=endpoint,
                     phase=phase, 
                     purposeobs = purposeobs, duration = duration, selection = selection, timing = timing, # observational features
                     samplesize_target = samplesize_target, samplesize_actual = samplesize_actual,
                     start_anticipated=start_anticipated, end_date=end_date, 
                     study_status = study_status, pub=pub, 
                     n_ethics_committees = n_ethics_committees, ethics_review = ethics_review, # ethics questions that are not repeated over multiple HRECs
                     funding = funding[1], # first named funder
                     n_funding = n_funding, 
                     stats_section = stats_section,
                     stringsAsFactors = FALSE)
  studies = bind_rows(studies, frame)
  frame = res_max = res_min = match = id_number = contacts = funding = funding_frame = ethics = ethics_frame = n_ethics_committees = NULL # tidy up
} # end of loop

### Data edits ###

# general additions and fixes
studies = mutate(studies, 
                 ID = 1:n(), # simple ID per row
                 #provisional = str_detect(string=number, pattern='p$'), # provisional has a `p` at the end # dropped, not sure of value
                 #provisional = factor(as.numeric(provisional), levels=0:1, labels=c('No','Yes')),
                 gender = ifelse(gender=='Both males and females', 'All', gender), # to match clintrials style
                 submitted = as.Date(submitted, origin='1970-01-01'),
                 approved = as.Date(approved, origin='1970-01-01'),
                 update = as.Date(update, origin='1970-01-01'),
#                 year = as.numeric(format(start_anticipated, "%Y")), # no longer needed
                 time = as.numeric(censor.date - end_date), # calculate time to publication (not yet used)
                 samplesize_target = as.numeric(samplesize_target),
                 samplesize_actual = as.numeric(samplesize_actual),
# fix error in intervention code (capital "D")
intervention_code = ifelse(intervention_code=='Early Detection / Screening', 'Early detection / Screening', intervention_code),
                 # tidy up free-text condition
                 condition = tolower(condition), # to avoid distinct categories for same condition
                 condition = str_remove_all(string=condition, '[^a-z|0-9| ]'),  # remove all non letter characters, e.g., full stop, carraige return 
                 condition = str_remove_all(string=condition, '^ | $|  '), # remove starting or trailing space, or double space
                 condition = str_replace_all(string=condition, pattern = 'type1 |typei', replacement = 'type 1 '),
                 condition = str_replace_all(string=condition, pattern = 'type2 |type ii ', replacement = 'type 2 '),
        ccode1 = str_remove_all(string=ccode1, ' $') # remove trailing space
                 ) 

## data edits to remove silly maximum ages (can't be done using dplyr)
index = studies$age_max >= 120
index[is.na(index)] = FALSE
studies$age_max[index] = NA
studies$age_max_type[index] = 'No limit'

# check for duplicates, should be none
table(duplicated(studies$number))

### funding ###
## rename variables
# any mention of ARC or NHMRC
mention_arc = c('(ARC)','ARC Centre of Excellence','ARC-Linkage','ARC Linkage','ARC Dementia Research Development','Australia Research Council','Australian Research Council','ARC Discovery','^ARC$')
mention_arc = mention_arc[order(-nchar(mention_arc))] # long to short
mention_arc = paste(mention_arc, collapse='|')
mention_nhmrc = c('NATIONAL HEALTH AND MEDICAL RESEARCH COUNCIL','National Health and Medical Research Council','National Health & Medical Research Council','(NHMRC)','NHMRC')
mention_nhmrc = mention_nhmrc[order(-nchar(mention_nhmrc))] # long to short
mention_nhmrc = paste(mention_nhmrc, collapse='|')
mention_hrc = c('Health Research Council','Health Research Council of New Zealand') # TO DO
funding_data = rename(funding_data,
                      'type' = 'fundingtype',
                      'name' = 'fundingname',
                      'country' = 'fundingcountry')
funding_data = mutate(funding_data,
name = str_replace_all(string=name, '  |\\n', ' '), # double spaces and carriage returns
    str_remove_all(string=name, '^  | $'), # remove space at start or end
    str_remove_all(string=name, '^  | $'), # remove space at start or end (repeat)
    str_remove_all(string=name, '^The '), # remove "The " at the start to help combine results (e.g. uni of sydney)
    name = ifelse(name =='N/A', NA, name), # add missing
    name = str_replace_all(string=name, 'Auatralian|australian', 'Australian'), # typos
    name = ifelse(str_detect(string = name, pattern = mention_arc), 'ARC', name), # combine ARC
    name = ifelse(str_detect(string = name, pattern = mention_nhmrc), 'NHMRC', name)) # combine NHMRC

### ethics ###
ethics_data = rename(ethics_data,
                     'submitdate' = 'ethicsubmitdate',
                     'approvaldate' = 'ethicapprovaldate',
                     'name' = 'ethicname',
                     'country' = 'ethiccountry')
ethics_data=  mutate(ethics_data, 
    time = case_when(
    is.na(approvaldate) & is.na(submitdate) ~ -99,
    is.na(approvaldate) & !is.na(submitdate) ~ as.numeric(censor.date - submitdate),
    !is.na(approvaldate) & is.na(submitdate) ~ -99,
    !is.na(approvaldate) & !is.na(submitdate) ~ as.numeric(approvaldate - submitdate)))
ethics_data=  mutate(ethics_data, event = case_when(
      is.na(approvaldate) & is.na(submitdate) ~ -99,
      is.na(approvaldate) & !is.na(submitdate) ~ 0, # censored
      !is.na(approvaldate) & is.na(submitdate) ~ -99,
      !is.na(approvaldate) & !is.na(submitdate) ~ 1)) 

# save
save(studies, excluded, censor.date, funding_data, ethics_data, file='data/AnalysisReady_Stats_Section.RData')

