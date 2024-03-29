library(readxl)
library(dplyr)
library(stringr)
library(summarytools)
library(readr)
source("combine_variables.R")
source("reorder_ordinals.R")

data.file <- "../Social+touch+in+a+pandemic_June+8,+2021_21.33.csv"

raw.data <- read_csv(data.file, col_names = FALSE, skip = 3, na = c('','NA','-99')) %>% 
  setNames( read_csv(data.file, col_names = FALSE, n_max = 1) %>% unlist ) 

# get nicer column names from file
colnames.file <- './Social_touch_column_names_key.csv'
colnames.key <- read.csv2("./social_touch_column_names_key.csv", comment.char="#", header=FALSE)
colnames(colnames.key) <- c('orig', 'long','importid', 'nice')

# tally consent
raw.data %>% group_by(Q55) %>% tally()

# tally problem with Q11 and Q50
raw.data %>% 
  filter(UserLanguage == 'DA') %>% 
  mutate(beforeErrorFixed = EndDate < '2020-11-09 12:15:00') %>% 
  group_by(beforeErrorFixed) %>% tally()

# merge questions where people living alone vs not have seen different question
# NB: important to always have the non-alone question first
raw.data <- raw.data %>% 
  merge_questions_touch(c('Q65', 'Q20'), 3) %>% 
  merge_questions_touch(c('Q66', 'Q21'), 3) %>% 
  merge_questions_touch(c('Q67', 'Q22'), 3) %>% 
  merge_questions_worry(c('Q45', 'Q63')) 

# Fix country names
raw.data <- raw.data %>% 
  mutate(Q3 = coalesce(Q3, Q56, Q59, Q62, Q64)) %>% 
  # modify Finnish country names
  mutate(Q3 = ifelse(Q3=='Suomi', 'Finland', Q3)) %>% 
  mutate(Q3 = ifelse(Q3=='Saksa', 'Germany', Q3)) %>% 
  mutate(Q3 = ifelse(Q3=='Yhdysvallat', 'United States of America', Q3)) %>% 
  mutate(Q3 = ifelse(Q3=='Alankomaat', 'Netherlands', Q3)) %>%
  # modify Scandi languages
  mutate(Q3 = ifelse(Q3=='Norge', 'Norway', Q3)) %>%
  mutate(Q3 = ifelse(Q3=='Østerrike', 'Austria', Q3)) %>%
  mutate(Q3 = ifelse(Q3=='Sverige', 'Sweden', Q3)) %>%
  mutate(Q3 = ifelse(Q3=='Frankrike', 'France', Q3)) %>%
  mutate(Q3 = ifelse(Q3=='Danmark', 'Denmark', Q3)) %>% 
  select(-c(Q56, Q59, Q62, Q64))

#replace original column names with human readable ones
orig_colnames <- data.frame(orig = colnames(raw.data))
colnames_matched <- left_join(orig_colnames, colnames.key)
colnames(raw.data) <- colnames_matched$nice

# tally completed
raw.data %>% group_by(Finished) %>% tally()

# apply filters/changes to data  
processed.data <- raw.data %>% 
  filter(Consent == 'yes') %>% # gave consent
  filter(Finished) %>% # completed the survey
  mutate(
    
    # fix issue with early Danish survey
    across(
      .cols = starts_with(c('Soc. Dist. Past Week','Would Do Activity')),
      .fns = ~ if_else( `User Language` == 'DA' &  `End Date` < '2020-11-09 12:15:00',
                      NA_real_, .x)
      ),
    
    # calculate age
    Age = format(`End Date`, format='%Y') %>% parse_number - `Year of Birth`,
    
    # collapse n. cohabiting
    `Number Cohabiting` = if_else(
      `Number Cohabiting` == '4' | `Number Cohabiting` == '5' | `Number Cohabiting` == 'More than 5',
      'More than 3', `Number Cohabiting`)
    
    ) %>% 
  # remove uninteresting variables
  select(-c(`Response Type`, Progress, Finished, `Recorded Date`, 
            `Distribution Channel`, Consent, `Year of Birth`)) 


# summary table (html)
processed.data %>% 
  reorder_ordinals() %>% 
  select(-c(`Response ID`)) %>% 
  dfSummary %>% view

processed.data %>% 
  write_csv( str_replace(data.file, "\\.csv", "_processed\\.csv") )
