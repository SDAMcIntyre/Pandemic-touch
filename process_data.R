library(readxl)
library(dplyr)
library(stringr)
library(summarytools)

data.file <- "Social+touch+in+a+pandemic_June+8,+2021_21.33.xlsx"

raw.data <- read_excel(data.file, col_names = FALSE, skip = 3, na = '-99') %>% 
  setNames( read_excel(data.file, col_names = FALSE, n_max = 1) %>% unlist ) 

# consent
raw.data %>% group_by(Q55) %>% tally()

# completed
raw.data %>% group_by(Finished) %>% tally()

# problem with Q11 and Q50
raw.data %>% 
  filter(UserLanguage == 'DA') %>% 
  mutate(beforeErrorFixed = EndDate < '2020-11-09 12:15:00') %>% 
  group_by(beforeErrorFixed) %>% tally()

# apply filters/changes to data
raw.data %>% 
  filter(Q55 == 'yes') %>% # gave consent
  filter(Finished) %>% # completed the survey
  mutate(across(
    .cols = starts_with(c('Q11','Q50')),
    .fns = ~ if_else( UserLanguage == 'DA' &  EndDate < '2020-11-09 12:15:00',
                      NA_real_, .x)
      )) %>%
  dfSummary %>% view
