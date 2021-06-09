library(readxl)
library(dplyr)
library(stringr)
library(summarytools)
library(readr)

data.file <- "Social+touch+in+a+pandemic_June+8,+2021_21.33.xlsx"

raw.data <- read_excel(data.file, col_names = FALSE, skip = 4, na = '-99') %>% 
  setNames( read_excel(data.file, col_names = FALSE, skip = 3, n_max = 1) %>% unlist ) 

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
  filter(Consent == 'yes') %>% # gave consent
  filter(Finished) %>% # completed the survey
  mutate(across(
    .cols = starts_with(c('Soc. Dist. Past Week','Would Do Activity')),
    .fns = ~ if_else( `User Language` == 'DA' &  `End Date` < '2020-11-09 12:15:00',
                      NA_real_, .x)
      )) %>%
  mutate(Age = format(`End Date`, format='%Y') %>% parse_number - `Year of Birth`) %>% 
  rename(
    ) %>% 
  # remove uninteresting variables
  select(-c(`Response Type`, Progress, Finished, `Recorded Date`, `Response ID`,
            `Distribution Channel`, Consent, `Year of Birth`)) %>% 
  dfSummary %>% view
