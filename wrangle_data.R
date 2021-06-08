library(readxl)
library(dplyr)
library(stringr)
library(summarytools)

data.file <- "/Users/sarmc72/OneDrive - Linköpings universitet/projects - in progress/pandemic touch/Data/Social+touch+in+a+pandemic_June+7,+2021_11.57.xlsx"

varIDs <- read_excel(data.file, col_names = FALSE, skip = 2, n_max = 1) %>% unlist %>% 
  str_replace_all('[,\\:]', '_') %>% str_replace_all('[\\{\\}\\"\\/]', '') %>% 
  str_replace('ImportId_','') 

raw.data <- read_excel(data.file, col_names = FALSE, skip = 3) %>% 
  setNames(varIDs) 

raw.data %>% group_by(QID55) %>% tally()

raw.data %>% filter(QID55 == 'yes') %>% 
  dfSummary %>% view
