library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
# library(patchwork)
library(svglite)
# library(knitr)
library(janitor)
source("reorder_ordinals.R")
source("plot_functions.R")

pandemic.data <- read_csv("./Social+touch+in+a+pandemic_June+8,+2021_21.33_processed.csv") %>% 
  reorder_ordinals()


#### Cohabiting & wanting touch ####

quartz(11,7); plot(1:10)

pandemic.data %>% 
  select(starts_with(c('Number Cohabiting', 'Wanted Touch'))) %>% 
  mutate(tempID = sample(1:n(), n())) %>% 
  pivot_longer(cols = starts_with('Wanted Touch'),  names_prefix = 'Wanted Touch ',
               names_to = 'Wanted Touch From', values_to = 'Response') %>% 
  na.omit() %>%
  ggplot() +
  facet_grid(`Number Cohabiting` ~ `Wanted Touch From`) +
  geom_bar(aes(x = Response, y = ..prop.., group = 1), stat = 'count') +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Wanted touch in the past week') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-wanting-touch.svg')
ggsave('Figures/cohabitation-and-wanting-touch.png')


alone.data <- pandemic.data %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE) ) %>% 
  select(starts_with(c('Lives Alone', 'Wanted Touch'))) 

# Pearson's chi-square test of equal distributions
chisq.test(alone.data$`Lives Alone`, alone.data$`Wanted Touch Someone Close`)
chisq.test(alone.data$`Lives Alone`, alone.data$`Wanted Touch Professional`)
chisq.test(alone.data$`Lives Alone`, alone.data$`Wanted Touch Stranger`)

# table with counts and percentages
alone.data %>% 
  tabyl(`Wanted Touch Someone Close`, `Lives Alone`, show_na = FALSE) %>%
  adorn_totals('row') %>% 
  adorn_percentages('col') %>% 
  adorn_pct_formatting() %>% 
  adorn_ns()

#### Cohabiting & had touch ####

quartz(11,7); plot(1:10)

pandemic.data %>% 
  select(starts_with(c('Number Cohabiting', 'Had Touch'))) %>% 
  mutate(tempID = sample(1:n(), n())) %>% 
  pivot_longer(cols = starts_with('Had Touch'),  names_prefix = 'Had Touch ',
               names_to = 'Had Touch From', values_to = 'Response') %>% 
  na.omit() %>%
  ggplot() +
  facet_grid(`Number Cohabiting` ~ `Had Touch From`) +
  geom_bar(aes(x = Response, y = ..prop.., group = 1), stat = 'count') +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Had touch in the past week') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-had-touch.svg')
ggsave('Figures/cohabitation-and-had-touch.png')


