library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(svglite)
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
  ggplot(aes(x = Response)) +
  facet_grid(`Number Cohabiting` ~ `Wanted Touch From`, scales = 'free') +
  geom_histogram(stat = 'count') +
  theme_bw(base_size = 14) +
  labs(y = 'Count', x = NULL, title = 'Wanted touch in the past week') +
  theme(axis.text.x=element_text(angle=45, hjust = 1))

ggsave('Figures/cohabitation-and-wanting-touch.svg')
ggsave('Figures/cohabitation-and-wanting-touch.png')
