library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(janitor)
source("reorder_ordinals.R")

pandemic.data <- read_csv("./Social+touch+in+a+pandemic_June+8,+2021_21.33_processed.csv") %>% 
  reorder_ordinals() %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', 'alone', 'with others'))


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

# ggsave('Figures/cohabitation-and-wanting-touch.svg')
# ggsave('Figures/cohabitation-and-wanting-touch.png')


kruskal.test(`Wanted Touch Someone Close` ~ `Number Cohabiting`, data = pandemic.data)
kruskal.test(`Wanted Touch Professional` ~ `Number Cohabiting`, data = pandemic.data)
kruskal.test(`Wanted Touch Stranger` ~ `Number Cohabiting`, data = pandemic.data)

alone.data <- pandemic.data %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(across(.cols = starts_with('Wanted Touch'),
                .fns = ~ as.numeric(.x))
         ) %>% 
  select(starts_with(c('Lives Alone', 'Wanted Touch'))) 

wilcox.test(`Wanted Touch Someone Close` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)
wilcox.test(`Wanted Touch Professional` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)
wilcox.test(`Wanted Touch Stranger` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)

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

# ggsave('Figures/cohabitation-and-had-touch.svg')
# ggsave('Figures/cohabitation-and-had-touch.png')


#### Cohabiting and wanting video touch ####

video.data <- pandemic.data %>% 
  select(starts_with(c('Response ID','Number Cohabiting', 'Lives Alone', 'Touch')), -c('Touch Order')) %>% 
  pivot_longer(cols = starts_with('Touch'),
               names_to = c('Touch Message', 'Cohabiting', 'Touched by'),
               names_pattern = 'Touch (.*) (.*) (.*)', 
               values_to = 'Response') %>% 
  na.omit() %>% 
  mutate(`Touched by` = factor(`Touched by`, levels = c('Partner',
                                                        'Child',
                                                        'Friend', 
                                                        'Family',
                                                        'Acquaintance',
                                                        'Stranger')),
         `Would like` = case_when(Response == 'I would dislike it' ~ -1L,
                                  Response == 'I don\'t mind' ~ 0L,
                                  Response == 'I would like it' ~ 1L,
                                  Response == 'I don\'t know / not applicable' ~ NA_integer_),
         Cohabiting = case_when(Cohabiting == 'Cohab' ~ 'cohabitant',
                                Cohabiting == 'Non-Cohab' ~ 'non-cohabitant')
  )

quartz(15,10); plot(1:10)
video.data %>%
  filter(`Touch Message` == 'Attention') %>% 
  ggplot(aes(fill = Cohabiting)) +
  facet_grid(`Number Cohabiting` ~ `Touched by`) +
  geom_bar(aes(x = Response, y = ..prop.., group = Cohabiting), 
           stat = 'count', position = position_dodge()) +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Response to \"Attention\" touch video') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-video-touch-Attention.svg')
ggsave('Figures/cohabitation-and-video-touch-Attention.png')

quartz(15,10); plot(1:10)
video.data %>%
  filter(`Touch Message` == 'Love') %>% 
  ggplot(aes(fill = Cohabiting)) +
  facet_grid(`Number Cohabiting` ~ `Touched by`) +
  geom_bar(aes(x = Response, y = ..prop.., group = Cohabiting), 
           stat = 'count', position = position_dodge()) +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Response to \"Love\" touch video') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-video-touch-Love.svg')
ggsave('Figures/cohabitation-and-video-touch-Love.png')

quartz(15,10); plot(1:10)
video.data %>%
  filter(`Touch Message` == 'Happiness') %>% 
  ggplot(aes(fill = Cohabiting)) +
  facet_grid(`Number Cohabiting` ~ `Touched by`) +
  geom_bar(aes(x = Response, y = ..prop.., group = Cohabiting), 
           stat = 'count', position = position_dodge()) +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Response to \"Happiness\" touch video') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-video-touch-Happiness.svg')
ggsave('Figures/cohabitation-and-video-touch-Happiness.png')

quartz(15,10); plot(1:10)
video.data %>%
  filter(`Touch Message` == 'Calming') %>% 
  ggplot(aes(fill = Cohabiting)) +
  facet_grid(`Number Cohabiting` ~ `Touched by`) +
  geom_bar(aes(x = Response, y = ..prop.., group = Cohabiting), 
           stat = 'count', position = position_dodge()) +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Response to \"Calming\" touch video') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-video-touch-Calming.svg')
ggsave('Figures/cohabitation-and-video-touch-Calming.png')


quartz(15,10); plot(1:10)
video.data %>%
  filter(`Touch Message` == 'Sadness') %>% 
  ggplot(aes(fill = Cohabiting)) +
  facet_grid(`Number Cohabiting` ~ `Touched by`) +
  geom_bar(aes(x = Response, y = ..prop.., group = Cohabiting), 
           stat = 'count', position = position_dodge()) +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Response to \"Sadness\" touch video') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-video-touch-Sadness.svg')
ggsave('Figures/cohabitation-and-video-touch-Sadness.png')


quartz(15,10); plot(1:10)
video.data %>%
  filter(`Touch Message` == 'Gratitude') %>% 
  ggplot(aes(fill = Cohabiting)) +
  facet_grid(`Number Cohabiting` ~ `Touched by`) +
  geom_bar(aes(x = Response, y = ..prop.., group = Cohabiting), 
           stat = 'count', position = position_dodge()) +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Response to \"Gratitude\" touch video') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

ggsave('Figures/cohabitation-and-video-touch-Gratitude.svg')
ggsave('Figures/cohabitation-and-video-touch-Gratitude.png')


##### don't know / not applicable #####

video.data %>% 
  mutate(DKNA = if_else(Response == 'I don\'t know / not applicable', TRUE, FALSE)) %>% 
  group_by(`Touch Message`, `Touched by`, Cohabiting, `Lives Alone`, DKNA) %>% 
  summarise(n = n()) %>% 
  group_by(`Touch Message`, `Touched by`, Cohabiting, `Lives Alone`) %>% 
  mutate(Total = sum(n)) %>% 
  filter(DKNA) %>% 
  mutate(`Proportion don't know / not applicable` = n/Total) %>% 
  ungroup() %>% 
  ggplot(aes(x = `Touched by`, y = `Proportion don't know / not applicable`, 
             fill = `Lives Alone`, alpha = Cohabiting)) +
  facet_wrap( ~ `Touch Message`) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  scale_alpha_discrete(range = c(0.5,1)) +
  theme_bw(base_size = 14) +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) +
  labs(title = 'Don\'t know / not applicable response rates')

ggsave('Figures/cohabitation-and-video-touch-DKNA.svg')
ggsave('Figures/cohabitation-and-video-touch-DKNA.png')

##### re-coded video wanting variable #####

quartz(15,10); plot(1:10)

video.data %>% 
  ggplot(aes(x = `Touched by`, y = `Would like`, colour = `Lives Alone`, linetype = Cohabiting)) +
  facet_wrap(~ `Touch Message`) +
  stat_summary(geom = 'point', fun = 'mean', position = position_dodge(0.3)) +
  stat_summary(geom = 'errorbar', fun.data = 'mean_cl_boot', 
               width = 0.3, position = position_dodge(0.3)) +
  theme_bw(base_size = 14) +
  theme(axis.text.x=element_text(angle=45, hjust = 1),
        legend.position = 'bottom') +
  labs(title = 'Response to touch videos, by \"message\" being communicated.',
       colour = 'Respondent \nlives...', linetype = 'Imagined \ntoucher is a...')

ggsave('Figures/cohabitation-and-video-touch-All.svg')
ggsave('Figures/cohabitation-and-video-touch-All.png')
