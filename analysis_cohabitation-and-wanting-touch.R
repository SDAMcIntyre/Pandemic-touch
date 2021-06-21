library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(janitor)
source("reorder_ordinals.R")

pandemic.data <- read_csv("../Social+touch+in+a+pandemic_June+8,+2021_21.33_processed.csv") %>% 
  reorder_ordinals() %>% 
  mutate(tempID = sample(1:n(), n())) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', 'alone', 'with others'))

#### Cohabiting & wanting touch ####

quartz(11,7); plot(1:10)

pandemic.data %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Wanted Touch'))) %>% 
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
  select(starts_with(c('tempID','Number Cohabiting', 'Had Touch'))) %>% 
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

alone.data <- pandemic.data %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE),
         across(.cols = starts_with('Had Touch'),
                .fns = ~ as.numeric(.x))
  ) %>% 
  select(starts_with(c('Lives Alone', 'Had Touch'))) 

wilcox.test(`Had Touch Someone Close` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)
wilcox.test(`Had Touch Professional` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)
wilcox.test(`Had Touch Stranger` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)



#### Cohabiting & typical pre-covid week touch ####

quartz(11,7); plot(1:10)

pandemic.data %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Typical Touch'))) %>% 
  mutate(tempID = sample(1:n(), n())) %>% 
  pivot_longer(cols = starts_with('Typical Touch'),  names_prefix = 'Typical Touch ',
               names_to = 'Typical Touch From', values_to = 'Response') %>% 
  na.omit() %>%
  ggplot() +
  facet_grid(`Number Cohabiting` ~ `Typical Touch From`) +
  geom_bar(aes(x = Response, y = ..prop.., group = 1), stat = 'count') +
  theme_bw(base_size = 14) +
  labs(y = 'Proportion', x = NULL, title = 'Typical touch before Covid') +
  theme(axis.text.x=element_text(angle=45, hjust = 1)) 

alone.data <- pandemic.data %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE),
         across(.cols = starts_with('Typical Touch'),
                .fns = ~ as.numeric(.x))
  ) %>% 
  select(starts_with(c('Lives Alone', 'Typical Touch'))) 

wilcox.test(`Typical Touch Someone Close` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)
wilcox.test(`Typical Touch Professional` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)
wilcox.test(`Typical Touch Stranger` ~ `Lives Alone`, data = alone.data, conf.int = TRUE)

want.vs.had <- pandemic.data %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE),
         across(.cols = starts_with('Typical Touch'),
                .fns = ~ as.numeric(.x))
  ) %>% 
  select(starts_with(c('Lives Alone','Pets','Had Touch', 'Wanted Touch'))) %>% 
  mutate(diff_professional = as.numeric(`Wanted Touch Professional`) - as.numeric(`Had Touch Professional`),
         diff_close = as.numeric(`Wanted Touch Someone Close`) - as.numeric(`Had Touch Someone Close`),
         diff_cohabitant = as.numeric(`Wanted Touch Cohabitant`) - as.numeric(`Had Touch Cohabitant`),
         diff_stranger = as.numeric(`Wanted Touch Stranger`) - as.numeric(`Had Touch Stranger`),
         total_diff = diff_professional + diff_close + diff_stranger + diff_cohabitant,
         total_diff_comparable = diff_professional + diff_close + diff_stranger) #%>% 
  # group_by(`Lives Alone`,Pets) %>% 
  # summarise(diff_professional_summary = mean(diff_professional, na.rm=T),
  #           diff_close_summary = mean(diff_close, na.rm=T),
  #           diff_cohabitant_summary = mean(diff_cohabitant, na.rm=T),
  #           diff_stranger_summary = mean(diff_stranger, na.rm=T),
  #           total_diff_comparable_summary = mean(total_diff_comparable, na.rm=T),
  #           pos_close = sum(diff_close > 0),
  #           neg_close = sum(diff_close < 0),
  #           pos_stranger = sum(diff_stranger > 0),
  #           neg_stranger = sum(diff_stranger < 0),
  #           pos_professional = sum(diff_professional > 0),
  #           neg_professional = sum(diff_professional < 0))

summary(want.vs.had[want.vs.had$`Lives Alone`=='TRUE',])
summary(want.vs.had[want.vs.had$`Lives Alone`=='FALSE',])

  
want.vs.had %>% ggplot(aes(x=total_diff_comparable, fill=`Lives Alone`)) + 
  geom_histogram(alpha=0.5, bins=20)

plot(want.vs.had$diff_close, want.vs.had$diff_cohabitant)

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

