library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(janitor)
source("reorder_ordinals.R")

pandemic.data <- read_csv("Social+touch+in+a+pandemic_June+8,+2021_21.33_processed.csv") %>% 
  reorder_ordinals() %>% 
  mutate(tempID = sample(1:n(), n()))

###
# plot stacked bar
###

# plot appearance ####
custom_colors <- c('#C0C0C0', scales::brewer_pal(type='div', palette='RdPu')(5))

theme_barchart_no_x <- theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(hjust=0.5, vjust=0.7, size=20,
                                   margin = margin(t = -30, r = 00, b = 0, l = 0)))

theme_barchart_x45 <- theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.x = element_text(vjust=0,
                                    margin = margin(t = 25, r = 0, b = 0, l = 0)),
        plot.margin = margin(0, 0, 2, 0, "cm"),
        axis.text.x = element_text(angle=45, hjust=1, vjust=0.7,
                                   margin = margin(t = -30, r = 00, b = 0, l = 0)))

theme_barchart_x60 <- theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        strip.text.x = element_text(size=20),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=60, hjust=1, vjust=0.7,
                                   margin = margin(t = -30, r = 00, b = 0, l = 0)))
# wanted ####

bar.chart.data.want <- pandemic.data %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Wanted Touch'))) %>%  #c('Wanted Touch', 'Had Touch', 'Typical Touch')
  pivot_longer(cols = starts_with('Wanted Touch'),  names_prefix = 'Wanted Touch ',
               names_to = 'Wanted Touch From', values_to = 'Response') %>% 
  mutate(`Wanted Touch From` = factor(`Wanted Touch From`, levels=c('Cohabitant', 'Someone Close', 'Professional','Stranger'))) %>% 
  na.omit() %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE))

bar.chart.data.want %>% 
  ggplot(aes(x=`Wanted Touch From`, fill=Response)) + 
  geom_bar(position='fill') + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'How much touch would you \nhave liked to have received \nin the past week?') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  scale_x_discrete(position = 'top') +
  theme_barchart_no_x

ggsave('Figures/wanting_touch_stacked_bar.png')

bar.chart.data.want %>% 
  mutate(`Lives Alone` = factor(`Lives Alone`, levels=c(TRUE, FALSE), labels = c('Alone', 'With others'))) %>% 
  ggplot(aes(x=`Lives Alone`, fill=Response)) + 
  geom_bar(position='fill', width = 0.9) + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'How much touch would you \nhave liked to have received \nin the past week?') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  xlab('Living situation') +
  facet_grid(~`Wanted Touch From`) + 
  #ggtitle('Wanted touch in the past week') +
  theme_barchart_x45

ggsave('Figures/wanting_touch_stacked_bar_by_living_status.png')

# had ####

bar.chart.data.had <- pandemic.data %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Had Touch'))) %>%  #c('Wanted Touch', 'Had Touch', 'Typical Touch')
  pivot_longer(cols = starts_with('Had Touch'),  names_prefix = 'Had Touch ',
               names_to = 'Had Touch From', values_to = 'Response') %>% 
  mutate(`Had Touch From` = factor(`Had Touch From`, levels=c('Cohabitant', 'Someone Close', 'Professional','Stranger'))) %>% 
  na.omit() %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE))

bar.chart.data.had %>% 
  ggplot(aes(x=`Had Touch From`, fill=Response)) + 
  geom_bar(position='fill') + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'How much touch have you \nhad in the past week? ') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  scale_x_discrete(position = 'top') +
  theme_barchart_no_x

ggsave('Figures/had_touch_stacked_bar.png')

bar.chart.data.had %>% 
  mutate(`Lives Alone` = factor(`Lives Alone`, levels=c(TRUE, FALSE), labels = c('Alone', 'With others'))) %>% 
  ggplot(aes(x=`Lives Alone`, fill=Response)) + 
  geom_bar(position='fill', width = 0.9) + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'How much touch have you \nhad in the past week? ') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  facet_grid(~`Had Touch From`) + 
  theme_barchart_x60

ggsave('Figures/had_touch_stacked_bar_by_living_status.png')

# typical touch ####

bar.chart.data.typical <- pandemic.data %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Typical Touch'))) %>%  #c('Wanted Touch', 'Had Touch', 'Typical Touch')
  pivot_longer(cols = starts_with('Typical Touch'),  names_prefix = 'Typical Touch ',
               names_to = 'Typical Touch From', values_to = 'Response') %>% 
  mutate(`Typical Touch From` = factor(`Typical Touch From`, levels=c('Cohabitant', 'Someone Close', 'Professional','Stranger'))) %>% 
  na.omit() %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE))

#On a typical week prior to the current pandemic, how much touch would you have received?

bar.chart.data.typical %>% 
  ggplot(aes(x=`Typical Touch From`, fill=Response)) + 
  geom_bar(position='fill') + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'On a typical week prior to \nthe current pandemic, how much \ntouch would you have received?') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  scale_x_discrete(position = 'top') +
  theme_barchart_no_x

ggsave('Figures/typical_touch_stacked_bar.png')

bar.chart.data.typical%>% 
  mutate(`Lives Alone` = factor(`Lives Alone`, levels=c(TRUE, FALSE), labels = c('Alone', 'With others'))) %>% 
  ggplot(aes(x=`Lives Alone`, fill=Response)) + 
  geom_bar(position='fill', width = 0.9) + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'On a typical week prior to \nthe current pandemic, how much \ntouch would you have received?') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  facet_grid(~`Typical Touch From`) + 
  theme_barchart_x60

ggsave('Figures/had_touch_stacked_bar_by_living_status.png')

comparison_data <- bar.chart.data.had %>% rename(had_touch = Response) %>% 
  left_join(bar.chart.data.want %>% rename(want_touch = Response), 
            by=c("tempID",  "Lives Alone", "Had Touch From"="Wanted Touch From", "Number Cohabiting")) %>% 
  left_join(bar.chart.data.typical %>% rename(typical_touch = Response), 
            by=c("tempID",  "Lives Alone", "Had Touch From"="Typical Touch From", "Number Cohabiting")) #%>% 
pivot_longer(cols=c(had_touch, want_touch, typical_touch), names_to='category', values_to='response')


# by age ####

## wanted ####

## had ####

# All subs ####

had_want <- comparison_data %>% #filter(category %in% c('had_touch', 'want_touch')) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch), as.numeric(.$want_touch),  paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

want_typical <- comparison_data %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$typical_touch), as.numeric(.$want_touch), paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

typical_had <- comparison_data %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch), as.numeric(.$typical_touch), paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

comparison_data %>%
  group_by(`Had Touch From`) %>% 
  summarise(had_mean = mean(as.numeric(had_touch), na.rm=T), 
            want_mean = mean(as.numeric(want_touch), na.rm=T), 
            typical_mean = mean(as.numeric(typical_touch),na.rm=T))


### people living alone

had_want_alone <- comparison_data %>% filter(`Lives Alone` == TRUE) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch), as.numeric(.$want_touch),  paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

want_typical_alone <- comparison_data %>% filter(`Lives Alone` == TRUE) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$typical_touch), as.numeric(.$want_touch), paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

typical_had_alone <- comparison_data %>% filter(`Lives Alone` == TRUE) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch), as.numeric(.$typical_touch), paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

comparison_data %>%
  filter(`Lives Alone` == TRUE) %>% 
  group_by(`Had Touch From`) %>% 
  summarise(had_mean = mean(as.numeric(had_touch), na.rm=T), 
            want_mean = mean(as.numeric(want_touch), na.rm=T), 
            typical_mean = mean(as.numeric(typical_touch),na.rm=T))


### people living alone

had_want_cohabit <- comparison_data %>% filter(`Lives Alone` == FALSE) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch), as.numeric(.$want_touch),  paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

want_typical_cohabit <- comparison_data %>% filter(`Lives Alone` == FALSE) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$typical_touch), as.numeric(.$want_touch), paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

typical_had_cohabit <- comparison_data %>% filter(`Lives Alone` == FALSE) %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch), as.numeric(.$typical_touch), paired=TRUE, conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

comparison_data %>%
  filter(`Lives Alone` ==  FALSE) %>% 
  group_by(`Had Touch From`) %>% 
  summarise(had_mean = mean(as.numeric(had_touch), na.rm=T), 
            want_mean = mean(as.numeric(want_touch), na.rm=T), 
            typical_mean = mean(as.numeric(typical_touch),na.rm=T))

p.adjust(c(had_want_cohabit$Wilcox.p, want_typical_cohabit$Wilcox.p, typical_had_cohabit$Wilcox.p))


## direct comparison lives alone vs not

typical_touch_alone_cohabit <- comparison_data %>%  
  filter(`Had Touch From` != 'Cohabitant') %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$typical_touch) ~ factor(.$`Lives Alone`), conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

had_touch_alone_cohabit <- comparison_data %>%  
  filter(`Had Touch From` != 'Cohabitant') %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$had_touch) ~ factor(.$`Lives Alone`), conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

want_touch_alone_cohabit <- comparison_data %>%  
  filter(`Had Touch From` != 'Cohabitant') %>% 
  group_by(`Had Touch From`) %>% 
  do(w = wilcox.test(as.numeric(.$want_touch) ~ factor(.$`Lives Alone`), conf.int=TRUE)) %>% 
  summarise(`Had Touch From`, Wilcox.p = w$p.value, Wilcox.conf.lower = w$conf.int[1], Wilcox.conf.upper = w$conf.int[2])

## NB: Only diff is in wanting touch from a close other
