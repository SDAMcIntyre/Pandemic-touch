library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(svglite)
library(janitor)
library(patchwork)
source("reorder_ordinals.R")

# plot appearance ####
custom_colors <- c(scales::brewer_pal(type='div', palette='RdPu')(3))

theme_barchart_x60 <- theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size=16),
        strip.text.x = element_text(size=18),
        axis.text.x = element_text(angle=60, hjust=1, vjust=0.7,
                                   margin = margin(t = -30, r = 00, b = 0, l = 0)))

# read in data ####

pandemic.data <- read_csv("Social+touch+in+a+pandemic_June+8,+2021_21.33_processed.csv") %>% 
  reorder_ordinals() %>% 
  mutate(
    tempID = sample(1:n(), n()),
    `Age Group` = case_when(
      between(Age, 18, 34) ~ "18-34",
      between(Age, 35, 50) ~ "35-50",
      between(Age, 51, 65) ~ "51-65",
      between(Age, 66, 81) ~ "66-81" # only 4 above 75
    ))

pandemic.data %>% 
  ggplot() +
  geom_bar(aes(x = `Age Group`))

## wanted ####

want.data <- pandemic.data %>% 
  filter(!is.na(Age)) %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Wanted Touch', "Age Group"))) %>%  #c('Wanted Touch', 'Had Touch', 'Typical Touch')
  pivot_longer(
    cols = starts_with('Wanted Touch'),  
    names_prefix = 'Wanted Touch ',
    names_to = 'Touch From', 
    values_to = 'Wanted'
    ) %>% 
  mutate(`Touch From` = factor(`Touch From`, levels=c('Cohabitant', 'Someone Close', 'Professional','Stranger'))) %>% 
  na.omit() %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE))

had.data <- pandemic.data %>% 
  select(starts_with(c('tempID','Number Cohabiting', 'Had Touch', "Age Group"))) %>%  #c('Wanted Touch', 'Had Touch', 'Typical Touch')
  pivot_longer(cols = starts_with('Had Touch'),  names_prefix = 'Had Touch ',
               names_to = 'Touch From', values_to = 'Had') %>% 
  mutate(`Touch From` = factor(`Touch From`, levels=c('Cohabitant', 'Someone Close', 'Professional','Stranger'))) %>% 
  na.omit() %>% 
  filter(!is.na(`Number Cohabiting`)) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', TRUE, FALSE))

touch.comparison.data <- inner_join(want.data, had.data) %>% 
  mutate(
    Wanted.num = as.numeric(Wanted),
    Had.num = as.numeric(Had)
    ) %>% 
  mutate(`Relative Touch` = case_when(
    Wanted.num > Had.num ~ "Not enough",
    Wanted.num < Had.num ~ "Too much",
    Wanted.num == Had.num ~ "About right"
  )) %>% 
  mutate(
    `Relative Touch` = factor(`Relative Touch`, levels = c("Not enough", "About right", "Too much")),
    `Lives Alone` = factor(`Lives Alone`, levels=c(TRUE, FALSE), labels = c('Living alone', 'Cohabiting'))
    )

# bar chart ####

touch.comparison.data %>% 
  mutate() %>% 
  ggplot(aes(x=`Age Group`, fill=`Relative Touch`)) + 
  facet_grid(`Lives Alone` ~ `Touch From`, scales = "free") +
  geom_bar(position='fill', width = 0.9) + 
  scale_fill_manual(values=custom_colors,
                    guide = guide_legend(reverse=TRUE),
                    name = 'Touch received\n vs wanted') + 
  scale_y_reverse('Proportion of responses', 
                  breaks = c(0, 0.5, 1), 
                  labels = c('100%', '50%', '0%')) + 
  theme_barchart_x60 +
  #labs(title = "Figure 2.\n   Touch from...", x = "\nAge") +
  labs(title = "Touch from...", x = "\nAge") +
  #plot_annotation(caption = "Pilot data from an online survey of social touch during the pandemic, N = 359, primarily from Nordic countries. 18-34 (n = 148), 35-50 (n = 107), 51-65 (n = 69), 66-81 (n = 35).") +
  theme(legend.position = "bottom")
  
#812 x 550 pixels

ggsave('Figures/had_wanted_touch_stacked_bar_by_age_v2.png', width = 812, height = 550, units = "px")

touch.comparison.data %>% 
  group_by(`Age Group`, tempID) %>% 
  summarise() %>% tally() #%>% 
  #summarise(sum(n))
