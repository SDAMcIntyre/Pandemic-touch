library(dplyr)

reorder_ordinals <- function(df) {
  df %>% 
    mutate(
      `Number Cohabiting` = factor(`Number Cohabiting`, levels = c('I live alone', 
                                                                   '1', '2', '3', 
                                                                   'More than 3')),
      across(.cols = starts_with(c('Wanted Touch', 'Had Touch', 'Typical Touch')),
             .fns =  ~ factor(.x, levels = c('Not at all', 
                                             'Once', 
                                             '2-5 times',
                                             'approx once a day', 
                                             '2-5 times per day',
                                             '6-10 times per day',
                                             'More than 10 times a day'))),
      
      across(.cols = contains(c('Attention', 'Gratitude', 'Calming', 'Sadness','Happiness', 'Love')),
             .fns = ~ factor(.x, levels = c('I would dislike it',
                                            'I don\'t mind',
                                            'I would like it',
                                            'I don\'t know / not applicable')))
    )
}