pandemic.data <- read_csv("../Social+touch+in+a+pandemic_June+8,+2021_21.33_processed.csv") %>% 
  reorder_ordinals()

pandemic.data <- pandemic.data %>% mutate(emo_loneliness_1 = ifelse(`Feel Emptiness` %in% c('yes!','yes','more or less'), 1, 0),
                         emo_loneliness_2 = ifelse(`Feel Miss People` %in% c('yes!','yes','more or less'), 1, 0),
                         emo_loneliness_3 = ifelse(`Feel Rejected` %in% c('yes!','yes','more or less'), 1, 0),
                         soc_loneliness_1 = ifelse(`Feel Can Rely` %in% c('no!','no','more or less'), 1, 0),
                         soc_loneliness_2 = ifelse(`Feel Can Trust` %in% c('no!','no','more or less'), 1, 0),
                         soc_loneliness_3 = ifelse(`Feel Close People` %in% c('no!','no','more or less'), 1, 0),
                         emotional_loneliness_score = emo_loneliness_1 + emo_loneliness_2 + emo_loneliness_3,
                         social_loneliness_score = soc_loneliness_1 + soc_loneliness_2 + soc_loneliness_3,
                         loneliness_score = emotional_loneliness_score+social_loneliness_score) %>% 
  mutate(`Lives Alone` = if_else(`Number Cohabiting` == 'I live alone', 'Alone', 'Cohabiting'),
         `Lives Alone`= factor(`Lives Alone`))

wilcox.test(pandemic.data$loneliness_score~pandemic.data$`Lives Alone`)
wilcox.test(pandemic.data$emotional_loneliness_score~pandemic.data$`Lives Alone`)
wilcox.test(pandemic.data$social_loneliness_score~pandemic.data$`Lives Alone`)

p.adjust(c(0.02259,0.0504,0.0777))

with(pandemic.data, plot(`Lives Alone`, loneliness_score))

summary(pandemic.data)

