# Merge similar questions
library(dplyr)
library(glue)


merge_questions_touch <- function(dataframe, names_of_cols, n_questions){
  # NB: It's very important that you give the not-live-alone column first!
  for(i in 1:n_questions){
    var_1 <- paste0(names_of_cols[1],'_', i+1)
    var_2 <- paste0(names_of_cols[2],'_', i)
    dataframe <- dataframe %>%
      mutate("{var_1}":= ifelse(get(var_1) == "", get(var_2), get(var_1))) %>% 
      select(-var_2)
  }
  return(dataframe)
}

merge_questions_worry <- function(dataframe, names_of_cols){
  # NB: It's very important that you give the not-live-alone column first!
  for(i in c(1,3)){
    var_1 <- paste0(names_of_cols[1],'_', i)
    var_2 <- paste0(names_of_cols[2],'_', i)
    dataframe <- dataframe %>%
      mutate("{var_1}":= ifelse(is.na(get(var_1)), get(var_2), get(var_1))) %>% 
      select(-var_2)
  }
  return(dataframe)
}



