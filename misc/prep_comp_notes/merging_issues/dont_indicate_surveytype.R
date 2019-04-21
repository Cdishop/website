df1 <- data.frame(
  'survey_type' = c('daily', 'daily', 'daily'),
  'time' = c(1, 2, 3),
  'score' = c(4, 6, 5)
)

df2 <- data.frame(
  'survey_type' = c('esm', 'esm', 'esm', 'esm', 'esm', 'esm', 'esm' , 'esm', 'esm'),
  'time' = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  'other_score' = c(9, 8, 7, 9, 8, 9, 8, 9, 5)
)

library(tidyverse)

df <- left_join(df2, df1)

df1 <- data.frame(
  'time' = c(1, 2, 3),
  'score' = c(4, 6, 5)
)

df2 <- data.frame(
  'time' = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  'score' = c(9, 8, 7, 9, 8, 9, 8, 9, 5)
)

library(tidyverse)

df <- left_join(df2, df1)
