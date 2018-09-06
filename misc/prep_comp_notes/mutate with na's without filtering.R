library(tidyverse)

cd <- data.frame(
  "q1" = c(1,2,NA),
  "q2" = c(2,2,2),
  'q3' = c(NA, NA,2),
  'id' = c('201', '202', '203')
)

# NA's 

cd %>%
  mutate(cohesion = 
           q1 + q2 + q3)

# Filter removes the data we want

cd %>%
  filter(!is.na(q1) == T && !is.na(q2) == T && !is.na(q3) == T)

# Can't use rowMeans because rowMeans removes the NA column in its calculation...but mutate doesn't. So we get conflicting functions

cd %>%
  mutate(cohesion =
           rowMeans(q1, q2, q3, na.rm = T))

# Taking the mean and using na.rm = T is close. But wrong answer

cd %>%
  rowwise() %>%
  mutate(mean = mean(q1, q2, q3, na.rm = T))

# We need to vectorize that those questions for it to work

cd %>%
  rowwise() %>%
  mutate(mean = mean(c(q1, q2, q3), na.rm = T))

