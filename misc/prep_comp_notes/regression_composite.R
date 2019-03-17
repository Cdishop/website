
N <- 500
sigma <- matrix(c(1.0, 0.1, 0.4,
                  0.1, 1.0, 0.4,
                  0.4, 0.4, 1.0), 3, 3, byrow = T)
mu <- c(0,0,0)

library(MASS)

df <- mvrnorm(N, mu, sigma)
df <- data.frame(df)
names(df) <- c('x1', 'x2', 'y')
df$id <- c(1:N)

summary(lm(y ~ x1 + x2,
           data = df))


library(tidyverse)
df <- df %>%
  mutate(composite_x = 0.33*x1 + 0.4*x2)

cor(df$y, df$composite_x)

summary(lm(y ~ composite_x,
           data = df))
