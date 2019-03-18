
people <- 500
time <- 4
autor <- -0.4
fl1 <- 0.9
fl2 <- 0.3
fl3 <- 0.4
fl4 <- 0.2
fl5 <- 0.5

data <- matrix(, ncol = 8, nrow = people*time)
# 1st column is true latent variable over time. other 5 columns are manifest observations
count <- 0

for(i in 1:people){
  
  y_het <- rnorm(1, 0, 2)
  
  for(j in 1:time){
    count <- count + 1
    if(j == 1){
      
      data[count, 1] <- y_het + rnorm(1, 40, 10)
      
      data[count, 2] <- fl1*data[count, 1] + rnorm(1, 0, 2)
      data[count, 3] <- fl2*data[count, 1] + rnorm(1, 0, 2)
      data[count, 4] <- fl3*data[count, 1] + rnorm(1, 0, 2)
      data[count, 5] <- fl4*data[count, 1] + rnorm(1, 0, 2)
      data[count, 6] <- fl5*data[count, 1] + rnorm(1, 0, 2)
      
      data[count, 7] <- i
      data[count, 8] <- j
      
    }else{
      
      data[count, 1] <- autor*data[count - 1, 1] + y_het + rnorm(1, 0, 1)
      
      data[count, 2] <- fl1*data[count, 1] + rnorm(1, 0, 2)
      data[count, 3] <- fl2*data[count, 1] + rnorm(1, 0, 2)
      data[count, 4] <- fl3*data[count, 1] + rnorm(1, 0, 2)
      data[count, 5] <- fl4*data[count, 1] + rnorm(1, 0, 2)
      data[count, 6] <- fl5*data[count, 1] + rnorm(1, 0, 2)
      
      data[count, 7] <- i
      data[count, 8] <- j
    }
  }
  
}

df <- data.frame(data)
names(df) <- c('true', 'x1', 'x2', 'x3', 'x4', 'x5', 'id', 'time')
library(tidyverse)
df <- df %>%
  select(starts_with('x'), id, time)

library(reshape2)
df_wide <- reshape(df, idvar = 'id', timevar = 'time', direction = 'wide')

library(lavaan)

bb_string <- '

# latent variable time 1
lv1 =~ fl1*x1.1 + fl2*x2.1 + fl3*x3.1 + fl4*x4.1 + fl5*x5.1
# latent variable time 2
lv2 =~ fl1*x1.2 + fl2*x2.2 + fl3*x3.2 + fl4*x4.2 + fl5*x5.2
# latent variable time 3
lv3 =~ fl1*x1.3 + fl2*x2.3 + fl3*x3.2 + fl4*x4.3 + fl5*x5.3
# latent variable time 4
lv4 =~ fl1*x1.4 + fl2*x2.4 + fl3*x3.4 + fl4*x4.4 + fl5*x5.4

# unobserved heterogeneity - condition on the first observation
eta_y =~ 1*lv2 + 1*lv3 + 1*lv4

# autoregression
lv2 ~ b1*lv1
lv3 ~ b1*lv2
lv4 ~ b1*lv3

# covary eta and first observation
eta_y ~~ eta_y
lv1 ~~ lv1
eta_y ~~ lv1

'

bb_model <- sem(bb_string, data = df_wide)
summary(bb_model, standardized = T)
