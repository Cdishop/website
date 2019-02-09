
intercept <- 3
constant <- 0.4
proportion <- 0.2

people <- 500
time <- 6

df <- matrix(, nrow = people*time, ncol = 3)
count <- 0

for(i in 1:people){
  
  y_het <- rnorm(1, 0, 2)
  
  for(j in 1:time){
    count <- count + 1
    
    if(j == 1){
      df[count, 1] <- i
      df[count, 2] <- j
      df[count, 3] <- y_het + rnorm(1,0,1)
    }else{
      df[count, 1] <- i
      df[count, 2] <- j
      df[count, 3] <- constant*(j-1) + proportion*((j-1)^2 + (j-1)) + y_het + rnorm(1,0,1)
    }
    
    
    
  }
  
  
  
}

library(tidyverse)
library(ggplot2)
library(reshape2)


df <- data.frame(df)
names(df) <- c('id', 'time', 'y')
random_ids <- sample(1:people, 5)
sample_df <- df %>%
  filter(id %in% random_ids)

ggplot(df, aes(x = time, y = y, group = id)) + 
  geom_point(color = 'grey85') + 
  geom_line(color = 'grey85') + 
  geom_point(data = sample_df, aes(x = time, y = y, group = id)) + 
  geom_line(data = sample_df, aes(x = time, y = y, group = id))

df_wide <- reshape(df, idvar = 'id', timevar = 'time', direction = 'wide')

library(lavaan)

dual_c_string <- '

# latent true scores over the observed y points
l_y1 =~ 1*y.1
l_y2 =~ 1*y.2
l_y3 =~ 1*y.3
l_y4 =~ 1*y.4
l_y5 =~ 1*y.5
l_y6 =~ 1*y.6

# latent change scores over the latent true scores
# y1 does not get one because it is the first time point
lc_y2 =~ 1*l_y2
lc_y3 =~ 1*l_y3
lc_y4 =~ 1*l_y4
lc_y5 =~ 1*l_y5
lc_y6 =~ 1*l_y6

# autoregression of the latent true scores (the first level latent variables)
l_y2 ~ 1*l_y1
l_y3 ~ 1*l_y2
l_y4 ~ 1*l_y3
l_y5 ~ 1*l_y4
l_y6 ~ 1*l_y5

# latent intercept over the first true score of y
latent_intercept =~ 1*l_y1

# CHANGE 1 OF THE DUAL CHANGE MODEL

# latent slope over the change scores
# this is called the change factor in dual change terminology...it is not really a slope term. It is the constant change factor
latent_slope =~ 1*lc_y2 + 1*lc_y3 + 1*lc_y4 + 1*lc_y5 + 1*lc_y6

# estimate covariance between latent intercept and slope (change factor)

latent_intercept ~~ latent_slope

# estimate mean and variance of intercept and slope (change factor)

latent_intercept ~~ latent_intercept
latent_slope ~~ latent_slope

latent_intercept ~ 1
latent_slope ~ 1

# CHANGE 2 OF THE DUAL CHANGE MODEL

# autoproportion change. Relationship between true score and latent change score at next time point
# these are estimated
lc_y2 ~ b*l_y1
lc_y3 ~ b*l_y2
lc_y4 ~ b*l_y3
lc_y5 ~ b*l_y4
lc_y6 ~ b*l_y5

# means and variances of latent factors set to zero

l_y1 ~ 0
l_y2 ~ 0
l_y3 ~ 0
l_y4 ~ 0
l_y5 ~ 0
l_y6 ~ 0

l_y1 ~~ 0*l_y1
l_y2 ~~ 0*l_y2
l_y3 ~~ 0*l_y3
l_y4 ~~ 0*l_y4
l_y5 ~~ 0*l_y5
l_y6 ~~ 0*l_y6

lc_y2 ~ 0
lc_y3 ~ 0
lc_y4 ~ 0
lc_y5 ~ 0
lc_y6 ~ 0

lc_y2 ~~ 0*lc_y2
lc_y3 ~~ 0*lc_y3
lc_y4 ~~ 0*lc_y4
lc_y5 ~~ 0*lc_y5
lc_y6 ~~ 0*lc_y6

# means of indicators set to zero

y.1 ~ 0
y.2 ~ 0
y.3 ~ 0
y.4 ~ 0
y.5 ~ 0
y.6 ~ 0

# residual variances constrained to be equal across time

y.1 ~~ res_var*y.1
y.2 ~~ res_var*y.2
y.3 ~~ res_var*y.3
y.4 ~~ res_var*y.4
y.5 ~~ res_var*y.5
y.6 ~~ res_var*y.6

# Constrain latent change factors to not correlate with each other

lc_y2 ~~ 0*lc_y3 + 0*lc_y4 + 0*lc_y5 + 0*lc_y6
lc_y3 ~~ 0*lc_y4 + 0*lc_y5 + 0*lc_y6
lc_y4 ~~ 0*lc_y5 + 0*lc_y6
lc_y5 ~~ 0*lc_y6


'

dual_change_model <- sem(dual_c_string, data = df_wide)
summary(dual_change_model, fit.measures = T)

