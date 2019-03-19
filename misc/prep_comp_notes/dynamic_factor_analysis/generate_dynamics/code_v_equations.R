people <- 600
time <- 5
ar_affect <- 0.8
ar_perf <- 0.65
cl_affect_perf <- 0
cl_perf_affect <- 0

fl1_a <- 0.7
fl2_a <- 0.9
fl3_a <- 0.5
fl1_p <- 0.9
fl2_p <- 0.9
fl3_p <- 0.9

df_mat <- matrix(, ncol = 10, nrow = people*time)
count <- 0

for(i in 1:people){
  
  het_affect <- rnorm(1, 0, 4)
  het_perf <- rnorm(1, 0, 4)
  
  for(j in 1:time){
    count <- count + 1
    
    if(j == 1){
      
      # true affect
      df_mat[count, 1] <- het_affect + rnorm(1, 0, 1)
      # true performance
      df_mat[count, 2] <- het_perf + rnorm(1, 0, 1)
      
      # manifest affect - measurement error here rather than shock
      df_mat[count, 3] <- fl1_a*df_mat[count, 1] + rnorm(1, 0, 3)
      df_mat[count, 4] <- fl2_a*df_mat[count, 1] + rnorm(1, 0, 3)
      df_mat[count, 5] <- fl3_a*df_mat[count, 1] + rnorm(1, 0, 3)
      
      # manifest performance - measurement error here rather than shock
      df_mat[count, 6] <- fl1_p*df_mat[count, 2] + rnorm(1, 0, 3)
      df_mat[count, 7] <- fl1_p*df_mat[count, 2] + rnorm(1, 0, 3)
      df_mat[count, 8] <- fl1_p*df_mat[count, 2] + rnorm(1, 0, 3)
      
      # id and time
      df_mat[count, 9] <- i
      df_mat[count, 10] <- j
      
      
    }else{
      
      # true affect
      df_mat[count, 1] <- ar_affect*df_mat[count - 1, 1] + cl_perf_affect*df_mat[count - 1, 2] + het_affect + rnorm(1, 0, 1)
      # true performance
      df_mat[count, 2] <- ar_perf*df_mat[count - 1, 2] + cl_affect_perf*df_mat[count - 1, 1] + het_perf + rnorm(1, 0, 1)
      
      # manifest affect - measurement error here rather than dynamic noise
      df_mat[count, 3] <- fl1_a*df_mat[count, 1] + rnorm(1, 0, 3)
      df_mat[count, 4] <- fl2_a*df_mat[count, 1] + rnorm(1, 0, 3)
      df_mat[count, 5] <- fl3_a*df_mat[count, 1] + rnorm(1, 0, 3)
      
      # manifest performance - measurement error here rather than dynamic noise
      df_mat[count, 6] <- fl1_p*df_mat[count, 2] + rnorm(1, 0, 3)
      df_mat[count, 7] <- fl1_p*df_mat[count, 2] + rnorm(1, 0, 3)
      df_mat[count, 8] <- fl1_p*df_mat[count, 2] + rnorm(1, 0, 3)
      
      # id and time
      df_mat[count, 9] <- i
      df_mat[count, 10] <- j
      
    }
  }
  
  
  
}


# observe manifest indicators, id, and time

df <- data.frame(df_mat)
names(df) <- c('true_a', 'true_p', 'i1_affect', 'i2_affect', 'i3_affect', 'i1_perf', 'i2_perf', 'i3_perf', 'id', 'time')
library(tidyverse)
df <- df %>%
  select(-starts_with('true'))

library(reshape2)
df_wide <- reshape(df, idvar = 'id', timevar = 'time', direction = 'wide')

library(lavaan)
bb_string <- '

# latent affect across 5 time points
t_affect_1 =~ fl1_a*i1_affect.1 + fl2_a*i2_affect.1 + fl3_a*i3_affect.1
t_affect_2 =~ fl1_a*i1_affect.2 + fl2_a*i2_affect.2 + fl3_a*i3_affect.2
t_affect_3 =~ fl1_a*i1_affect.3 + fl2_a*i2_affect.3 + fl3_a*i3_affect.3
t_affect_4 =~ fl1_a*i1_affect.4 + fl2_a*i2_affect.4 + fl3_a*i3_affect.4
t_affect_5 =~ fl1_a*i1_affect.5 + fl2_a*i2_affect.5 + fl3_a*i3_affect.5

# latent performance across 5 time points
t_perf_1 =~ fl1_p*i1_perf.1 + fl2_p*i2_perf.1 + fl3_p*i3_perf.1
t_perf_2 =~ fl1_p*i1_perf.2 + fl2_p*i2_perf.2 + fl3_p*i3_perf.2
t_perf_3 =~ fl1_p*i1_perf.3 + fl2_p*i2_perf.3 + fl3_p*i3_perf.3
t_perf_4 =~ fl1_p*i1_perf.4 + fl2_p*i2_perf.4 + fl3_p*i3_perf.4
t_perf_5 =~ fl1_p*i1_perf.5 + fl2_p*i2_perf.5 + fl3_p*i3_perf.5

# eta affect - condition on first time point
eta_affect =~ 1*t_affect_2 + 1*t_affect_3 + 1*t_affect_4 + 1*t_affect_5

# eta performance - condition on first time point
eta_perf =~ 1*t_perf_2 + 1*t_perf_3 + 1*t_perf_4 + 1*t_perf_5

# autoregression affect
t_affect_2 ~ ba*t_affect_1
t_affect_3 ~ ba*t_affect_2
t_affect_4 ~ ba*t_affect_3
t_affect_5 ~ ba*t_affect_4

# autoregression performance
t_perf_2 ~ bp*t_perf_1
t_perf_3 ~ bp*t_perf_2
t_perf_4 ~ bp*t_perf_3
t_perf_5 ~ bp*t_perf_4

# cross lags
t_affect_2 ~ clpa*t_perf_1
t_affect_3 ~ clpa*t_perf_2
t_affect_4 ~ clpa*t_perf_3
t_affect_5 ~ clpa*t_perf_4

t_perf_2 ~ clap*t_affect_1
t_perf_3 ~ clap*t_affect_2
t_perf_4 ~ clap*t_affect_3
t_perf_5 ~ clap*t_affect_4

# covariance of etas with first time points
eta_affect ~~ eta_affect
eta_perf ~~ eta_perf

eta_affect ~~ t_affect_1
eta_perf ~~ t_perf_1

t_affect_1 ~~ t_affect_1
t_perf_1 ~~ t_perf_1
t_affect_1 ~~ t_perf_1

'
bb_model <- sem(bb_string, data = df_wide)
summary(bb_model, fit.measures = T, standardized = T)


# Now with Zhiyong Zhang’s code (Notre Dame) ---------------------------------------------------

# Matrices
# The basic equation 



# shock vector following multivariate normal with mean zeroes and #latent x #latent covariance
shock[,i]=t(rmvnorm(1,c(0,0),latent_covariance))

# 
f[,i]=fp%*%f0+v[,i]
e[,i]=t(rmvnorm(1,c(0,0,0,0,0,0),r))
y[,i]=h%*%f[,i]+e[,i]
f0<-f[,i]



people <- 600
store_wides <- list()

for(o in 1:people){
  
  
  dafsar1<-function(n){
    library(mvtnorm)
    
    y<-array(,dim=c(6,n))
    e<-array(,dim=c(6,n))
    
    f<-array(,dim=c(2,n))
    v<-array(,dim=c(2,n))
    
    #parameter:
    f0<-array(c(0,0),dim=c(2,1))
    
    h<-array(c(1,1,1,0,0,0,
               0,0,0,1,1,1),dim=c(6,2))
    
    fp<-array(c(.8,0,0,.8),dim=c(2,2))
    r<-array(c(1,0,0,0,0,0,
               0,1,0,0,0,0,
               0,0,1,0,0,0,
               0,0,0,1,0,0,
               0,0,0,0,1,0,
               0,0,0,0,0,1),dim=c(6,6))
    qq<-array(c(.36,.18,.18,.36),dim=c(2,2))
    
    for (i in 1:n){
      v[,i]=t(rmvnorm(1,c(0,0),qq))
      f[,i]=fp%*%f0+v[,i]
      e[,i]=t(rmvnorm(1,c(0,0,0,0,0,0),r))
      y[,i]=h%*%f[,i]+e[,i]
      f0<-f[,i]
    }
    return(list(y=t(y)[1001:n,],f=t(f)[1001:n,],v=t(v)[1001:n,],e=t(e)[1001:n,]))
  }
  
  run_it <- dafsar1(1004)
  
  df_it <- run_it[[1]]
  df_it <- data.frame(df_it)
  names(df_it) <- c('x1', 'x2', 'x3', 'x4', 'x5', 'x6')
  df_it$id <- c(1, 1, 1, 1)
  df_it$time <- c(1, 2, 3, 4)
  
  library(reshape2)
  df_wide <- reshape(df_it, idvar = 'id', timevar = 'time', direction = 'wide')
  store_wides[[o]] <- df_wide
  
}


