
people <- 500
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


library(lavaan)

sem_string <- '

# F1 latent variable time 1
lv11 =~ fl1*x1.1 + fl2*x2.1 + fl3*x3.1
# F1 latent variable time 2
lv12 =~ fl1*x1.2 + fl2*x2.2 + fl3*x3.2

# F2 latent variable time 1
lv21 =~ fl4*x4.1 + fl5*x5.1 + fl6*x6.1
# F2 latent variable time 2
lv22 =~ fl4*x4.2 + fl5*x5.2 + fl6*x6.2

# residual variances

x1.1 ~~ res_var*x1.1
x1.2 ~~ res_var*x1.2
x2.1 ~~ res_var*x2.1
x2.2 ~~ res_var*x2.2
x3.1 ~~ res_var*x3.1
x3.2 ~~ res_var*x3.2
x4.1 ~~ res_var*x4.1
x4.2 ~~ res_var*x4.2
x5.1 ~~ res_var*x5.1
x5.2 ~~ res_var*x5.2
x6.1 ~~ res_var*x6.1
x6.2 ~~ res_var*x6.2

# allow lvs at same time point to correlate
lv11 ~~ lv21
lv12 ~~ lv22

# autoregression

lv12 ~ autof1*lv11
lv22 ~ autof2*lv21

# cross paths
lv12 ~ b1*lv21
lv22 ~ b2*lv11

# factor variances
lv11 ~~ lv11
lv12 ~~ lv12
lv21 ~~ lv21
lv22 ~~ lv22

'

sem_model <- sem(sem_string, data = try[, -1])
summary(sem_model, standardized = T)

