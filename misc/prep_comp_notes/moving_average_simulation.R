time <- 200
noise <- rnorm(time)
ma_2 <- NULL
for(i in 3:time){
  
  ma_2[i] <- noise[i] + 0.7*noise[i-1] + 0.2*noise[i-2]
  
}

library(ggplot2)
df1 <- data.frame(
  'time' = c(1:time),
  'y' = c(ma_2)
)
ggplot(df1, aes(x = time, y = y)) + 
  geom_point() + 
  geom_line()

yy <- numeric(time)
zs <- numeric(time)
for(i in 1:time){
  
  if(i == 1){
    
    zs[i] <- rnorm(1,0,1)
    yy[i] <- zs[i]
  
  }else if(i == 2){
    
    zs[i] <- rnorm(1,0,1) 
    yy[i] <- zs[i] + 0.7*zs[i-1]
    
    }else{
  
    zs[i] <- rnorm(1,0,1)
    yy[i] <- zs[i] + 0.7*zs[i-1] + 0.2*zs[i-2]
  
    }
  
}

df2 <- data.frame(
  'time' = c(1:time),
  'y' = c(yy)
)

ggplot(df2, aes(x = time, y = yy)) + 
  geom_point() + 
  geom_line()
