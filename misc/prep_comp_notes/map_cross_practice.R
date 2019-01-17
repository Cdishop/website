library(tidyverse)
a <- seq(1:3)
b <- seq(1:3)

df1 <- data.frame(
  'chris' = c(4,5,6),
  'josh' = c(4,3,2)
)

df1 %>%
  filter(chris == chris[[1]]) %>%
  filter(josh == josh[[1]])

selector <- function(x, y){
  
  new <- df1 %>%
    filter(chris == chris[x]) %>%
    filter(josh == josh[y])
  return(new)
}

map2(a, b, selector)

# That doesn't work. Need to better label our data frame

selector2 <- function(x, y){
  
  new <- df1 %>%
    filter(chris == df1[['chris']][x]) %>%
    filter(josh == df1[['josh']][y])
  
  return(new)
}

map2(a, b, selector2)

# cross

map_overs <- data.frame(
  a= c(a),
  b = c(b)
)

map_overs %>%
  cross() %>%
  map(paste)

# cross gets me the combinations, but it doesn't map the way I want it to. Use cross to get me different vectors of "a" and "b" to map over

map_overs %>%
  cross_df()

cross_selector <- function(x, y){
  
  new <- df1 %>%
    filter(chris == df1[['chris']][x]) %>%
    filter(josh == df1[['josh']][y])
  return(new)
  
}

# need to update df1 though. The first filter returns a data frame with a single row, so the second filter is essentially useless and throws an error

df1 <- data.frame(
  'chris' = c(1,2,3,1,2,3,1,2,3),
  'josh' = c('int-w-1','int-w-2','int-w-3','int-w-3','int-w-1','int-w-2','int-w-2','int-w-3','int-w-1')
)

map_overs_use <- map_overs %>%
  cross_df()

map2(map_overs_use$a, map_overs_use$b, cross_selector)


