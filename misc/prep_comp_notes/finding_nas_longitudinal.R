
cd <- data.frame(
  'time' = c(1,1,1,2,2,3,3,3,4,4),
  'id' = c('a', 'b', 'c', 'a', 'b', 'a', 'b', 'c', 'a', 'b'),
  'q1' = c(4,5,3,6,7,5,4,3,4,5),
  'q2' = c(3,2,4,1,2,3,4,2,3,4)
)

cd %>%
 spread(key = time, value = q1)


cd # the spread command imputes too many NA's because it retains the other variables in long format

library(reshape2)

reshape(cd, timevar = 'time', idvar = 'id', direction = 'wide')

# so they are not equivalent unless I trim down cd first

r1 <- reshape(cd, timevar = 'time', idvar = 'id', direction = 'wide')
r2 <- reshape(r1, timevar = 'time', idvar = 'id', direction = 'long')
r2 # boom, I know that C is missing values at certain times


# Now, to do the equivalent with tidyr I have to select just one variable

cd %>%
  select(time, id, q1) %>%
  spread(key = time, value = q1) %>%
  gather(key = time, value = 'q1', '1','2','3','4') # I also have to do this weird string thing
  # plus I have to merge it back with original data set

time_string <- as.character(unique(cd$time))

cd %>%
  select(time, id, q1) %>%
  spread(key = time, value = q1) %>%
  gather(key = time, value = 'q1', time_string)
  



# also a ggplot application of column names as parameters -----------------


library(data.table)
c4_full_weekly <- data.table(c4_full_weekly)

ggplot() + 
  geom_point(data = c4_full_weekly[mission_id == 2032,], aes(x = date_weekly, y = leadership_2036), color = 'blue') + 
  geom_line(data = c4_full_weekly[mission_id == 2032,], aes(x = date_weekly, y = leadership_2036, color = 'Tristan')) + 
  geom_point(data = c4_full_weekly[mission_id == 2036,], aes(x = date_weekly, y = leadership_2032), color = 'red') + 
  geom_line(data = c4_full_weekly[mission_id == 2036,], aes(x = date_weekly, y = leadership_2032, color = 'Andrzej')) + 
  ylab('Leadership rating of other') + 
  scale_color_discrete(name = NULL)



paired_plot <- function(id1, id2, name1, name2, col1, col2){
  
  
  create <-  ggplot() + 
    geom_point(data = c4_full_weekly[mission_id == id1,], aes(x = date_weekly, y = !!col1), color = 'blue') + 
    geom_line(data = c4_full_weekly[mission_id == id1,], aes(x = date_weekly, y = !!col1, color = name1)) + 
    geom_point(data = c4_full_weekly[mission_id == id2,], aes(x = date_weekly, y = !!col2), color = 'red') + 
    geom_line(data = c4_full_weekly[mission_id == id2,], aes(x = date_weekly, y = !!col2, color = name2)) + 
    ylab('Leadership rating of other') + 
    scale_color_discrete(name = NULL)
  
  return(create)
}


paired_plot(2031, 2032, 'Cyprien', 'Tristin', quo(leadership_2031), quo(leadership_2032))
