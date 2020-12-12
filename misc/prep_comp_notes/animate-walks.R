library(dplyr)
runs <- 30
dm <- data.frame()
for(i in 1:runs){
  

t <- 50
w1 <- cumsum(rnorm(t, 0, 1))
w2 <- cumsum(rnorm(t, 0, 1))
dd <- data.frame(
  "time" = c(1:t,
             1:t),
  'score' = c(w1,
            w2),
  'var' = c(rep('w1', t),
            rep('w2', t)),
  'sim' = c(i)
)

dm <- bind_rows(dm, dd)

}



library(ggplot2)
library(gganimate)



ggplot(dd, aes(x = time, y = score, color = var)) + 
  geom_point() + 
  geom_line()

ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  # Here comes the gganimate specific bits
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')

library(gapminder)
hm <- gapminder

g1 <- ggplot(dm, aes(x = time, y = score, color = var)) + 
  geom_point() + 
  geom_line() + 
  labs(title = "Simulation: {frame_time}") + 
  transition_time(sim) + 
  ease_aes("linear")

animate(g1, fps = 10, width = 750, height = 450, renderer = gifski_renderer())
anim_save("g1.gif")
