hm <- data.frame(
  'c' = c(1,2,3,4,5,6),
  'd' = c(4,5,6,7,8,8)
)

hm %>%
  summarise(
    mean_c = mean(c),
    sd_c = sd(c)
  )


# nope because you recalculated the column

hm %>%
  summarise(
    c = mean(c),
    c_sd = sd(c)
  )


# next
# ...
# ...

# running simulations by compiling through c ++