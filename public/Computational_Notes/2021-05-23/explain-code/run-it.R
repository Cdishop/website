library(igraph)
library(animation)
library(tidyverse)



# create network
net_size <- 50
gstar <- graph.star(net_size)
plot(gstar)

# seed it, meaning place an infected individual (or individuals) into it
number_initially_infected <- 1
first_infected <- sample(V(gstar), number_initially_infected)
first_infected <- as_ids(first_infected)
infected <- list()
infected[[1]] <- first_infected


# iterate network dynamics

i <- 1
total_infected <- unlist(infected)
while(length(total_infected) < net_size){
  V(gstar)$color[V(gstar)%in%total_infected] <- "red"
  plot(gstar)
  
  neighbor <- unlist(neighborhood(gstar, 1, unlist(infected)))
  neighbor <- neighbor[!neighbor %in% c(unlist(infected))]
  infects <- rbinom(length(neighbor), 1, prob = 0.8)
  allneighbors <- data.frame(
    "infected" = c(infects),
    "neighbor" = c(neighbor)
  )
  infectedneighbors <- allneighbors %>% 
    filter(infected == 1) %>% 
    pull(neighbor)
  infected[[i + 1]] <- infectedneighbors
  total_infected <- unlist(infected)
  i <- i + 1
  
  V(gstar)$color[V(gstar)%in%total_infected] <- "red"
  plot(gstar)
}

# plot
V(gstar)$color[V(gstar)%in%total_infected] <- "red"
plot(gstar)








# lower the probability of infection

# create network
net_size <- 50
gstar <- graph.star(net_size)
plot(gstar)

# seed it, meaning place an infected individual (or individuals) into it
number_initially_infected <- 1
first_infected <- sample(V(gstar), number_initially_infected)
first_infected <- as_ids(first_infected)
infected <- list()
infected[[1]] <- first_infected


# iterate network dynamics

i <- 1
total_infected <- unlist(infected)
while(length(total_infected) < net_size){
  V(gstar)$color[V(gstar)%in%total_infected] <- "red"
  plot(gstar)
  
  neighbor <- unlist(neighborhood(gstar, 1, unlist(infected)))
  neighbor <- neighbor[!neighbor %in% c(unlist(infected))]
  infects <- rbinom(length(neighbor), 1, prob = 0.5)
  allneighbors <- data.frame(
    "infected" = c(infects),
    "neighbor" = c(neighbor)
  )
  infectedneighbors <- allneighbors %>% 
    filter(infected == 1) %>% 
    pull(neighbor)
  infected[[i + 1]] <- infectedneighbors
  total_infected <- unlist(infected)
  i <- i + 1
  
  V(gstar)$color[V(gstar)%in%total_infected] <- "red"
  plot(gstar)
}
