# https://chengjunwang.com/post/en/2014-03-09-simulate-network-diffusion-with-r/

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
## change this graph object to a pure number
first_infected <- as_ids(first_infected)

# place the first infected individual into a list
infected <- list()
infected[[1]] <- first_infected


# iterate network dynamics
i <- 1



# find neighbors of those who are infected
neighbor <- unlist(neighborhood(gstar, 1, unlist(infected)))
# remove from this list people who are already infected
neighbor <- neighbor[!neighbor %in% c(unlist(infected))]

# flip a coin to see if each neighbor becomes infected
# this will give me a series of 1s and 0s
# each neighbor gets a 1 or a 0
# 1 means infected. 0 means fine
infects <- rbinom(length(neighbor), 1, prob = 0.8)

# combine my 1s and 0s series with the neighbor identifiers
allneighbors <- data.frame(
    "infected" = c(infects),
    "neighbor" = c(neighbor)
  )
  
# filter to only those neighbors who are infected
# save their numbers
infectedneighbors <- allneighbors %>% 
    filter(infected == 1) %>% 
    pull(neighbor)


# place these newly infected neighbors into my store list
infected[[i + 1]] <- infectedneighbors



# keep going
i <- i + 1


## note about the structure of the list
# it will be something like
# 24
# 1
# 13, 16, 28, 29, 40

### ...which means that 24 was infected at period 1, 1 was infected at period 2, and multiple people (13, 16, etc.) were infected at period 3
