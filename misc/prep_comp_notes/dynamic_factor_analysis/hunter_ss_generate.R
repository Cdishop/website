
people <- 500
time <- 10
autor <- 0.4
fl <- 0.6

data <- matrix(, ncol = 8, nrow = people*time)
# 1st column is true latent variable over time. other 5 columns are manifest observations
count <- 0

for(i in 1:people){
  
  y_het <- rnorm(1, 0, 2)
  
  for(j in 1:time){
    count <- count + 1
    if(j == 1){
      
      data[count, 1] <- y_het + rnorm(1, 40, 10)
      
      data[count, 2] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 3] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 4] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 5] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 6] <- fl*data[count, 1] + rnorm(1, 0, 2)
      
      data[count, 7] <- i
      data[count, 8] <- j
      
    }else{
      
      data[count, 1] <- autor*data[count - 1, 1] + y_het + rnorm(1, 0, 1)
      
      data[count, 2] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 3] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 4] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 5] <- fl*data[count, 1] + rnorm(1, 0, 2)
      data[count, 6] <- fl*data[count, 1] + rnorm(1, 0, 2)
      
      data[count, 7] <- i
      data[count, 8] <- j
    }
  }
  
}

df <- data.frame(data)
names(df) <- c('true', 'x1', 'x2', 'x3', 'x4', 'x5', 'id', 'time')
library(tidyverse)
df <- df %>%
  select(starts_with('x'), id, time)

individual_dfs <- list(people)

for(i in 1:people){
  individual_dfs[[i]] <- df %>%
    filter(id == i) %>%
    dplyr::select(starts_with('x'))
}




# base model --------------------------------------------------------------

nvar <- ncol(individual_dfs[[1]]) 
unam <- colnames(individual_dfs[[1]])

# long format 1 time point many subjects

# number of latent states x 1 
amat <- mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
                 name = "A", labels = "a")

# number of latent states by number of covariates or inputs
bmat <- mxMatrix("Zero", 1, 1, name = "B")

# number of observed outputs (manifest variables) by number of latent states
clab <- paste0("load", 1:nvar)
cdim <- list(unam, "F1")
cmat <- mxMatrix("Full", nvar, 1, TRUE, .6, name = "C",
                 dimnames = cdim, labels = clab)

# number of observed outputs (manifest variables) by number of covariates or inputs
dmat <- mxMatrix("Zero", nvar, 1,
                 name = "D")

# number of latent states by 1
qmat <- mxMatrix("Diag", 1, 1, FALSE, 1,
                 name = "Q")

# number of observed outputs (manifest variables) by 1
rlab <- paste0("resid", 1:nvar)
rmat <- mxMatrix("Diag", nvar, nvar, TRUE,
                 name = "R", labels = rlab)

# initial latent mean
xmat <- mxMatrix("Zero", 1, 1, name = "x0") 

# initial latent variance
pmat <- mxMatrix("Diag", 1, 1, FALSE, 1, name = "P0")

# input and covariate variables
umat <- mxMatrix("Zero", 1, 1, name = "u")


ssModel <- mxModel(model = "Lag1LAR", amat, bmat, cmat, dmat, qmat, rmat, xmat,
                   pmat, umat,
                   mxExpectationStateSpace("A", "B", "C",
                                           "D", "Q", "R", 
                                           "x0", "P0", "u"), mxFitFunctionML()
)



# structure individual models ---------------------------------------------------

individual_models <- list(people)
model_names <- paste0('indiv', 1:people)
for(i in 1:people){
  
  individual_models[[i]] <- mxModel(name = model_names[i],
                              model = ssModel,
                              mxData(individual_dfs[[i]],
                                     type = 'raw'))
  
}



# structure multiple subject model ----------------------------------------

multi_subject_model <- mxModel(name = 'MultiMod', individual_models,
                          mxFitFunctionMultigroup(model_names))


# Run multiple subject model ----------------------------------------------

multi_subj_model_fit <- mxRun(multi_subject_model)
summary(multi_subj_model_fit)


