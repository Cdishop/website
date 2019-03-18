require('OpenMx')
data('demoOneFactor')
data("demoOneFactor")
data(demoOneFactor)
nvar <- ncol(demoOneFactor) 
unam <- colnames(demoOneFactor)

# long format 1 time point many subjects

# number of latent states x 1 
amat <- mxMatrix(type = "Full", nrow = 1, ncol = 1, free = TRUE,
                 values = .3, name = "A", labels = "a")

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
                 .2, name = "R", labels = rlab)

# initial latent mean
xmat <- mxMatrix("Zero", 1, 1, name = "x0") 

# initial latent variance
pmat <- mxMatrix("Diag", 1, 1, FALSE, 1, name = "P0")

# input and covariate variables
umat <- mxMatrix("Zero", 1, 1, name = "u")


ssModel <- mxModel(model = "Lag1LAR", amat, bmat, cmat, dmat, qmat, rmat, xmat,
                   pmat, umat,
                   mxData(observed = demoOneFactor,
                          type = "raw"), mxExpectationStateSpace("A", "B", "C",
                                                                 "D", "Q", "R", "x0", "P0", "u"), mxFitFunctionML()
)

ssRun <- mxRun(ssModel)
summary(ssRun)


# Multilple subjects autoregression

nSubj <- 80
nTime <- 5
dataL <- list()
for(k in 1:nSubj){
  dataL[[k]] <- mxGenerateData(ssModel, nTime)
}

indivmodels <- list()
modNames <- paste0('indiv', 1:nSubj)
for(k in 1:nSubj){
  
  DataSetForSubjectK <- dataL[[k]]
  indivmodels[[k]] <- mxModel(name = modNames[k],
                              model = ssModel,
                              mxData(DataSetForSubjectK,
                                     type = 'raw'))
  
}

indivmodels[1]
multiSubjModel <- mxModel(name = 'MultiMod', indivmodels,
                          mxFitFunctionMultigroup(modNames))

multiSubjRun <- mxRun(multiSubjModel)
summary(multiSubjRun)


coef(ssModel)
coef(multiSubjRun)










DataSetForSubjectK


# Data for subject in list 
# store individual list
# For every k in that list, run the individual ss model and store in individual list
# run multi subject model on individual list