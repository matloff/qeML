
# qeML wrapper for 'torch for r', alternative to qeNeural not needing
# Python; see also the 'cito' package

# arguments:

#    data: standard qeML
#    yName: standard qeML
#    layers: the usual "c(100,100)" spec for neurons and layers
#    yesYVal: for factor Y, which level is to be considered Y = 1 not 0
#    learnRate: learning rate, vital, might need to be even e-05 or 2
#    wtDecay: weight decay, regularization
#    nEpochs: number of iterations
#    dropout fraction
#    holdout: as in other QE functions

# see examples at the end of this file

qeNNtorch <- function (data, yName, layers = c(100, 100), yesYVal = NULL, 
    learnRate = 0.001, wtDecay = 0, nEpochs = 100, dropout = 0, 
    holdout = floor(min(1000, 0.1 * nrow(data)))) 
{
    checkPkgLoaded("torch")
    checkForNonDF(data)
    trainRow1 <- qeML:::getRow1(data, yName)
    ycol <- which(names(data) == yName)
    y <- data[, ycol]
    if (is.factor(y)) {
        classif <- TRUE
        yLevels <- levels(y)
        if (length(yLevels) != 2) {
            stop("presently classif case only for binary Y")
        }
        if (is.null(yesYVal)) 
            yesYVal <- yLevels[1]
        y <- as.numeric(y == yesYVal)
    }
    else classif <- FALSE
if (classif) stop('not set up yet for classification problems')
    nYcols <- 1
    x <- data[, -ycol]
    classes <- sapply(data, class)
    if (sum(classes == "numeric") + sum(classes == "integer") < 
        ncol(data)) {
        x <- regtools::factorsToDummies(x, omitLast = FALSE, 
            dfOut = FALSE)
        factorsInfo <- attr(x, "factorsInfo")
    }
    else factorsInfo <- NULL
    holdIdxs <- tst <- trn <- NULL
    if (!is.null(holdout)) {
        nHold <- holdout
        holdIdxs <- sample(1:nrow(x), nHold)
        xTst <- x[holdIdxs, ]
        xTrn <- x[-holdIdxs, ]
        xTrn <- as.matrix(xTrn)
        xTst <- as.matrix(xTst)
        yTst <- y[holdIdxs]
        yTrn <- y[-holdIdxs]
        tst <- cbind(xTst, yTst)
        trn <- cbind(xTrn, yTrn)
        ycol <- ncol(trn)
    }
    xTrnTensor <- torch_tensor(xTrn, dtype = torch_float())
    yTrnTensor <- torch_tensor(yTrn, dtype = torch_float())
    xTstTensor <- torch_tensor(xTst, dtype = torch_float())
    yTstTensor <- torch_tensor(yTst, dtype = torch_float())
    nLayers <- length(layers)
    modelCall <- "model <- nn_sequential("
    for (i in 1:nLayers) {
        if (i == 1) 
            modelCall <- paste(modelCall, "nn_linear(ncol(x),layers[i]),")
        else modelCall <- paste(modelCall, "nn_linear(layers[i-1],layers[i]),")
        modelCall <- paste(modelCall, "nn_relu(),")
    }
    modelCall <- paste(modelCall, "nn_linear(layers[nLayers],1))")
    eval(parse(text = modelCall))
    model(xTrnTensor)
    optimizer <- optim_adam(model$parameters, lr = learnRate, 
        weight_decay = wtDecay)
    for (i in 1:nEpochs) {
        preds <- model(xTrnTensor)
        loss <- nnf_mse_loss(preds, yTrnTensor, reduction = "sum")
        optimizer$zero_grad()
        loss$backward()
        optimizer$step()
    }
    if (!is.null(holdout)) {
        preds <- model(xTstTensor)
        testAcc <- mean(abs(yTst - preds))
    }
    torchout <- list()
    torchout$classif <- classif
    torchout$trainRow1 <- qeML:::getRow1(data, yName)
    torchout$model <- model
    class(torchout) <- c("qeNNTorch", "torch_tensor")
    torchout$testAcc <- testAcc
    torchout
}

# examples

# data(mlb1)
# qeNNtorch(mlb1[, -1], "Weight",layers=c(200,200),nEpochs=500)$testAcc
