
# qeML wrapper for 'torch for r', alternative to qeNeural not needing
# Python; see also the 'cito' package

# arguments:

#    data: standard qeML
#    yName: standard qeML
#    layers: list of lists; layers[[i]] specifies the layer type and any
#       parameters for layer; the fan-in may be specified as 0 for the
#       first layer, in which case it will be set the number of features
#       (after factors, if any, are converted to dummies)
#    yesYVal: for factor Y, which level is to be considered Y = 1 not 0
#    learnRate: learning rate, vital, might need to be even e-05 or 2
#    wtDecay: weight decay, regularization
#    nEpochs: number of iterations
#    dropout fraction
#    holdout: as in other QE functions

# see examples at the end of this file

qeNeuralTorch <- function (data, yName, layer, yesYVal = NULL, 
    learnRate = 0.001, wtDecay = 0, nEpochs = 100, dropout = 0, 
    holdout = floor(min(1000, 0.1 * nrow(data)))) 
{
    checkPkgLoaded("torch")
    qeML:::checkForNonDF(data)
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
    yToAvg <- y
    nYcols <- 1
    x <- data[, -ycol]
    classes <- sapply(data, class)
    if (sum(classes == "numeric") + sum(classes == "integer") < 
        ncol(data)) {
        x <- regtools::factorsToDummies(x, omitLast = TRUE)
        factorsInfo <- attr(x, "factorsInfo")
    }
    else factorsInfo <- NULL
    holdIdxs <- tst <- trn <- NULL
    if (!is.null(holdout)) 
        qeML:::makeHoldout(0)
    x <- as.matrix(x)
    xT <- torch_tensor(x)
    yToAvg <- matrix(as.numeric(yToAvg, ncol = 1))
    yT <- torch_tensor(yToAvg)

    # prep to create model
    nnSeqArgs <- list(); nsa <- 0
    i <- 1
    while (i <= length(layer)) {
       nsa <- nsa + 1
       if (layer[[i]] == "linear") {
           nnSeqArgs[[nsa]] <- nn_linear(layer[[i+1]], layer[[i+2]])
           i <- i + 3
       } else if (layer[[i]] == "relu") {
           nnSeqArgs[[nsa]] <- nn_relu(inplace = FALSE)
           i <- i + 1
       }
       else if (layer[[i]] == "dropout") {
         nnSeqArgs[[nsa]] <- nn_dropout(dropout)
           i <- i + 1
       }
       else {
          nnSeqArgs[[nsa]] <- nn_sigmoid()
          i <- i + 1
       }
    }

    model <- do.call(nn_sequential, nnSeqArgs)
    optimizer <- optim_adam(model$parameters, lr = learnRate, 
        weight_decay = wtDecay)
    for (i in 1:nEpochs) {
        preds <- model(xT)
        loss <- nnf_mse_loss(preds, yT, reduction = "sum")
        optimizer$zero_grad()
        loss$backward()
        optimizer$step()
    }
    torchout <- list()
    torchout$classif <- classif
    torchout$x <- x
    torchout$xT <- xT
    torchout$yT <- yT
    torchout$trainRow1 <- qeML:::getRow1(data, yName)
    torchout$model <- model
    class(torchout) <- c("qeNeuralTorch", "torch_tensor")
    ## if (!is.null(holdout)) {
    ##     ## predictHoldoutTorch(torchout)
    ##     preds <- model(
    ##     torchout$holdIdxs <- holdIdxs
    ## }
    torchout$preds <- preds
    torchout
}

predictHoldoutTorch <- defmacro(res,
   expr={

      ycol <- which(names(tst) == 'yTst');
      tstx <- tst[,-ycol,drop=FALSE];
      trnx <- trn[,-ycol,drop=FALSE];
      # tsty <- tst[,ycol]

      preds <- predict(res,tstx);
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds
         
      if (res$classif) {
         #### stop('regression case only for now')
         preds <- round(as.numeric(preds[,1]))  
         res$testAcc <- mean(preds!=tst[,ycol])
      } else {  # regression case
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         ### res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycol])))
         predsTrn <- predict(res,trnx)
         res$trainAcc <- mean(abs(predsTrn - trn[,ycol]))
      }  
   }  # end of expr= for the macro
)        

predict.qeNeuralTorch <- function(object,newx,...)
{
   finfo <- object$factorsInfo
   if(!is.null(finfo)) {
      newx <- factorsToDummies(newx,omitLast=TRUE,factorsInfo=finfo)
   }
   newx <- as.matrix(newx)
   newxT <- torch_tensor(newx)
   object$model(newxT)
}

makeHoldout <- defmacro(placeholder,expr=
   {
      nHold <- holdout
      holdIdxs <- sample(1:nrow(x),nHold)
      xTst <- x[holdIdxs,]
      x <- x[-holdIdxs,]
      yTst <- yToAvg[holdIdxs]
      yToAvg <- yToAvg[-holdIdxs]
      tst <- cbind(xTst,yTst)
      tst <- as.data.frame(tst)
      trn <- cbind(x,yToAvg)
      ycol <- ncol(trn)
      nrX <- nrow(x)
      ncX <- ncol(x)
   }
)

# lyrsClass <- list( 
#    list('linear',0,100), 
#    list('relu'), 
#    list('linear',100,100), 
#    list('relu'), 
#    list('linear',100,1), 
#    list('sigmoid')) 

# svcensus from qeML
# qeNeuralTorch(svcensus,'gender',yesYVal='male',
#    layers=lyrsClass,learnRate=0.008)$testAcc

# WA phone churn data, from my ML book
# qeNeuralTorch(tc,'Churn',yesYVal='Yes',layers=lyrsClass,
#    learnRate=0.0035)$testAcc

# from dsld package
# qeNeuralTorch(mortgageSE,'deny',yesYVal='1',layers=lyrsClass,
#    learnRate=0.003)$testAcc

# lyrsReg <- list(
#   list('linear',0,100),
#   list('relu'),
#   list('linear',100,1))
#
# qeNeuralTorch(svcensus,'wageinc',layers=lyrsReg,
#   learnRate=0.05)$testAcc
