
qeNNtorchcito<-function(data,yName,holdout=floor(min(1000,0.1*nrow(data)))
   hidden=c(50L,50L),activation="selu",bias=TRUE,dropout=0,
   batchsize=NULL,lr=0.01,epochs=100,device=c("cpu","cuda","mps"),
   loss=c("mse","mae","softmax","cross-entropy","gaussian","binomial",
      "poisson","mvp","nbinom"),lambda=0,
   optimizer=c("sgd","adam","adadelta","adagrad","rmsprop","rprop"),
   bootstrap=NULL,bootstrap_parallel=FALSE,plot=TRUE,verbose=TRUE,
   lr_scheduler=NULL,custom_parameters=NULL,
   early_stopping=FALSE,tuning=config_tuning()) 
{
    checkPkgLoaded("deepnet")
    checkForNonDF(data)
    trainRow1 <- qeML:::getRow1(data, yName)
    classif <- is.factor(data[[yName]])
    if (classif) {
       yLevels <- levels(y)
       if (length(yLevels) == 2) classif <- 'binary'
       else classif <- 'multicall'
    }
    ycol <- which(names(data) == yName)
    x <- data[, -ycol, drop = FALSE]
    y <- data[, yName]
    holdIdxs <- tst <- trn <- NULL
    if (!is.null(holdout)) {
        holdIdxs <- sample(1:nrow(data), holdout)
        trnData <- data[-holdixs,]
        tstData <- data[holdixs,]
    }
    else {
        trnData <- data
        tstData <- NULL
    }
    ## xm <- as.matrix(x)
    frml <- paste0(yName,' ~ .')
    frml <- as.formula(frml)
    lossFtn <- 
       if (!classif) 'mse'
       else if (classif=='binary) 'softmax'
       else 'cross-entropy'
    dnnout <- dnn(formula=frml,
       data=data,hidden=hidden,activation=activation, 
       bias=bias,dropout=dropout,loss=lossFtn,lambda=lambda,
       batchsize=batchsize,epochs=epochs,device=device,lr=lr,
       validation=validation,alpha=alpha,optimizer=optimizer)
    class(ddnnout) <- c("qeNNtorchcito")
    dnnout$classif <- classif
    ## dnnout$factorsInfo <- factorsInfo
    dnnout$yLevels <- yLevels
    if (!is.null(holdout)) {
        preds <- predict(dnnout, tstData[,-ycol])
        dnnout$testAcc <- if (classif) 
            mean(preds != data[holdIdxs,ycol])
        else mean(abs(data[holdIdxs,ycol] - preds))
        dnnout$holdIdxs <- holdIdxs
    }
    else dnnout$holdIdxs <- NULL
    dnnout
}
