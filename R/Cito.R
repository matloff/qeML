
qeNNtorchcito<-function(data,yName,holdout=floor(min(1000,0.1*nrow(data))),
   hidden=c(50L,50L),activation="selu",bias=TRUE,dropout=0,
   batchsize=NULL,lr=0.01,epochs=100,device=c("cpu"),
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
       yLevels <- levels(data[[yName]])
       if (length(yLevels) == 2) classif <- 'binary'
       else classif <- 'multicall'
    } else {
       classif <- 'regr'
       yLevels <- NULL
    }

    ycol <- which(names(data) == yName)
    dataY <- data[,ycol]
    dataX <- data[,-ycol]
    if (!allNumeric(dataX)) {
        dataX <- regtools::factorsToDummies(dataX,omitLast=TRUE,dfOut=TRUE)
        data <- cbind(dataX,dataY)
        ycol <- ncol(data)
        names(data)[ycol] <- yName
        factorsInfo <- attr(dataX, "factorsInfo")
    }
    else factorsInfo <- NULL

    holdIdxs <- tst <- trn <- NULL
    if (!is.null(holdout)) {
        holdIdxs <- sample(1:nrow(data), holdout)
        trnData <- data[-holdIdxs,]
        tstData <- data[holdIdxs,]
    }
    else {
        trnData <- data
        tstData <- NULL
    }
    frml <- paste0(yName,' ~ .')
    frml <- as.formula(frml)
    lossFtn <- 
       if (classif=='regr') 'mse'
       else if (classif=='binary') 'softmax'
       else 'cross-entropy'
    dnnout <- dnn(formula=frml,
       data=data,hidden=hidden,activation=activation, 
       bias=bias,dropout=dropout,loss=lossFtn,lambda=lambda,
       batchsize=batchsize,epochs=epochs,device=device,lr=lr,
       ,optimizer=optimizer)
    class(dnnout) <- c("qeNNtorchcito","citodnn")
    dnnout$classif <- classif
    ## dnnout$factorsInfo <- factorsInfo
    dnnout$yLevels <- yLevels
    if (!is.null(holdout)) {
        preds <- predict(dnnout, tstData[,-ycol],device=device)
        dnnout$testAcc <- if (classif) 
            mean(preds != data[holdIdxs,ycol])
        else mean(abs(data[holdIdxs,ycol] - preds))
        dnnout$holdIdxs <- holdIdxs
    }
    else dnnout$holdIdxs <- NULL
    dnnout
}

predict.qeNNtorchcito <- function(object,newX,device,...) 
{
   cito:::predict.citodnn(object=object,newdata=newX,device=device,
      reduce='mean')
}

