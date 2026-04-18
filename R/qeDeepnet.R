
library(deepnet)

qeDeepnet <- function(data,yName,hidden=c(100,100),activationfun="sigm", 
    learningrate = 0.001, momentum = 0.5, learningrate_scale = 1, 
    numepochs = 100, batchsize = 25, hidden_dropout = 0, yesYVal = NULL, 
    holdout = floor(min(1000, 0.1 * nrow(data)))) 
{
    checkPkgLoaded("deepnet")
    checkForNonDF(data)
    trainRow1 <- qeML:::getRow1(data, yName)
    classif <- is.factor(data[[yName]])
    ycol <- which(names(data) == yName)
    x <- data[, -ycol, drop = FALSE]
    y <- data[, yName]
    yLevels <- levels(y)
    if (classif) 
        y <- regtools::factorsToDummies(y, omitLast = FALSE)
    output <- "linear"
    if (!allNumeric(x)) {
        x <- regtools::factorsToDummies(x, omitLast = TRUE)
        factorsInfo <- attr(x, "factorsInfo")
    }
    else factorsInfo <- NULL
    holdIdxs <- tst <- trn <- NULL
    if (!is.null(holdout)) {
        holdIdxs <- sample(1:nrow(data), holdout)
        tsty <- y[holdIdxs]
        tstx <- x[holdIdxs, , drop = FALSE]
        trny <- y[-holdIdxs]
        trnx <- x[-holdIdxs, , drop = FALSE]
    }
    else {
        trnx <- x
        trny <- y
    }
    xm <- as.matrix(x)
    nnOut <- deepnet::nn.train(x = xm, y = y, hidden = hidden, 
        activationfun = activationfun, learningrate = learningrate, 
        momentum = momentum, learningrate_scale = learningrate_scale, 
        output = output, numepochs = numepochs, batchsize = batchsize, 
        hidden_dropout = hidden_dropout)
    class(nnOut) <- c("qeDeepnet")
    nnOut$classif <- classif
    nnOut$factorsInfo <- factorsInfo
    nnOut$yLevels <- yLevels
    if (!is.null(holdout)) {
        preds <- predict(nnOut, tstx)
        nnOut$testAcc <- if (classif) 
            mean(preds$predClasses != data[holdIdxs, ycol])
        else mean(abs(tsty - preds))
        nnOut$holdIdxs <- holdIdxs
    }
    else nnOut$holdIdxs <- NULL
    nnOut
}

predict.qeDeepnet <- function(object,newx)
{
    if (!allNumeric(newx))
        newx <- regtools::factorsToDummies(newx, omitLast = TRUE,
            factorsInfo = object$factorsInfo)
    else newx <- as.matrix(newx)
    if (object$classif) {
        probs <- deepnet::nn.predict(object, newx)
        colnames(probs) <- object$yLevels
        predClassIdxs <- apply(probs, 1, which.max)
        predClasses <- object$yLevels[predClassIdxs]
        list(predClasses = predClasses, probs = probs)
    }
    else deepnet::nn.predict(object, newx)
}

