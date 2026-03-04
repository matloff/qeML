

# arguments:  see above, plus

#     nRounds: number of boosting rounds
#     params: R list of tuning parameters; see documentation fo
#        xgboost::xgboost()
 
qeMlr3Torch <- function(data,yName,nRounds=250,
   neurons=c(100,100),batch_Size=16,epochs=50,device='cpu',
   validate=0.1,learnRate=0.001,
   holdout=floor(min(1000,0.1*nrow(data))))
{
stop('under construction')
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   ycol <- which(names(data) == yName)
   if (classif) {
      y <- data[,yName]
      yLevels <- levels(y)
      tmp <- as.integer(y) - 1
      data[,yName] <- tmp
      objective <- 'multi:softprob'
   } else {
      yLevels <- NULL
      objective='reg:squarederror'
   }

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      splitData(holdout,data)
      y <- data[-holdIdxs,ycol]
      x <- data[-holdIdxs,-ycol]
   } else {
      x <- data[,-ycol]
      y <- data[,ycol]
   }
   # if holdout, x,y are now the training set
   
   if (!allNumeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo') 
   } else factorsInfo <- NULL

   xm <- as.matrix(x)
   # set up learner
   if (classif) {
   } else {
      lrnr <- lrn('regr.mlp',
         activation=-n_relu)
   }



   xgbOut <- xgboost::xgboost(data=xm,label=y,nrounds=nRounds,
      param=params)
   class(xgbOut) <- c('qeMlr3Torch','xgb.Booster')

   xgbOut$classif <- classif
   xgbOut$factorsInfo <- factorsInfo
   xgbOut$yLevels <- yLevels
   xgbOut$yName <- yName

   if (!is.null(holdout)) {
      tst[,ycol] <- tst[,ycol] + 1
      predictHoldoutqeMlr3Torch(xgbOut)
      xgbOut$holdIdxs <- holdIdxs
    }
   else xgbOut$holdIdxs <- NULL

   xgbOut

}

predict.qeMlr3Torch <- function(object,x,...) 
{
   if (!allNumeric(x)) 
      x <- regtools::factorsToDummies(x,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   else x <- as.matrix(x)
   class(object) <- class(object)[-1]
   preds <- predict(object,x)
   if (object$classif) {
      preds <- t(matrix(preds,ncol=nrow(x)))
      colnames(preds) <- object$yLevels
      predClassIdxs <- apply(preds,1,which.max)
      predClasses <- object$yLevels[predClassIdxs]
      preds <- list(predClasses=predClasses,probs=preds)
   }
   preds
}

