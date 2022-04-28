
#########################  qelightGBoost()  #################################

# lightgbM 

# arguments:  see above, plus

#     nTree: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qelightgboost <- function(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(lightgbm)
   classif <- is.factor(data[[yName]])
   ycol <- which(names(data) == yName)

   x <- data[,-ycol]
   y <- data[,ycol]
   x.save <- x
   if (!allNumeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   if (classif) data[,ycol] <- as.numeric(y)


   if (!is.null(holdout)) {
      splitData(holdout,x)
      trnx <- trn
      tstx <- tst
      trny <- y[-holdIdxs]
      tsty <- y[holdIdxs]
   } else {
      trnx <- x
      trny <- y
      tstx <- NULL
      tsty <- NULL
   }
   
   # convert to lighGBM binned form 
   trnxm <- as.matrix(trnx)
   lgbData <- lgb.Dataset(data=trnxm,label=trny)

   outlist <- 
      list(classif=classif,factorsInfo=factorsInfo,trnx=trnx,
         trainRow1=getRow1(data,yName))

   # regression case
   
   params <- list(min_data_in_leaf=minNodeSize,learning_rate=learnRate)
   if (classif) params$objective <- 'binary'
   cmd <- 'lgbout <- lgb.train(params=params,data=lgbData,obj="regression",'
   cmd <- paste0(cmd,'nrounds=nTree)')
   eval(parse(text=cmd))
   outlist$lgbout <- lgbout
   
   outlist$nTree <- nTree
   outlist$trainRow1 <- data[1,-ycol]
   class(outlist) <- c('qelightgboost')

   if (!is.null(holdout)) {
      # predictHoldout(outlist)
      if (inherits(tstx,'data.frame')) tstx <- as.matrix(tstx)
      preds <- predict(outlist$lgbout,tstx)
      outlist$holdoutPreds <- preds
      outlist$testAcc <- mean(abs(preds - tsty))
      outlist$baseAcc <- mean(abs(tsty - mean(tsty)))
      outlist$holdIdxs <- holdIdxs
   }
   outlist
}

# arguments:  see above
# value:  object of class 'qelightGBoost'; see above for components

predict.qelightgboost <- function(object,newx) 
{
   # newx <- setTrainFactors(object,newx)
   newx <- factorsToDummies(newx,omitLast=TRUE,factorsInfo=object$factorsInfo)
   # newx <- lgb.Dataset(data=newx)
   lgbout <- object$lgbout
   predict(lgbout,newx)
}

