
# macros

requireNamespace(gtools)

# standard split into training, test sets

# arguments:
#    holdout: holdout set size
#    data: XY data frame

# globals/value:
#    tst: the generated holdout set
#    data: the correspondingly reduced training set
#    holdIdxs: indices of the holdout set in original ata

splitData <- defmacro(holdout,data, 
   expr={
      nHold <- holdout;
      cat('holdout set has ',nHold, 'rows\n');
      idxs <- sample(1:nrow(data),nHold);
      tst <- data[idxs,,drop=FALSE];
      trn <- data[-idxs,,drop=FALSE];
      data <- data[-idxs,,drop=FALSE];
      holdIdxs <- idxs
   }
)

# deprecated, gradually moving to toAllNumeric()
# x: 
#    change character variables to factors, then all factors to dummies,
#    recording factorInfo for later use in prediction; put result in xm
# data: 
#    if character, change to factor
makeAllNumeric <- defmacro(x,data,
   expr={
      data <- regtools::charsToFactors(data)
      if (regtools::hasFactors(x)) {
         xm <- regtools::factorsToDummies(x,omitLast=TRUE)
         factorsInfo <- attr(xm,'factorsInfo')
      } else {
         xm <- as.matrix(x)
         factorsInfo <- NULL
      }
   }
) 

# does the predictions in the holdout set, also setting testAcc and
# baseAcc, and checking for new levels of Y in the holdout set

# ASSUMPTIONS:  

#    this is for Y either in the regression setting or in the
#    2-class classification setting;

#    predict() will be called, returning preds

#    in the regression setting, preds is numeric

#    in the classification setting, it is assumed that

#       preds is list(predicted class names,class probabilities) or

#       preds is a vector of class probabilities; if so, it is
#       assumed that yesYVal is defined

# arguments:
#    res: ultimate output of qe*()

# global inputs (from the caller):

#    trn, tst: training, holdout set
#    data: arg in the qe*() function
#    yName: arg in the qe*() function

# global outputs (creating locals in the caller):

#     res$testAcc: MAPE or class. error in holdout set
#     res$baseAcc: base MAPE or class. error (no features) in holdout set
#     res$holdoutPreds: predicted values in the holdout set
#     preds: ditto 
#     ycol: index of yName in 'data'
#     tstx: X portion of holdout data

predictHoldout <- defmacro(res,
   expr={
      ycol <- which(names(tst) == yName);
      tstx <- tst[,-ycol,drop=FALSE];
      trnx <- trn[,-ycol,drop=FALSE];
      tsty <- tst[,ycol]
      newLvls <- regtools:::checkNewLevels(trnx,tstx)
      if (length(newLvls) > 0) {
         tstx <- tstx[-newLvls,,drop=FALSE]
         tst <- tst[-newLvls,,drop=FALSE]
         warning(paste(length(newLvls),
            'rows removed from test set, due to new factor levels'))
      }

      preds <- predict(res,tstx);
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds

      if (res$classif) {
         yesNo <- !is.null(res$yesYVal)
         if (is.numeric(preds)) {
            probs <- preds
            predClasses <- round(probs)
            if (is.numeric(tsty)) {
               predClasses <- 
                  ifelse(predClasses,res$yesYVal,res$noYVal)
               if (is.numeric(tsty)) 
                  tsty <- ifelse(tsty,res$yesYVal,res$noYVal)
               preds <- list(predClasses=predClasses,probs=probs)
            } 
         } 
         if (listPreds) predClasses <- preds$predClasses
         # at this point, predClasses should be an R factor in either
         # case re preds; same for tsty
         res$testAcc <- mean(predClasses != tsty)
         res$baseAcc <- 1 - max(table(data[,ycol])) / nrow(data)
         # res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
      } else {  # regression case
         numericClassPreds <- FALSE
         res$testAcc <- mean(abs(preds - tst[,ycol]),na.rm=TRUE)
         res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycol])))
         predsTrn <- predict(res,trnx)
         res$trainAcc <- mean(abs(predsTrn - trn[,ycol]),na.rm=TRUE)
      }
   }  # end of expr= for the macro
)


