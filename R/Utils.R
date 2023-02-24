
# macros

# requireNamespace(gtools)

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

# same as above, but for qeKNN

predictHoldoutKNN <- defmacro(res,
   expr={
      tstx <- tst[,-ycol,drop=FALSE]
      trnx <- trn[,-ycol,drop=FALSE]
      trny <- trn[,ycol,drop=FALSE]
      tsty <- tst[,ycol]
      preds <- predict(res,tstx)
      if (!classif) preds <- preds[1,]
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds

      if (classif2) {
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
      }

      if (!classif2 &&classif) {
         predClasses <- preds$predClasses
         probs <- preds$probs
         preds <- list(predClasses=predClasses,probs=probs)
         tmp <- apply(tsty,1,which.max)
         charTsty <- colnames(tsty)[tmp]
         res$testAcc <- mean(predClasses != charTsty,na.rm=TRUE)
      }

      if (classif2) {
         predClasses <- preds$predClasses 
         res$testAcc <- mean(predClasses != tsty,na.rm=TRUE)
         res$baseAcc <- 1 - max(table(tst[,ycol])) / nrow(tst)
      } 
      
      if (!classif) {
         res$testAcc <- mean(abs(preds-tsty),na.rm=TRUE)
         meantrny <- mean(trny,na.rm=TRUE)
         res$baseAcc <- mean(abs(meantrny-tsty),na.rm=TRUE)
      }
      # res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
   }  # end of expr= for the macro
)

predictHoldoutXGB <- defmacro(res,
   expr={
      tstx <- tst[,-ycol,drop=FALSE]
      trnx <- trn[,-ycol,drop=FALSE]
      trny <- trn[,ycol,drop=FALSE]
      tsty <- tst[,ycol]
      preds <- predict(res,tstx)
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds

      if (classif) {
         predClasses <- preds$predClasses
         probs <- preds$probs
         preds <- list(predClasses=predClasses,probs=probs)
         charTsty <- yLevels[tsty]
         res$testAcc <- mean(predClasses != charTsty,na.rm=TRUE)
      }

      if (!classif) {
         res$testAcc <- mean(abs(preds-tsty),na.rm=TRUE)
         meantrny <- mean(trny[,1],na.rm=TRUE)
         res$baseAcc <- mean(abs(meantrny-tsty),na.rm=TRUE)
      }
      # res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
   }  # end of expr= for the macro
)

predictHoldoutNCV <- defmacro(res,
   expr={
      preds <- predict(res,tstx)
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds

      if (classif) {
         predClasses <- preds$predClasses
         probs <- preds$probs
         preds <- list(predClasses=predClasses,probs=probs)
         # charTsty <- yLevels[tsty]
         charTsty <- ifelse(tsty,yesYVal,noYVal)
         res$testAcc <- mean(predClasses != charTsty,na.rm=TRUE)
         prop1 <- mean(trny == yesYVal)
         res$baseAcc <- min(prop1,1-prop1)
      }

      if (!classif) {
         res$testAcc <- mean(abs(preds-tsty),na.rm=TRUE)
         meantrny <- mean(trny,na.rm=TRUE)
         res$baseAcc <- mean(abs(meantrny-tsty),na.rm=TRUE)
      }
      # res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
   }  # end of expr= for the macro
)

# lm() balks if a label begins with a digit; check to see if we have any
checkNumericNames <- function(nms)
{
   for (nm in nms) {
      s <- substr(nm,1,1)
      if (s >= '0' && s <= '9') {
         stop('factor level begins with a digit')
      }
   }
}

# prepend the string s to each element of the character vector v
prepend <- function(s,v)
{
   v <- as.character(v)
   for (i in 1:length(v)) {
      v[i] <- paste0(s,v[i])
   }
   as.factor(v)
}

# plot code for most

genericPlot <- function(x) 
{
   obj <- x
   class(obj) <- class(obj)[-1]  # actually not needed in many cases
   plot(obj)
}

whatSplit <- function(qeObj) 
{
}

# check whether an installed package is of version at least that
# specified in 'atleast'; latter of form x.y for now, not x.y.x, i.e.
# only the number and subnumber will be checked; e.g. '1.3.5' >= '1.3'
## checkPkgVersion <- function(pkgname,atleast) 
## {
##    pkgVersion <- as.character(packageVersion(pkgname))
##    nums <- strsplit(pkgVersion,'.',fixed=T)[[1]]
##    nums <- nums[1:2]
##    pkgVersion <- paste(nums,collapse='.')
##    pkgVersion >= atleast
## 
## }

# wrapper to load pkg that was only Suggested for, not Imported by, qeML

getSuggestedLib <- function(pkgName) 
   if (!requireNamespace(pkgName,quietly=TRUE))
      stop(paste0(pkgName, 'not loaded'))

# input could be tibble or data table
checkForNonDF <- defmacro(data,
   expr={
      if (class(data)[1] != 'data.frame') {
         data <- as.data.frame(data)
         warning('"data" converted to data frame')
      }
   }
) 

# simplify a factor to its top levels; input f, output f but with only
# only the most-frequent levels explicit, with all others combined to
# 'other'; any f level for which there lowCountThresh or fewer data points
# becomes 'other', with all other f levels retaining their names

factorToTopLevels <- function(f,lowCountThresh=0) 
{
   levelCounts <- table(f)
   if (lowCountThresh==0) {
      hist(levelCounts,xlab='counts per level',
         ylab='number of levels having a given count')
      lowCountThresh <- readline('enter lowCountThresh: ')
   }
   lowCounts <- which(levelCounts <= lowCountThresh)
   newLevels <- names(levelCounts)[-lowCounts]
   newLevels <- c(sort(newLevels),'other')
   fNew <- ifelse(f %in% newLevels,as.character(f),'other')
   as.factor(fNew)
}

checkPkgLoaded <- function(pkgName,whereObtain='CRAN') 
{

   # workaround
   cmd <- sprintf('%s <- NULL',pkgName)

   cmd <- sprintf('require(%s)',pkgName)
   if (!evalr(cmd)) {
      pkgErr <- sprintf('%s is required',pkgName)
      print(pkgErr)
      obtain <- paste0('if not installed, obtain from ',whereObtain)
      stop(sprintf('if not installed, obtain from %s',whereObtain))
   }
   requireNamespace(pkgName)

}


evalr <- function(toexec) {
   eval(parse(text=toexec),parent.frame())
}


