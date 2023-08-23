
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
      # cat('holdout set has ',nHold, 'rows\n');
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
      newLvls <- checkNewLevels(trnx,tstx)
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

      # multiclass Y
      if (!classif2 && classif) {
         predClasses <- preds$predClasses
         probs <- preds$probs
         preds <- list(predClasses=predClasses,probs=probs)
         tstY <- origY[holdIdxs]   ###
         tstY <- paste0('dfr.',tstY)   ###
         res$testAcc <- mean(predClasses != tstY,na.rm=TRUE)
         res$baseAcc <- 1 - max(colMeans(trny))
      }

      if (classif2) {
         predClasses <- preds$predClasses 
         tstyClasses <- ifelse(tsty,yesYVal,noYVal)
         res$testAcc <- mean(predClasses != tstyClasses,na.rm=TRUE)
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

######################   factorToTopLevels etc.   ########################

# VERY USEFUL! often a factor will have 1 or more rare levels in the
# data; that already raises overfitting concerns (e.g. in a parametric
# model, more dummy coefficients), but in doing cross-validation, there
# may be "surprise" levels in the test set that were not there in the
# training set

# so we have these functions:

#   levelCounts(data); simply applies table() to each column of 'data',
#      returing the result as a list; if more than 10 levels, reports NA

#   factorToTopLevels(f,lowCountThresh=0); inputs the factor f
#      and replaces each instace of a rare level (defined by
#      lowCountThresh) by the new level 'other'

#   dataToTopLevels(data,lowCountThresholds); applies factorToTopLevels 
#      to each column of 'data'; lowCountThresholds is an R list,
#      indexed by the desired column names

levelCounts <- function(data) {
   makeTable <- function(col) if (is.factor(col)) table(col) else NA
   tmp <- lapply(data,makeTable)
   tmp
}

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

dataToTopLevels <- defmacro(data,lowCountThresholds,expr=
   {
      for(nm in names(lowCountThresholds)) {
         lctnm <- lowCountThresholds[[nm]]
         data[[nm]] <- factorToTopLevels(data[[nm]],lctnm)
      }
   }
)

# run R call from string
evalr <- function(toexec) {
   eval(parse(text=toexec),parent.frame())
}

# extract args from ...

getDotsArgs <- defmacro(argName, expr=
   {
      v <- c(as.list(environment()), list(...))
      if (is.null(v[[argName]])) {
         stop(sprintf('%s argument missing',argName))
      } else {
         cmd <- sprintf('%s <<- v$%s',argName,argName)
         evalr(cmd)
      }
   }
)

# try to require() pkg; if not installed, tell user where to get it
checkPkgLoaded <- function(pkgName,whereObtain='CRAN') 
{
      # workaround
      cmd <- sprintf('%s <- NULL',pkgName)
      evalr(cmd)
   
      cmd <- sprintf('require(%s)',pkgName)
      if (!evalr(cmd)) {
         pkgErr <- sprintf('%s is required',pkgName)
         print(pkgErr)
         obtain <- paste0('if not installed, obtain from ',whereObtain)
         stop(sprintf('if not installed, obtain from %s',whereObtain))
      }
      requireNamespace(pkgName)
}

# from regtools, but included here due to package rules:

checkNewLevels <- function (info1, data2) 
{
    tmp <- sapply(data2, is.factor)
    factorNames <- names(data2)[tmp]
    if (is.data.frame(info1)) {
        tmp <- sapply(info1, is.factor)
        tmp <- names(info1)[tmp]
        levelsPresent1 <- lapply(info1[tmp], function(t) unique(t))
    }
    else if (setequal(names(info1), names(data2))) {
        levelsPresent1 <- info1
    }
    else {
        levelsPresent1 <- info1$factorLevelsPresent
    }
    res <- NULL
    for (nm in factorNames) {
        tmp <- setdiff(levels(data2[[nm]]), levelsPresent1[[nm]])
        if (length(tmp) > 0) {
            matches <- which(data2[[nm]] %in% tmp)
            res <- union(res, matches)
        }
    }
    res
}

predict.krsFit <- function (object, ...) 
{
    arglist <- list(...)
    newx <- arglist[[1]]
    if (!inherits(newx, "matrix")) 
        newx <- as.matrix(newx)
    model <- object$model
    mm <- object$mmScaleX
    if (!is.null(mm)) 
        newx <- regtools::mmscale(newx, mm)
    if (!is.null(object$xShape)) {
        newx <- matrixToTensor(newx, object$xShape)
    }
    preds <- predict(model, newx)
    if (object$classif) {
        probs <- preds
        preds <- apply(preds, 1, which.max) - 1
        attr(preds, "probs") <- probs
    }
    else {
        mm <- object$mmScaleY
        if (!is.null(mm)) 
            preds <- mm[1] + preds * (mm[2] - mm[1])
    }
    preds
}

matrixToTensor <- function (x, xShape) 
{
    nrw <- xShape[1]
    ncl <- xShape[2]
    if (length(xShape) == 3) {
        nch <- xShape[3]
    }
    else {
        nch <- ncol(x)/(nrw * ncl)
        xShape <- c(xShape, nch)
    }
    # res <- keras::array <- reshape(x, c(nrow(x), nrw, ncl, nch))
    res <- reshape(x, c(nrow(x), nrw, ncl, nch))
    attr(res, "xShape") <- xShape
    res
}

plot.tuner <- function (x, ...) 
{
    arglist <- list(...)
    tmp <- arglist["col"][[1]]
    col <- if (!is.null(tmp)) 
        tmp
    else "meanAcc"
    tmp <- arglist["disp"][[1]]
    disp <- if (!is.null(tmp)) 
        tmp
    else 0
    tmp <- arglist["jit"][[1]]
    jit <- if (!is.null(tmp)) 
        tmp
    else 0.05
    outdf <- x$outdf
    macol <- which(names(outdf) == "meanAcc")
    outdf <- outdf[, 1:macol]
    if (jit > 0) {
        nc <- ncol(outdf)
        for (i in 1:(nc - 3)) {
            dfCol <- outdf[, i]
            if (is.numeric(dfCol)) {
                rng <- max(dfCol) - min(dfCol)
                outdf[, i] <- dfCol + jit * rng * stats::runif(length(dfCol), 
                  -0.5, 0.5)
            }
        }
    }
    if (col == "smoothed") 
        outdf$meanAcc <- NULL
    else outdf$smoothed <- NULL
    if (disp != 0) {
        nc <- ncol(outdf)
        if (abs(disp) < nc - 1) 
            stop("disp too small")
        ord <- order(outdf[, nc], decreasing = (disp > 0))
        outdf <- outdf[ord[1:abs(disp)], ]
    }
    nr <- nrow(outdf)
    cdparcoord::discparcoord(outdf, k = nr, differentiate = TRUE)
}

# forms a list of the rows of a data frame

# returns an R list of the rows of 'data'

getRows <- function(data) split(data,1:nrow(data))

# finds the matches, in terms of row numbers, of rows in d1 in rows of
# d2; assumes each of the former has exactly one match in the latter

# warning: uses character representations; conceivably two R factor
# types could map to the same character representations

rowMatch <- function(d1,d2) 
{
   charD1 <- apply(d1,1,function(rw) paste(rw,collapse=' '))
   charD2 <- apply(d2,1,function(rw) paste(rw,collapse=' '))
   g <- function(x) match(x,charD2)
   g(charD1)
}

# generates a "superfactor" from individual ones; e.g. if factors f1 and
# f2 have n1 and n2 levels, the output is a new factor with n1 * n2
# levels

cartesianFactor <- function(dataName,factorNames,fNameSep='.')
{
   dta <- get(dataName)
   # form list of levels of each factor{
   theLevels <- lapply(factorNames,
      function(fName) {
         cmd <- paste0("levels(dta[['",fName,"']])")
         evalr(cmd)
      }
   )
   # form cartesian product of the levels, with one row for each
   # combination of levels, i.e. n1 * n2 rows as in the comment above
   superLevels <- expand.grid(theLevels)
   # don't mistake 1 for '1'
   for (i in 1:ncol(superLevels))
      superLevels[,i] <- as.character(superLevels[,i])
   subData <- dta[,factorNames]
   subDataLevels <- rowMatch(subData,superLevels)
   combNames <- function(nms) paste0(nms,collapse='.')
   newvec <- 
      sapply(subDataLevels,function(sdl) combNames(superLevels[sdl,]))
   as.factor(newvec)

}

# after fitting a qeML model on a data frame dta, saving the result in
# z, say we now want to predict the Y value of a new case x; x must be
# of the same R modes as rows of dta, e.g. numeric, character and R
# factor variables; this information must be retrieved from dta, which
# is the goal of this function here

# x is specified as an R list of column name/value pairs, one for each
# column of dta

# to save typing in specifying x, the user can say, "Give me a row like
# that of dta[dtaRowNum,] but with some elements changed according to
# what I specify in x

newDFRow <- function(dta,yName,x,dtaRowNum=1) 
{
   # remove yName column
   ycol <- which(names(dta) == yName)
   dtaX <- dta[,-ycol]
   # use row 1 as a skeleton having the right R modes
   tmp <- dtaX[dtaRowNum,]  # eventually will be our output
   # now replace
   for (nm in names(x)) {
      dtaCol <- dta[[nm]]
      fX <- is.factor(dtaCol)
      if (!fX) tmp[[nm]] <- x[[nm]]
      else {
         newDtaVal <- x[[nm]]
         newDtaVal <- factor(newDtaVal,levels(dta[[nm]]))
         tmp[[nm]] <- newDtaVal
      }
   }
   tmp
}
