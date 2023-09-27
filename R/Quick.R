

################################################################## 
##################  qe-series of ML wrappers  #################### 
################################################################## 

# this qe*() series is inspired ggplot2::qplot; here 'qe' is for
# "quick-explore"

# the functions provide wrappers with a uniform interface, for
# quick and convenient fitting and prediction; for any given method, 
# not all options are offered in the wrapper version

# intended for quick exploration only;l sophisticated use
# should use the original functions, not these

# each has a predict() method, again with a fairly uniform interface

# some have plot() methods

# qe*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    possible algorithm-specific options
#    allDefaults:  if TRUE, take all the defaults of the wrapped
#       function, e.g. e1071::svm()
#    holdout:  size of holdout set, if any

# value:

#    see individual functions below

# predict() arguments:

#    object:  output from q*()
#    newx:  data frame of points to be predicted
#    possible options
 
# value:  R list with components as follows:
 
#    classification case:

#       ypreds:  R factor instance of predicted class labels, one element f
#          for each row of newx 
#       conditprobs:  vector/matrix of class probabilities; in the 2-class
#          case, a vector, the probabilities of Y = 1
 
#    regression case:

#       vector of predicted values

# a note on predictors/features that are R factors:

# In e.g. lm(), suppose some of the predictors are factors. Internally
# lm() will convert these to dummy variables, alleviating the user of
# that burden.  However, it can create problems in using the fitted
# model to predict new cases, say newx.

# Here newx must be a data frame (even if it has only one row), with
# column names matching those of the the training data frame.  But the
# types must match too, and in particular, for factors the levels must
# match.  This can be tricky.

# Our solution here is to have the qe*() functions include one row of
# the input data in the output object; the utility getRow1() does this.
# Then in the paired predict.qe*(), we call another utility,
# setTrainFactors() to make the factor levels match.  This is done by
# temporarily tacking newx onto the saved input row, resulting in a data
# frame that preserves the structure of the original data, then deleted
# that saved row.

####################  the qe*() functions  #######################

#######################  qeLogit()  ##############################

# qeLogit: generate estimated regression functions

# arguments:  see above

# value:

#    list of glm() output objects, one per class, and some misc.

qeLogit <- 
   function(data,yName,holdout=floor(min(1000,0.1*nrow(data))),yesYVal=NULL)
{
   yNameSave <- yName
   data <- stats::na.exclude(data)
   checkForNonDF(data)
   dataY <- data[[yName]]
   classif <- is.factor(dataY)
   if (!classif) {print('for classification problems only'); return(NA)}
   yLevels <- levels(dataY)
   if (classif && length(yLevels == 2))
      if (is.null(yesYVal)) 
         yesYVal <- yLevels[1]
   yLevels <- levels(dataY)
   if(length(yLevels) == 2) {
      whichYes <- which(yLevels == yesYVal)
      noYVal <- yLevels[3 - whichYes]
   }
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   xyc <- getXY(data,yName,classif=TRUE,makeYdumms=TRUE) 
   xy <- xyc$xy
   x <- xyc$x
   yDumms <- xyc$yDumms
   y <- xyc$y
   classNames <- xyc$classNames
   ncxy <- ncol(xy)
   nx <- ncol(x)
   nydumms <- ncxy - nx
   # check for 2-class case
   if (nydumms == 2) {
      tmp <- 
         if (!is.null(holdout)) dataY[-holdIdxs] else dataY
      # tmp is an R factor
      tmp <- ifelse(tmp == yesYVal,1,0)
      yDumms <- data.frame(y=tmp)
      empirClassProbs <- mean(tmp)
   } else {
      empirClassProbs <- colMeans(yDumms)
   }
   outlist <- 
      list(x=x,y=y,classNames=classNames,empirClassProbs=empirClassProbs)
   # One-vs-All setup, with doGlm() applied to each subanalysis
   doGlm <- function(colI) 
   {
      tmpDF <- cbind(x,yDumms[,colI])
      names(tmpDF)[nx+1] <- 'yDumm'
      glmout <- glm(yDumm ~ .,data=tmpDF,family=binomial)
   }
   nSubanalyses <- 
      if (nydumms == 2) 1 else nydumms
   outlist$glmOuts <- lapply(1:nSubanalyses,doGlm)
   outlist$classif <- classif
   outlist$trainRow1 <- getRow1(data,yName)
   outlist$nClasses <- nydumms
   outlist$yesYVal <- yesYVal
   if (nydumms == 2) outlist$noYVal <- noYVal
   class(outlist) <- c('qeLogit')
   if (!is.null(holdout)) {
      predictHoldout(outlist)
      outlist$holdIdxs <- holdIdxs
   }
   outlist$yName <- yNameSave
   outlist
}


# predict.qeLogit: predict Ys from new Xs

# arguments:  see above

# value:  object of class 'qeLogit'; see above for components
 
predict.qeLogit <- function(object,newx,...) 
{
   newx <- setTrainFactors(object,newx)
   # get probabilities for each class
   glmOuts <- object$glmOuts
   g <- function(glmOutsElt) predict(glmOutsElt,newx,type='response') 
   probs <- sapply(glmOuts,g)
   if (is.vector(probs)) probs <- matrix(probs,nrow=1)

   # if > 2 classes, separate logits for the m classes will not
   # necessrily sum to 1, so normalize

   if (object$nClasses > 2) {
      sumprobs <- apply(probs,1,sum)  
      probs <- (1/sumprobs) * probs
      classNames <- object$classNames
      colnames(probs) <- classNames
      predClasses <- apply(probs,1,which.max) 
      predClasses <- classNames[predClasses]
   } else {
      predClasses <- ifelse(probs >= 0.5,object$yesYVal,object$noYVal)
   }
   list(predClasses=predClasses,probs=probs)
}

#######################  qeMultniomLogit()  ##############################

### qeMultinomLogit <- function(data,yName,holdout=floor(min(1000,0.1*nrow(data))))
### {
###    require(mlogit)
###    classif <- is.factor(data[[yName]])
###    if (!classif) stop('for classification problems only')
###    if (!is.null(holdout)) splitData(holdout,data)
###    frml <- paste0(yName,' ~ .')
###    frml <- as.formula(frml)
###    mnlogout <- mlogit(frml,data)
###    if (classif) mnlogout$classNames <- classNames
###    mnlogout$classif <- classif
###    mnlogout$trainRow1 <- trainRow1
###    class(mnlogout) <- c('qeMultinomLogit',class(mnlogout))
###    if (!is.null(holdout)) {
###       predictHoldout(knnout)
###       mnlogout$holdIdxs <- holdIdxs
###    } else mnlogout$holdIdxs <- NULL
###    mnlogout
### }

#######################  qeLin()  ################################

# in regression case, simply wraps ordinary lm()

# in classification case, uses multivariate (i.e. vector Y) lm() for
# classification; faster than glm(), and may be useful as a rough tool
# if the goal is prediction, esp. if have some quadratic terms, which
# would make the linear approximation better 

# arguments:  see above
# value:  object of class 'qeLin' -- lm() output object, plus misc.

qeLin <- function(data,yName,noBeta0=FALSE,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   checkForNonDF(data)
   classif <- is.factor(data[[yName]])
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   if (classif) {
      xyc <- getXY(data,yName,classif=TRUE,makeYdumms=TRUE)
      xy <- xyc$xy
      classNames <- xyc$classNames
      # check for numeric class names
      checkNumericNames(classNames)
      yNames <- paste0(classNames,collapse=',')
   } else {
      xy <- data
      yNames <- yName
   }
   minus1 <- if (noBeta0) ' -1' else ''
   cmd <- paste0('lmout <- lm(cbind(',yNames,') ~ .',minus1,',data=xy)')
   eval(parse(text=cmd))
   lmout$classif <- classif 
   lmout$trainRow1 <- getRow1(data,yName)
   class(lmout) <- c('qeLin',class(lmout))
   if (!is.null(holdout)) {
      ycol <- preds <- NULL  # for CRAN "unbound globals" complaint
      predictHoldout(lmout)
      lmout$holdIdxs <- holdIdxs
      if (!classif) {
         summ <- summary(lmout)
         lmout$R2 <- summ$r.squared
         lmout$adjR2 <- summ$adj.r.squared
         lmout$holdoutR2 <- cor(preds,tst[,ycol])^2
      }
   }
   lmout$yName <- yNameSave
   lmout
}


# arguments:  see above

# value:  see above

predict.qeLin <- function(object,newx,useTrainRow1=TRUE,...) {
   class(object) <- class(object)[-1]
   if (useTrainRow1) newx <- setTrainFactors(object,newx)
   preds <- predict(object,newx)
   if (!object$classif) return(preds)
   probs <- pmax(preds,0)
   probs <- pmin(probs,1)
   if (is.vector(probs)) probs <- matrix(probs,nrow=1)
   probsums <- apply(probs,1,sum)
   probs <- probs * 1/probsums
   predClasses <- apply(preds,1,which.max)
   predClasses <- colnames(preds)[predClasses]
   list(predClasses=predClasses,probs=probs)
}

#########################  qeKNNtmp()  #################################

# will be revised qeKNN

# seems to work in all 3 cases: regression, classif binary Y, classif
# categorical Y; need to revise predict()

# arguments:  see above, plus

#     k: number of nearest neighbors
#     scaleX: if TRUE, features will be centered and scaled; note that
#        this means the features must be numeric
#     smoothingFtn: as in kNN(); 'mean' or 'loclin'
#     expandVars,expandVals:  e.g. expandVars element = 3 and
#        expandVals = 0.2 means give variable 3 a weight of 0.2
#        instead of 1.0 in the distance function

# value:  see above

# see note in kNN() man pg
 
qeKNN <- function(data,yName,k=25,scaleX=TRUE,
   smoothingFtn=mean,yesYVal=NULL,expandVars=NULL,expandVals=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol]
   origY <- y

   # housekeeping for classification case
   if (is.factor(y)) {
      classif <- TRUE
      yLevels <- levels(y)
      classif2 <- length(yLevels) == 2
      if (classif2) {
         if (is.null(yesYVal)) 
            yesYVal <- yLevels[1]
         whichYes <- which(yLevels == yesYVal)
         noYVal <- yLevels[3 - whichYes]
         ### y <- as.integer(y == yesYVal)
      } else noYVal <- NULL
   } else {
      noYVal <- NULL
      classif <- FALSE
      classif2 <- FALSE
   }
   
   if (classif) {
      if (classif2) {
         yToAvg <- as.integer(y == yesYVal)
         nYcols <- 1
      }
       else  {
         yToAvg <- regtools::factorsToDummies(y)
         nYcols <- ncol(yToAvg)
      }
   } else {
      yToAvg <- y
      nYcols <- 1
   }

   if (!is.numeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo') 
   } else factorsInfo <- NULL

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      # splitData(holdout,newData); no, write separate code for qeKNN
      nHold <- holdout
      # cat('holdout set has ',nHold, 'rows\n')
      holdIdxs <- sample(1:nrow(x),nHold)

      xTst <- x[holdIdxs,]
      x <- x[-holdIdxs,]
      if (classif2 || !classif) {
         yTst <- yToAvg[holdIdxs]
         yToAvg <- yToAvg[-holdIdxs]
      }
      else {
         yTst <- yToAvg[holdIdxs,]
         yToAvg <- yToAvg[-holdIdxs,]
      }
      tst <- cbind(xTst,yTst)
      tst <- as.data.frame(tst)
      trn <- cbind(x,yToAvg)
      if (classif && !classif2) ycol <- (ncol(x)+1):(ncol(x)+nYcols)
      else ycol <- ncol(trn)
   } 
   # if holdout, x,y are now the training set
   
   xm <- as.matrix(x)

   if (scaleX) {
      xm <- scale(xm)
      ctr <- attr(xm,'scaled:center')
      scl <- attr(xm,'scaled:scale')
      scalePars <- list(ctr=ctr,scl=scl)
   } else scalePars <- NULL
  
   if (!is.null(expandVars)) {
      # convert expandVars, expandVals according to possible creation of
      # dummies
      dta <- xm[,expandVars,drop=FALSE]
      dta <- rbind(expandVals,dta)
      dta <- as.data.frame(dta)
      tmp <- regtools::factorsToDummies(dta,omitLast=TRUE)
      expandVars <- colnames(tmp)
      expandVals <- tmp[1,]
      # convert expandVars from names to column numbers (not efficient, but
      # quick anyway)
      for (i in 1:length(expandVars)) {
         j <- which(expandVars[i] == colnames(x))
         expandVars[i] <- j
      }
      # col numbers are character strings, change to numbers
      expandVars <- as.numeric(expandVars)
   
      xm <- newMultCols(xm,expandVars,expandVals)
   }

   # set scaleX to FALSE; scaling, if any, has already been done
   knnout <- regtools::kNN(xm,yToAvg,newx=NULL,k,scaleX=FALSE,classif=classif,
      smoothingFtn=smoothingFtn)
   knnout$classif <- classif
   knnout$classif2 <- classif2
   knnout$yesYVal <- yesYVal
   knnout$noYVal <- noYVal
   knnout$scalePars <- scalePars
   knnout$factorsInfo <- factorsInfo
   knnout$trainRow1 <- trainRow1
   if (!is.null(expandVars)) {
      knnout$expandVars <- expandVars
      knnout$expandVals <- expandVals
   }
   # class(knnout) <- c('qeKNNtmp','kNN')
   class(knnout) <- c('qeKNN','kNN')
   if (!is.null(holdout)) {
      yName <- 'yTst'
      predictHoldoutKNN(knnout)
      knnout$holdIdxs <- holdIdxs
   } else knnout$holdIdxs <- NULL
   knnout$yName <- yNameSave
   knnout
}

predict.qeKNN <- function(object,newx,newxK=1,...)
{
   class(object) <- 'kNN'
   if (!regtools::allNumeric(newx)) newx <- setTrainFactors(object,newx)
   classif <- object$classif
   classif2 <- object$classif2

   if (!is.numeric(newx) && !is.null(object$factorsInfo)) 
      newx <- regtools::factorsToDummies(newx,omitLast=TRUE,object$factorsInfo)

   if (is.data.frame(newx)) newx <- as.matrix(newx)

   if (is.vector(newx)) {
      nr <- 1
   } else {
      nr <- nrow(newx)
   } 
   newx <- matrix(newx,nrow=nr)

   if (!is.null(object$scalePars)) {
      ctr <- object$scalePars$ctr
      scl <- object$scalePars$scl
      newx <- scale(newx,ctr,scl)
   }

   if (!is.null(object$expandVars)) 
      newx <- regtools::multCols(newx,object$expandVars,object$expandVals)

   preds <- predict(object,newx,newxK)

   if (!object$classif) return(preds)

   if (object$classif2) {
      probs <- preds
      predClasses <- round(probs) 
      yesYVal <- object$yesYVal
      noYVal <- object$noYVal
      predClasses[predClasses == 1] <- yesYVal
      predClasses[predClasses == 0] <- noYVal
      probs <- as.vector(probs)
      predClasses <- as.vector(predClasses)
      return(list(predClasses=predClasses,probs=probs))
   }

   # multiclass case
   predClassIdxs <- apply(preds,1,which.max) 
   predClasses <- colnames(preds)[predClassIdxs]
   list(predClasses=predClasses,probs=preds)

}

### #########################  qeKNN()  #################################
### 
### # arguments:  see above, plus
### 
### #     k: number of nearest neighbors
### #     scaleX: if TRUE, features will be centered and scaled; note that
### #        this means the features must be numeric
### #     smoothingFtn: as in kNN(); 'mean' or 'loclin'
### #     expandVars,expandVals:  e.g. expandVars element = 3 and
### #        expandVals = 0.2 means give variable 3 a weight of 0.2
### #        instead of 1.0 in the distance function
### 
### # value:  see above
### 
### # see note in kNN() man pg
###  
### qeKNN <- function(data,yName,k=25,scaleX=TRUE,
###    smoothingFtn=mean,yesYVal=NULL,expandVars=NULL,expandVals=NULL,
###    holdout=floor(min(1000,0.1*nrow(data))))
### {
###    checkForNonDF(data)
###    trainRow1 <- getRow1(data,yName)
###    classif <- is.factor(data[[yName]])
###    if (classif) {
###       y <- data[,yName]
###       yLevels <- levels(y)
###       if (length(yLevels) > 2)
###          stop('use regtools::kNN directly for multiclass case')
###       if (is.null(yesYVal)) 
###          stop('must specify yesYVal')
###       whichYes <- which(yLevels == yesYVal)
###       noYVal <- yLevels[3 - whichYes]
###    } else noYVal <- NULL
### 
###    ycol <- which(names(data) == yName)
###    if (classif) data[,ycol] <- as.integer(data[,ycol] == yesYVal)
### 
###    holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
###    if (!is.null(holdout)) {
###       splitData(holdout,data)
###       y <- data[-holdIdxs,ycol]
###       x <- data[-holdIdxs,-ycol]
###    } else {
###       x <- data[,-ycol]
###       y <- data[,ycol]
###    }
###    # if holdout, x,y are now the training set
###    
###    if (!allNumeric(x)) {
###       x <- regtools::factorsToDummies(x,omitLast=TRUE)
###       factorsInfo <- attr(x,'factorsInfo') 
###    } else factorsInfo <- NULL
### 
###    xm <- as.matrix(x)
### 
###    if (scaleX) {
###       xm <- scale(xm)
###       ctr <- attr(xm,'scaled:center')
###       scl <- attr(xm,'scaled:scale')
###       scalePars <- list(ctr=ctr,scl=scl)
###    } else scalePars <- NULL
###   
###    if (!is.null(expandVars)) {
###       # convert expandVars, expandVals according to possible creation of
###       # dummies
###       dta <- xm[,expandVars,drop=FALSE]
###       dta <- rbind(expandVals,dta)
###       dta <- as.data.frame(dta)
###       tmp <- regtools::factorsToDummies(dta,omitLast=TRUE)
###       expandVars <- colnames(tmp)
###       expandVals <- tmp[1,]
###       # convert expandVars from names to column numbers (not efficient, but
###       # quick anyway)
###       for (i in 1:length(expandVars)) {
###          j <- which(expandVars[i] == colnames(x))
###          expandVars[i] <- j
###       }
###       # col numbers are character strings, change to numbers
###       expandVars <- as.numeric(expandVars)
###    
###       xm <- newMultCols(xm,expandVars,expandVals)
###    }
### 
###    # set scaleX to FALSE; scaling, if any, has already been done
###    knnout <- regtools::kNN(xm,y,newx=NULL,k,scaleX=FALSE,classif=classif,
###       smoothingFtn=smoothingFtn)
###    knnout$classif <- classif
###    knnout$yesYVal <- yesYVal
###    knnout$noYVal <- noYVal
###    knnout$scalePars <- scalePars
###    knnout$factorsInfo <- factorsInfo
###    knnout$trainRow1 <- trainRow1
###    if (!is.null(expandVars)) {
###       knnout$expandVars <- expandVars
###       knnout$expandVals <- expandVals
###    }
###    class(knnout) <- c('qeKNN','kNN')
###    if (!is.null(holdout)) {
###       predictHoldout(knnout)
###       knnout$holdIdxs <- holdIdxs
###    } else knnout$holdIdxs <- NULL
###    knnout
### }

newMultCols <- function (x,cols,vals) {
   partx <- x[,cols,drop=FALSE]
   nvals <- length(vals)
   x[,cols] <- partx %*% diag(vals,nrow=nvals,ncol=nvals)
   x
}

### predict.qeKNN <- function(object,newx,newxK=1,...)
### {
###    class(object) <- 'kNN'
###    if (!regtools::allNumeric(newx)) newx <- setTrainFactors(object,newx)
###    classif <- object$classif
### 
###    if (!is.null(object$factorsInfo)) 
###       newx <- regtools::factorsToDummies(newx,omitLast=TRUE,object$factorsInfo)
### 
###    if (is.data.frame(newx)) newx <- as.matrix(newx)
### 
###    if (is.vector(newx)) {
###       nr <- 1
###    } else {
###       nr <- nrow(newx)
###    } 
###    newx <- matrix(newx,nrow=nr)
### 
###    if (!is.null(object$scalePars)) {
###       ctr <- object$scalePars$ctr
###       scl <- object$scalePars$scl
###       newx <- scale(newx,ctr,scl)
###    }
### 
###    if (!is.null(object$expandVars)) 
###       newx <- regtools::multCols(newx,object$expandVars,object$expandVals)
### 
###    preds <- predict(object,newx,newxK)
### 
###    if (!object$classif) return(preds)
### 
###    probs <- preds
###    predClasses <- round(probs) 
###    yesYVal <- object$yesYVal
###    noYVal <- object$noYVal
###    predClasses[predClasses == 1] <- yesYVal
###    predClasses[predClasses == 0] <- noYVal
###    list(predClasses=predClasses,probs=probs)
### }

#########################  qeRF()  #################################

# random forests, from the package 'randomForest'

# arguments:  see above, plus

#     ntree: number of trees to generate
#     minNodeSize: minimum number of data points in a node
#     mtry: number of variables randomly tried at each split

# value:  see above
 
qeRF <- function(data,yName,nTree=500,minNodeSize=10,
   mtry=floor(sqrt(ncol(data)))+1,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   checkForNonDF(data)
   classif <- is.factor(data[[yName]])
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   requireNamespace('randomForest')
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
   frml <- stats::as.formula(paste(yName,'~ .'))
   rfout <- randomForest::randomForest(frml,data=data,
      importance=TRUE,ntree=nTree,nodesize=minNodeSize,mtry=mtry)
   rfout$classNames <- xyc$classNames
   rfout$classif <- classif
   rfout$trainRow1 <- getRow1(data,yName)
   class(rfout) <- c('qeRF','randomForest')
   if (!is.null(holdout)) {
      predictHoldout(rfout)
      rfout$holdIdxs <- holdIdxs
   }
   rfout$yName <- yNameSave
   rfout
}

predict.qeRF <- function(object,newx,...)
{
   class(object) <- 'randomForest'
   newx <- setTrainFactors(object,newx)
   classif <- object$classif
   if (classif) {
      probs <- predict(object,newx,type='prob')
      res <- collectForReturn(object,probs)
   } else {
      res <- predict(object,newx,type='response')
   }
   res
}

plot.qeRF <- function(x,...) 
{
   genericPlot(x)
}

#########################  qeRFranger()  #################################

# if wish to specify a set of features to be less likely to be involved
# in node splitting, so specify in deweightPars, and R list.
# For instance, list(age=0.2,gender=0.5)

# All this goes into the ranger()'s 'split.select.weights' argument.
# This is done by scaling the weights in deweightPars:  Initially, all
# the weights are 1.0 except for those in deweightPars; then all is
# scaled to sum to 1.0.

qeRFranger <- function(data,yName,nTree=500,minNodeSize=10,
   mtry=floor(sqrt(ncol(data)))+1,deweightPars=NULL,
   holdout=floor(min(1000,0.1*nrow(data))),yesYVal=NULL)
{
   yNameSave <- yName
   checkForNonDF(data)
   classif <- is.factor(data[[yName]])
   # in binary Y case, change to 0,1
   ycol <- which(names(data) == yName)
   yvec <- data[,ycol]
   if (is.factor(yvec)) {
      if (length(levels(yvec)) == 2) {
         if (is.null(yesYVal)) yesYVal <- levels(yvec)[1]
         if (length(yesYVal) > 0) {
            whichYes <- which(yvec == yesYVal)
            yvec <- as.character(yvec)
            yvec[whichYes] <- '1'
            yvec[-whichYes] <- '0'
            yvec <- as.factor(yvec)
            data[,ycol] <- yvec
         }
      }
   }
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   requireNamespace('ranger')
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
   frml <- stats::as.formula(paste(yName,'~ .'))
   if (!is.null(deweightPars)) {
      dataNames <- names(data)
      yCol <- which(dataNames == yName)
      xNames <- dataNames[-yCol]
      numX <- length(xNames)
      wts <- rep(1,numX)
      names(wts) <- xNames
      deweightNames <- names(deweightPars)
      deweightVals <- unlist(deweightPars)
      wts[deweightNames] <- deweightVals
      wts <- wts / sum(wts)
      split.select.weights <- wts
   } else split.select.weights <- NULL
   rfrout <- ranger::ranger(frml,data=data,num.trees=nTree,mtry=mtry,
      importance='impurity',
      split.select.weights=split.select.weights,probability=classif,
      min.node.size=minNodeSize)
   rfrout$classNames <- xyc$classNames
   rfrout$classif <- classif
   rfrout$trainRow1 <- getRow1(data,yName)
   rfrout$yesYVal <- yesYVal
   class(rfrout) <- c('qeRFranger','ranger')
   if (!is.null(holdout)) {
      predictHoldout(rfrout)
      rfrout$holdIdxs <- holdIdxs
   }
   rfrout$yName <- yNameSave
   rfrout

}

qerfranger <- qeRFranger

predict.qeRFranger <- function(object,newx,...) 
{
   class(object) <- 'ranger'
   if (is.null(object$importance.mode)) object$importance.mode <- 'none'
   classif <- object$classif
   res <- predict(object, newx, type = "response")$predictions
   if (classif) {
       res <- collectForReturn(object,res)
   }
   res
}

#########################  qeRFgrf()  #################################

# random forests, from the package 'grf'

# arguments:  see above, plus

#     ntree: number of trees to generate
#     minNodeSize: minimum number of data points in a node

# value:  see above
 
qeRFgrf <- function(data,yName,nTree=2000,minNodeSize=5,
   mtry=floor(sqrt(ncol(data)))+1,
   ll=FALSE,lambda=0.1,splitCutoff=sqrt(nrow(data)),
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   checkForNonDF(data)
   classif <- is.factor(data[[yName]])

   ycol <- which(names(data) == yName)
   x <- data[,-ycol]

   # change factors, if any, to dummies
   if (!regtools::allNumeric(x)) {
      x <- regtools::toAllNumeric(x)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)

   # start the computation
   requireNamespace('grf')
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=classif,makeYdumms=TRUE)
   x <- as.matrix(xyc$x)
   y <- xyc$y
   if (!classif) {
      rfout <- 
         if (!ll) 
            grf::regression_forest(x,y,num.trees=nTree,min.node.size=minNodeSize,
            mtry=mtry)
         else 
            grf::ll_regression_forest(x,y,
            num.trees=nTree,min.node.size=minNodeSize,mtry=mtry,
            ll.split.lambda=lambda,ll.split.cutoff=splitCutoff)
   } else {
      lvls <- levels(y)
      ydumms <- regtools::factorToDummies(y,yName,omitLast=(length(lvls)==2))
      doGRF <- function(i) 
         if (!ll)
            regression_forest(x,ydumms[,i],
            num.trees=nTree,min.node.size=minNodeSize,
            mtry=mtry)
         else 
            ll_regression_forest(x,ydumms[,i],
            num.trees=nTree,min.node.size=minNodeSize,mtry=mtry,
            ll.split.lambda=lambda,ll.split.cutoff=splitCutoff)
      grfOuts <- lapply(1:ncol(ydumms),doGRF)
      names(grfOuts) <- colnames(ydumms)
      rfout <- list(grfOuts=grfOuts,classNames=lvls)
   }
   rfout$classNames <- xyc$classNames
   rfout$classif <- classif
   rfout$trainRow1 <- getRow1(data,yName)
   rfout$factorsInfo <- factorsInfo
   class(rfout) <- c('qeRFgrf','regression_forest')
   if (!is.null(holdout)) {
      predictHoldout(rfout)
      rfout$holdIdxs <- holdIdxs
   }
   rfout$yName <- yNameSave
   rfout
}

predict.qeRFgrf<- function(object,newx,...)
{
  newx <- setTrainFactors(object,newx)
  classif <- object$classif
  if (!regtools::allNumeric(newx)) {
     ## newx <- regtools::charsToFactors(newx)
     newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
        factorsInfo=object$factorsInfo)
  }
  if (classif) {
     grfOuts <- object$grfOuts
     # do OVA on the various classes
     getProbs <- function(grfOut) {
        tmp <- predict(grfOut,newx)
        as.matrix(tmp)[,1]
     }
     probs <- sapply(grfOuts,getProbs)

     # if newx has just one row, make it the proper matrix
     if (is.vector(probs)) {
        probs <- matrix(probs,nrow=1)
     }
     
     # special case of 2 classes; did not run grf on the other class
     if (ncol(probs) == 1) {
        tmp <- 1 - probs[,1]
        probs <- cbind(probs,tmp)
        colnames(probs)[2] <- 'other'
     }
     sumprobs <- apply(probs,1,sum)  
     probs <- (1/sumprobs) * probs
     predClasses <- apply(probs,1,which.max) 
     predClasses <- object$classNames[predClasses]
     res <- list(predClasses=predClasses,probs=probs)
     ## probs <- predict(x,newx,type='prob')
     ## res <- collectForReturn(x,probs)
  } else {
     class(object) <- 'regression_forest'
     res <- predict(object,newx)
     res <- as.matrix(res)[,1]
  }
  res
}

######################### qeSVM()  #################################

#SVM

#arguments:  see above, plus

#    gamma: scale param, e.g. sd of radial basis ftn
#    cost: the SVM "C" parameter penalizing nonseparable data
#    kernel: the ones offered by e1071::svm(), i.e. 'linear',
#       'polynomial', 'radial basis' and 'sigmoid'
#    degree: only specifiable for polynomial kernel

#value:  see above

qeSVM <- function (data, yName, gamma = 1, cost = 1, kernel = "radial",
    degree = 2, allDefaults = FALSE, holdout = floor(min(1000,
        0.1 * nrow(data))))
{
   yNameSave <- yName
   checkForNonDF(data)
    classif <- is.factor(data[[yName]])
    if (!classif) {
        print("for classification problems only")
        return(NA)
    }
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
    if (!is.null(holdout))
        splitData(holdout, data)
    requireNamespace('e1071')
    frml <- stats::as.formula(paste(yName, "~ ."))
    svmout <- if (allDefaults)
        e1071::svm(frml, data = data, probability = TRUE)
    else e1071::svm(frml, data = data, cost = cost, gamma = gamma,
        kernel = kernel, degree = degree, decision.values = TRUE,
        probability = TRUE)
    ycol <- which(names(data) == yName)
    svmout$x <- data[, -ycol, drop = FALSE]
    y <- data[, ycol]
    svmout$data <- data
    svmout$yName <- yName
    svmout$ycol <- ycol
    svmout$classNames <- levels(y)
    svmout$classif <- classif
    svmout$formula <- frml
    svmout$trainRow1 <- getRow1(data, yName)
    class(svmout) <- c("qeSVM", class(svmout))
    if (!is.null(holdout)) {
        predictHoldout(svmout)
        svmout$holdIdxs <- holdIdxs
    }
    svmout$yName <- yNameSave
    svmout
}

predict.qeSVM <- function (object, newx,...) 
{
    requireNamespace('e1071')
    class(object) <- class(object)[-1]
    newx <- setTrainFactors(object, newx)
    preds <- predict(object, newx, decision.values = TRUE,probability=TRUE)
    probs <- attr(preds,'probabilities')
    attributes(preds) <- NULL
    predClasses <- object$classNames[preds]
    res <- list(predClasses = predClasses, probs = probs)
    res
}

# plot.qeSVM <- function(object,formula) 
# {
#    classNames <- object$classNames
#    class(object) <- class(object)[-1]
#    formula <- object$formula
#    formula <- as.formula(formula)
#    plot(object,object$data,formula)
# }

#########################  qeGBoost()  #################################

# gradient boosting

# arguments:  see above, plus

#     nTree: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qeGBoost <- function(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   classif <- is.factor(data[[yName]])
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   requireNamespace('gbm')
   outlist <- list(classif=classif)
   if (classif) {   # classification case
      xyc <- getXY(data,yName,classif=classif,makeYdumms=TRUE) 
      xy <- xyc$xy
      x <- xyc$x
      yDumms <- xyc$yDumms
      y <- xyc$y
      classNames <- xyc$classNames
      nClass <- length(classNames)
      ncxy <- ncol(xy)
      nx <- ncol(x)
      nydumms <- ncxy - nx
      empirClassProbs <- colMeans(yDumms)
      outlist <- c(outlist,list(x=x,y=y,classNames=classNames,
         empirClassProbs=empirClassProbs))
      doGbm <- function(colI) 
      {
         tmpDF <- cbind(x,yDumms[,colI])
         names(tmpDF)[nx+1] <- 'yDumm'
         gbmout <- gbm::gbm(yDumm ~ .,data=tmpDF,distribution='bernoulli',
            n.trees=nTree,n.minobsinnode=minNodeSize,shrinkage=learnRate)
      }
      outlist$gbmOuts <- lapply(1:nydumms,doGbm)
   } else {   # regression case
      cmd <- paste0('gbmout <- gbm::gbm(',yName)
      cmd <- paste0(cmd,' ~ .,data=data,distribution="gaussian",')
      cmd <- paste0(cmd,'n.trees=',nTree,',')
      cmd <- paste0(cmd,'n.minobsinnode=',minNodeSize,',')
      cmd <- paste0(cmd,'shrinkage=',learnRate,')')
      gbmout <- NULL  # for CRAN "unbound globals" complaint
      eval(parse(text=cmd))
      outlist$gbmOuts <- gbmout
   }
   outlist$nTree <- nTree
   outlist$trainRow1 <- getRow1(data,yName)
   class(outlist) <- c('qeGBoost')
   if (!is.null(holdout)) {
      predictHoldout(outlist)
      outlist$holdIdxs <- holdIdxs
   }
   outlist$yName <- yNameSave
   outlist
}

# arguments:  see above
# value:  object of class 'qeGBoost'; see above for components
predict.qeGBoost <- function(object,newx,newNTree=NULL,...) 
{
   newx <- setTrainFactors(object,newx)
   gbmOuts <- object$gbmOuts
   if (is.null(newNTree)) {
      nTree <- object$nTree
   } else nTree <- newNTree
   if (object$classif) {
      # get probabilities for each class; 
      # NOTE: we have a choice of 'link' and 'response', not much
      # difference between the two; from man page, we are getting
      # probabilities, OVA ones for us
      g <- function(gbmOutsElt) 
         predict(gbmOutsElt,newx,n.trees=nTree,type='response') 
      probs <- sapply(gbmOuts,g)
      if (is.vector(probs)) probs <- matrix(probs,nrow=1)
      classNames <- object$classNames
      colnames(probs) <- classNames
      # normalize
      sumprobs <- apply(probs,1,sum)  
      probs <- (1/sumprobs) * probs
      predClasses <- apply(probs,1,which.max) 
      predClasses <- classNames[predClasses]
      res <- list(predClasses=predClasses,probs=probs)
   } else {
      res <- predict(object$gbmOuts,newx,n.trees=nTree)
   }
   class(res) <- 'qeGBoost'
   res
}

# graph to explore best number of trees

plot.qeGBoost <- function(x,...) 
{
   gbm.perf(x$gbmOuts)
}

#########################  qeLightGBoost()  #################################

# lightGBM 

# arguments:  see above, plus

#     nTree: number of trees
#     minNodeSize: minimum number of data points per tree node
#     learnRate: learning rate: 

# value:  see above
 
qeLightGBoost <- function(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   requireNamespace('lightgbm')
   classif <- is.factor(data[[yName]])
   if (classif) stop('classification cases not implemented yet')  
   ycol <- which(names(data) == yName)

   x <- data[,-ycol]
   y <- data[,ycol]
   x.save <- x
   if (!allNumeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
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
   
   # convert to lightGBM binned form 
   trnxm <- as.matrix(trnx)
   lgbData <- lightgbm::lgb.Dataset(data=trnxm,label=trny)

   outlist <- 
      list(classif=classif,factorsInfo=factorsInfo,trnx=trnx,
         trainRow1=getRow1(data,yName))

   # regression case
   
   params <- list(min_data_in_leaf=minNodeSize,learning_rate=learnRate)
   cmd <- 
    'lgbout <- lightgbm::lgb.train(params=params,data=lgbData,obj="regression",'
   cmd <- paste0(cmd,'nrounds=nTree)')
   lgbout <- NULL  # for CRAN "unbound globals" complaint
   eval(parse(text=cmd))
   outlist$lgbout <- lgbout
   
   outlist$nTree <- nTree
   outlist$trainRow1 <- data[1,-ycol]
   class(outlist) <- c('qelightGBoost')

   if (!is.null(holdout)) {
      # predictHoldout(outlist)
      if (inherits(tstx,'data.frame')) tstx <- as.matrix(tstx)
      preds <- predict(outlist$lgbout,tstx)
      outlist$holdoutPreds <- preds
      outlist$testAcc <- mean(abs(preds - tsty))
      outlist$baseAcc <- mean(abs(tsty - mean(tsty)))
      outlist$holdIdxs <- holdIdxs
   }
   outlist$yName <- yNameSave
   outlist
}

# arguments:  see above
# value:  object of class 'qeLightGBoost'; see above for components

predict.qeLightGBoost <- function(object,newx,...) 
{
   newx <- setTrainFactors(object,newx)
   newx <- regtools::factorsToDummies(newx,omitLast=TRUE,factorsInfo=object$factorsInfo)
   lgbout <- object$lgbout
   predict(lgbout,newx)
}

#########################  qeAdaBoost()  #################################

# Ada Boost

# arguments:  see above, plus

#     treeDepth: depth of tree to create
#     nRounds: number of boosting rounds to use 
#     rpartControl: see man page for 'rpart.control' (min node size etc.)

# value:  see above
 
qeAdaBoost <- function(data,yName,treeDepth=3,nRounds=100,rpartControl=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   if (!is.factor(data[[yName]])) stop('for classification problems only')
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   requireNamespace('JOUSBoost')
   outlist <- list()

   # factors to dummies, both for x and y
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=TRUE,makeYdumms=TRUE) 
   xy <- xyc$xy
   x <- xyc$x
   yDumms <- xyc$yDumms
   y <- xyc$y
   factorsInfo <- xyc$factorsInfo
   outlist$factorsInfo <- factorsInfo
   if (!is.null(factorsInfo)) attr(outlist,'factorsInfo') <- factorsInfo
   classNames <- xyc$classNames
   nClass <- length(classNames)
   ncxy <- ncol(xy)
   nx <- ncol(x)
   nydumms <- ncxy - nx
   empirClassProbs <- colMeans(yDumms)
   outlist <- c(outlist,list(x=x,y=y,classNames=classNames,
      empirClassProbs=empirClassProbs))

   # OVA, one AdaBoost op per class
   xMatrix <- as.matrix(x)
   doAdaBoost <- function(colI) 
   {
      yi <- 2 * yDumms[,colI] - 1
      adaboostout <- 
         JOUSBoost::adaboost(xMatrix,yi,tree_depth=treeDepth,n_rounds=nRounds,
         control=rpartControl)
   }
   outlist$abOuts <- lapply(1:nydumms,doAdaBoost)
    
   outlist$treeDepth <- treeDepth
   outlist$nRounds <- nRounds
   outlist$rpartControl <- rpartControl
   outlist$trainRow1 <- getRow1(data,yName)
   class(outlist) <- c('qeAdaBoost')
   outlist$classif <- TRUE
   if (!is.null(holdout)) {
      predictHoldout(outlist)
      outlist$holdIdxs <- holdIdxs
   }
   outlist$yName <- yNameSave
   outlist
}

# arguments:  see above
# value:  object of class 'qeAdaBoost'; see above for components
predict.qeAdaBoost <- function(object,newx,newNTree=NULL,...) 
{
   newx <- setTrainFactors(object,newx)
   factorsInfo <- object$factorsInfo
   if (!is.null(factorsInfo))
      xyc <- getXY(newx,NULL,TRUE,FALSE,factorsInfo)
   newx <- xyc$x
   abOuts <- object$abOuts
   if (is.null(newNTree)) {
      nTree <- object$nTree
   } else nTree <- newNTree
      # get probabilities for each class; 
      # NOTE: we have a choice of 'prob' and 'response'; the latter
      # assumes a logit model, not so good, but the latter won't work
      # well in the OVA context here
      # the OVA process
      g <- function(abOutsElt) 
         predict(abOutsElt,newx,n_tree=nTree,type='prob') 
      probs <- sapply(abOuts,g)
      if (is.vector(probs)) probs <- matrix(probs,nrow=1)
      classNames <- object$classNames
      colnames(probs) <- classNames
      # normalize
      sumprobs <- apply(probs,1,sum)  
      probs <- (1/sumprobs) * probs
      predClasses <- apply(probs,1,which.max) 
      predClasses <- classNames[predClasses]
      res <- list(predClasses=predClasses,probs=probs)
   class(res) <- 'qeAdaBoost'
   res
}

# graph to explore best number of trees

plot.qeGBoost <- function(x,...) 
{
   gbm.perf(x$gbmOuts)
}

#########################  qeNeural()  #################################

# neural networks, using TensorFlow/Keras 

# arguments:  see above, plus

#     hidden, vector of number of units per layer, numeric or string
#        (numbers sep by commas)
#     nEpoch, number of epochs
#     acts,conv,xShape:  as in krsFit

qeNeural <- function(data,yName,hidden=c(100,100),nEpoch=30,
   acts=rep("relu",length(hidden)),learnRate=0.001,conv=NULL,xShape=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   checkPkgLoaded('keras')
   yNameSave <- yName
   checkForNonDF(data)
   # for use with qeRT(), hidden could be a string
   if (is.character(hidden)) 
      hidden <- as.numeric(strsplit(hidden,',')[[1]])

   classif <- is.factor(data[[yName]])
   requireNamespace('keras')
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   ycol <- which(names(data) == yName)
   x <- data[,-ycol]
   if (!is.numeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   y <- data[,ycol]
   if (classif) {
      classNames <- levels(y)
      yFactor <- y
      y <- as.numeric(as.factor(y)) - 1
   } else {
      classNames <- NULL
      yFactor <- NULL
   }
   krsout <- regtools::krsFit(x,y,hidden,acts=acts,learnRate=learnRate,
      conv=conv,xShape=xShape,
      classif=classif,nClass=length(classNames),
      nEpoch=nEpoch)
   krsout$classif <- classif
   krsout$classNames=classNames
   krsout$factorsInfo=factorsInfo
   krsout$x <- x
   krsout$y <- y
   krsout$yFactor <- yFactor
   krsout$trainRow1 <- getRow1(data,yName)
   class(krsout) <- c('qeNeural',class(krsout))
   if (!is.null(holdout)) {
      predictHoldout(krsout)
      krsout$holdIdxs <- holdIdxs
   }
   krsout$yName <- yNameSave
   krsout
}

predict.qeNeural <- function(object,newx=NULL,k=NULL,...)
{
   class(object) <- class(object)[-1]
   newx <- setTrainFactors(object,newx)
   if (nrow(newx) == 1) {  # kludge!; Tensorflow issue
      kludge1row <- TRUE
      newx <- rbind(newx,newx)
   } else kludge1row <- FALSE
   if (!is.null(object$factorsInfo)) {
      newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   }
   preds <- predict.krsFit(object,newx)
   probs <- attr(preds,'probs')  # may be NULL
   if (kludge1row) preds <- preds[1]
   if (!object$classif) {
      preds
   } else {
      classNames <- object$classNames
      preds <- classNames[preds+1]
      if (kludge1row) probs <- probs[1,]

      origProbs <- probs
      if (!is.null(k)) {
         # not ideal, but no apparent easy way to get this during 
         # training phases
         trnScores <- predict.krsFit(object,object$x)
         trnScores <- attr(trnScores,'probs')
         newScores <- matrix(probs,ncol=length(classNames))
         probs <- knnCalib(object$yFactor,trnScores,newScores,k)
      }

      outlist <- list(predClasses=preds,probs=probs,origProbs=origProbs)
      outlist
   } 
}

### removing, to avoid CRAN issues
### 
### #########################  qeNeuralNet()  #################################
### 
### # neural networks, wrapping 'neuralnet' package 
### 
### # arguments:  see above, plus
### 
### #     hidden, vector of units per hidden layer
### 
### qeNeuralNet <- function(data,yName,hidden=c(5),
###    holdout=floor(min(1000,0.1*nrow(data))))
### {
###    classif <- is.factor(data[[yName]])
###    requireNamespace('neuralnet')
###    if (!is.null(holdout)) splitData(holdout,data)
###    ycol <- which(names(data) == yName)
###    x <- data[,-ycol]
###    if (!is.numeric(x)) {
###       x <- regtools::factorsToDummies(x,omitLast=TRUE)
###       factorsInfo <- attr(x,'factorsInfo')
###    } else factorsInfo <- NULL
###    y <- data[,ycol]
###    if (classif) {
###       classNames <- levels(y)
###       linear.output <- FALSE
###    } else {
###       classNames <- NULL
###       linear.output <- TRUE
###    }
###    frml <- paste0(yName,' ~ .')
###    data <- as.data.frame(cbind(x,y))
###    names(data)[ncol(data)] <- yName
###    nnout <- neuralnet::neuralnet(frml,data=data,hidden=hidden,
###       linear.output=linear.output)
###    nnout$classif <- classif
###    nnout$classNames=classNames
###    nnout$factorsInfo=factorsInfo
###    nnout$x <- x
###    nnout$y <- y
###    class(nnout) <- c('qeNeuralNet',class(nnout))
###    if (!is.null(holdout)) {
###       predictHoldout(nnout)
###       nnout$holdIdxs <- holdIdxs
###    }
###    nnout
### }
### 
### predict.qeNeuralNet <- function(x,newx=NULL,k=NULL)
### {
###    class(x) <- class(x)[-1]
###    ### newx <- setTrainFactors(x,newx)
###    if (!is.null(x$factorsInfo)) {
###       newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
###          factorsInfo=x$factorsInfo)
###    }
###    preds <- predict(x,newx)
###    probs <- attr(preds,'probs')  # may be NULL
###    if (!x$classif) {
###       preds
###    } else {
###       classNames <- x$classNames
###       preds <- classNames[preds+1]
### 
###       origProbs <- probs
###       if (!is.null(k)) {
###          # not ideal, but no apparent easy way to get this during 
###          # training phases
###          trnScores <- regools::predict.krsFit(x,x$x)
###          trnScores <- attr(trnScores,'probs')
###          newScores <- matrix(probs,ncol=length(classNames))
###          probs <- knnCalib(x$yFactor,trnScores,newScores,k)
###       }
### 
###       outlist <- list(predClasses=preds,probs=probs,origProbs=origProbs)
###       outlist
###    } 
### }

#########################  qePolyLin()  #################################

qePolyLin <- function(data,yName,deg=2,maxInteractDeg=deg,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   classif <- is.factor(data[[yName]])
   if (classif) {print('currently not for classification problems'); return(NA)}
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol,drop=FALSE]
   factorsInfo <- NULL; xm <- NULL  # for CRAN "unbound globals" complaint
   makeAllNumeric(x,data)
   data <- cbind(xm,y)
   data <- as.data.frame(data)
   names(data)[ncol(data)] <- yName

   requireNamespace('polyreg')
   qeout <- regtools::penrosePoly(d=data,yName=yName,deg=deg,maxInteractDeg)
   qeout$x <- x
   qeout$y <- y
   qeout$classif <- classif
   qeout$factorsInfo <- factorsInfo
   qeout$trainRow1 <- getRow1(data,yName)
   class(qeout) <- c('qePolyLin',class(qeout))
   if (!is.null(holdout)) {
      predictHoldout(qeout)
      qeout$holdIdxs <- holdIdxs
   }
   qeout$yName <- yNameSave
   qeout
}

predict.qePolyLin <- function(object,newx,...)
{  
   class(object) <- 'penrosePoly'
   if (ncol(object$x) == 1) {
      newx <- as.data.frame(newx)
      names(newx) <- names(object$x)
   }
   
   newx <- regtools::charsToFactors(newx)
   newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
      factorsInfo=object$factorsInfo)
   predict(object,newx)
}

predict.qePoly <- function() 
{
   print('use qePolyLin')
}

#########################  qePolyLASSO()  #################################

qePolyLASSO <- function(data,yName,deg=2,maxInteractDeg=deg,alpha=0,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   classif <- is.factor(data[[yName]])
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol,drop=FALSE]
   requireNamespace('polyreg')
   polyout <- polyreg::getPoly(x,deg,maxInteractDeg)
   requireNamespace('glmnet')
   glmx <- as.matrix(polyout$xdata)
   fam <- if (classif) 'multinomial' else 'gaussian'
   glmout <- glmnet::cv.glmnet(glmx,y,alpha=alpha,family=fam)
   res <- list(polyout=polyout,glmout=glmout,
      deg=deg,maxInteractDeg=maxInteractDeg,classif=classif,
      classNames=levels(y))
   class(res) <- 'qePolyLASSO'
   if (!is.null(holdout)) {
      predictHoldout(res)
      res$holdIdxs <- holdIdxs
   }
   res$yName <- yNameSave
   res
}

predict.qePolyLASSO <- function(object,newx,...)
{
   if (nrow(newx) == 1) {
       oneRow <- TRUE
       newx <- rbind(newx, newx)
   }
   else oneRow <- FALSE
   fittedPolyOut <- object$polyout
   polyout <- polyreg::getPoly(newx,deg=object$deg,
      maxInteractDeg=object$maxInteractDeg, 
      modelFormula=object$modelFormula,
      retainedNames=fittedPolyOut$retainedNames)
   fittedGlm <- object$glmout
   newx <- as.matrix(polyout$xdata)
   preds <- predict(fittedGlm,newx,type='response')
   if (object$classif) {
      probs <- preds
      maxCols <- apply(preds,1,which.max)
      predClasses <- object$classNames[maxCols]
      list(predClasses=predClasses,probs=probs)
   } else preds
}

#########################  qePolyLog()  #################################

# logit form of qePolyLin

qePolyLog <- function(data,yName,deg=2,maxInteractDeg=deg,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   ycol <- which(names(data) == yName)
   xy <- data[,c(setdiff(1:ncol(data),ycol),ycol)]
   classif <- is.factor(data[[yName]])
   if (!classif) stop('for classification problems')

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      splitData(holdout,data)
   }

   requireNamespace('polyreg')
   # if (!checkPkgVersion('polyreg','0.7'))
   #    stop('polyreg must be of version >= 1.7')
      
   qeout <- polyreg::polyFit(xy,deg,maxInteractDeg,use='glm')
   qeout$trainRow1 <- getRow1(data,yName)
   qeout$classif <- classif
   class(qeout) <- c('qePolyLog',class(qeout))
   if (!is.null(holdout)) {
      predictHoldout(qeout)
      qeout$holdIdxs <- holdIdxs
   }
   qeout$yName <- yName
   qeout
}

predict.qePolyLog <- function(object,newx,...)
{
   class(object) <- 'polyFit'
   predClasses <- predict(object,newx)
   probs <- attr(predClasses,'prob')
   attributes(predClasses) <- NULL 
   list(predClasses=predClasses, probs=probs)
}

#########################  qeLASSO()  #################################

qeLASSO <- function(data,yName,alpha=1,holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   requireNamespace('glmnet')
   ycol <- which(names(data) == yName)
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   y <- data[,ycol]
   x <- data[,-ycol]
   factorsInfo <- NULL; xm <- NULL  # for CRAN "unbound globals" complaint
   makeAllNumeric(x,data)
   
   classif <- is.factor(y)
   # if (classif) stop('currently not handling classification case')
   fam <- if (classif) 'multinomial' else 'gaussian'
   ym <- as.matrix(y)
   qeout <- glmnet::cv.glmnet(x=xm,y=ym,alpha=alpha,family=fam)
   qeout$x <- x
   qeout$y <- y
   qeout$classif <- classif
   qeout$factorsInfo <- factorsInfo
   if (classif) qeout$classNames <- levels(y)
   # index of the "best" lambda
   qeout$lambda.whichmin <- 
      which(qeout$lambda == qeout$lambda.min)
   if (!classif) {  # classification case hard due to multiple glm()'s
      # for i-th lambda value, place beta-hat in column i+1
      qeout$betaHatVecs <- as.matrix(qeout$glmnet.fit$beta)
      # when, if ever, did each variable enter?
      tmp <- apply(qeout$betaHatVecs,1,function(rw) which(rw != 0)[1])
      qeout$whenEntered <- sort(tmp)
   } else {
      qeout$betaHatVecs <- NA
      qeout$whenEntered <- NA
   }

   qeout$coefs <- coef(qeout)
   coefMatrix <- 
      if (!classif) as.matrix(qeout$coefs) else as.matrix(qeout$coefs[[1]])
   nonZeroIdxs <- which(coefMatrix != 0)
   nonZeroNames <- names(coefMatrix[nonZeroIdxs,])[-1]  # exclude beta0
   newdata <- xm[,nonZeroNames]
   newdata <- cbind(newdata,ym)
   newdata <- as.data.frame(newdata)
   names(newdata)[ncol(newdata)] <- names(data)[ycol]
   qeout$newData <- newdata

   if (!classif) {
      glmout <- qeout$glmnet.fit
      bestIdx <- which.min(glmout$lambda)
   }

   class(qeout) <- c('qeLASSO',class(qeout))
   if (!is.null(holdout)) {
      predictHoldout(qeout)
      qeout$holdIdxs <- holdIdxs
   }
   qeout$yName <- yNameSave
   qeout
}

qelasso <- qeLASSO

predict.qeLASSO <- function(object,newx,...) 
{
   class(object) <- class(object)[-1]
   newx <- regtools::charsToFactors(newx)
   newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
      factorsInfo=object$factorsInfo)

   if (!object$classif) return(predict(object,newx))
   # classif case
   classNames <- object$classNames
   tmp <- predict(object,newx,type='response')
   tmp <- tmp[,,1,drop=TRUE]
   # dropped too far?
   # if (is.vector(tmp)) tmp <- matrix(tmp,ncol=ncol(object$x))
   if (is.vector(tmp)) tmp <- matrix(tmp,nrow=1)
   colnames(tmp) <- classNames
   maxCols <- apply(tmp,1,which.max)
   predClasses <- object$classNames[maxCols]
   list(predClasses=predClasses,probs=tmp)
}

plot.qeLASSO <- function(x,...) 
{
   plot(x)
}

### to facilitate CRAN adherence, temporarily removed
### 
### #########################  qeIso()  #################################
### 
### # isotonic regression
### 
### qeIso <- function(data,yName,isoMethod='isoreg', 
###    holdout=floor(min(1000,0.1*nrow(data))))
### {
###    if (!is.null(holdout)) splitData(holdout,data)
###    ycol <- which(names(data) == yName)
###    y <- data[,ycol]
###    if (!is.numeric(y)) stop('cannot be used in classification problems')
###    if (ncol(data) > 2) stop('for single features only')
###    x <- data[,-ycol]
###    if (!is.numeric(x)) stop('x must be numeric')
###    xorder <- order(x)
###    xs <- x[xorder]
###    ys <- y[xorder]
###    if (isoMethod == 'isoreg') {
###       isout <- Iso:::isoreg(xs,ys)
###       isout$regests <- isout$yf[rank(x)]
###    } else if (isoMethod == 'pava') {
###       requireNamespace('Iso')
###    }
###    if (!is.null(holdout)) {
###       predictHoldout(isout)
###       isout$holdIdxs <- holdIdxs
###    }
###    isout$x <- x
###    isout$xs <- xs
###    isout$y <- y
###    class(isout) <- c('qeIso',class(isout))
###    isout
### }
### 
### predict.qeIso <- function(x,newx)
### {
###    # will need to know where newx is within the original x vector
###    xs <- x$xs
###    yf <- x$yf
###    idxs <- findInterval(newx,xs)
###    # could improve using interpolation
###    x$y[idxs]
### }

#########################  qePCA()  #################################

# PCA preprocess for qe*-series functions, including for prediction

# could have instead made PCA an argument in each qe*(), but this is cleaner

# the additional argument here is pcaProp, the proportion of variance desired
# for the principal components

qePCA <- function(data,yName,qeName,opts=NULL,pcaProp,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   # eventual return value
   res <- list()
   res$scaleX <- FALSE  # already scaled via prcomp()

   # error checks
   if (is.character(data)) data <- get(data)
   if (!is.character(qeName)) stop('specify function name, not the function')

   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol]
   if (!regtools::allNumeric(x)) {
      x <- regtools::toAllNumeric(x)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   res$factorsInfo <- factorsInfo
   res$classif <- is.factor(y)
   
   tmp <- regtools::doPCA(x,pcaProp)
   newData <- tmp$newData
   pcaout <- tmp$pcaout
   numPCs <- tmp$numPCs
   y <- data[[yName]]
   newData[[yName]] <- y

   # now call the request
   # we've already scaled during PCA, so don't now 
   cmd <- buildQEcall(qeName,'newData',yName,opts=opts,holdout=holdout)
   qeOut <- eval(parse(text=cmd))

   res$qeOut <- qeOut
   res$testAcc <- qeOut$testAcc
   res$baseAcc <- qeOut$baseAcc
   res$confusion <- qeOut$confusion
   res$pcaout <- pcaout
   res$numPCs <- numPCs
   res$trainRow1 <- qeOut$trainRow1
   class(res) <- 'qePCA'
   res$yName <- yNameSave
   res
}

predict.qePCA <- function(object,newx,...)
{
   class(object) <- class(object)[-1]
   if (!regtools::allNumeric(newx)) {
      # newx <- regtools::charsToFactors(newx)
      newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   }
   newx <- predict(object$pcaout,newx)
   if (is.vector(newx)) {
      newxnames <- names(newx)
      newx <- matrix(newx,nrow=1)
   } else newxNames <- colnames(newx)
   numPCs <- object$numPCs
   newx <- newx[,1:numPCs,drop=FALSE]
   newx <- as.data.frame(newx)
   colnames(newx) <- newxNames[1:numPCs]
   predict(object$qeOut,newx=newx)
}

#########################  qeUMAP()  #################################

# UMAP preprocess for selected qe*-series functions, including for prediction

# the additional argument is nComps, the number of components in the
# transformed X

qeUMAP <- function(data,yName,qeName,opts=NULL,
   holdout=floor(min(1000,0.1*nrow(data))),scaleX=FALSE,
   nComps=NULL,nNeighbors=NULL)
{
   yNameSave <- yName
   umap <- NULL  # avoid err msg later; umap actually set here:
   checkPkgLoaded('umap')
   # requireNamespace('umap')

   # eventual return value
   res <- list()
   res$scaleX <- scaleX  
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   x <- data[,-ycol,drop=FALSE]
   if (!regtools::allNumeric(x)) {
      x <- regtools::toAllNumeric(x)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   res$factorsInfo <- factorsInfo
   res$classif <- is.factor(y)
   
   # add more flexibility later
   umdf <- umap::umap.defaults
   if (!is.null(nComps)) umdf$n_components <- nComps
   if (!is.null(nNeighbors)) umdf$n_neighbors <- nNeighbors
   tmp <- umap::umap(x,config=umdf)
   UMAPnames <- paste0('U',1:ncol(tmp$layout))  # need for later use in factors
   colnames(tmp$layout) <- UMAPnames
   newDFrame <- as.data.frame(tmp$layout)
   newDFrame$y <- y

   # now call the request
   cmd <- paste0(qeName,'(newDFrame,"y",holdout=')
   cmd <- if (is.null(holdout)) 
      paste0(cmd,'NULL)') else paste0(cmd,holdout,')')
   qeOut <- eval(parse(text=cmd))

   res$qeOut <- qeOut
   res$testAcc <- qeOut$testAcc
   res$baseAcc <- qeOut$baseAcc
   res$confusion <- qeOut$confusion
   res$UMAPout <- tmp
   res$UMAPnames <- UMAPnames
   res$nComps <- nComps
   res$trainRow1 <- qeOut$trainRow1
   res$nColX <- ncol(x)
   class(res) <- 'qeUMAP'
   res$yName <- yNameSave
   res
}

predict.qeUMAP <- function(object,newx,...)
{
   class(object) <- class(object)[-1]
   if (!regtools::allNumeric(newx)) {
      ## newx <- regtools::charsToFactors(newx)
      newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   }
   if (is.vector(newx)) {
      newx <- matrix(newx,ncol=object$nColX)
   }

   newx <- predict(object$UMAPout,newx)
   newx <- as.data.frame(newx)
   # names(newx) <- object$UMAPnames
   predict(object$qeOut,newx=newx)
}

###########################  qeTS  #################################

# time series wrappers for the qe*-series, including for prediction

# the additional argument is lag, the number of recent values to use in
# predicting the next

# currently only numeric univariate time series are supported

qeTS <- function(lag,data,qeName,opts=NULL,
   holdout=floor(min(1000,0.1*length(data))))
{
   if (inherits(data,'data.frame')) {
      if (ncol(data) > 1)
         stop('multivariate time series not supported')
      # convert to vector
      data <- data[,1]
   }

   # convert to "rectangular" form
   tx <- regtools::TStoX(data,lag)
   tx <- as.data.frame(tx)
   yName <- names(tx)[ncol(tx)]

   # now call the ML function, forming the call in string form first
   cmd <- paste0(qeName,'(tx,','"',yName,'",')
   if (!is.null(opts)) cmd <- paste0(cmd,opts,',')
   cmd <- paste0(cmd,'holdout=holdout')
   cmd <- paste0(cmd,')')
   cmdout <- eval(parse(text=cmd))

   res <- list(lag=lag,cmdout=cmdout,testAcc=cmdout$testAcc)
   class(res) <- c('qeTS',class(cmdout))
   res
}

predict.qeTS <- function(object,newx,...)
{
   if (is.vector(newx)) {
      newx <- matrix(newx,nrow=1)
      newx <- as.data.frame(newx)
   } 
   lag <- object$lag
   if (ncol(newx) != lag) 
      stop('newx must have length "lag" or "lag" columns')
   predict(object$cmdout,newx)
}

# text classification

# mainly a wrapper for regtools::textToXY()

# arguments:

#    data: as in other qe* functions, but with the "Y" column
#       consisting of a character vector, with one element storing an
#       entire document in a single string
#    yName: as in other qe* functions
#    kTop: number of most-frequent words to use
#    stopWords: stop lists to use
#    qeName: qe-series function to use
#    opts: optional arguments for that function
#    holdout: as with the other qe-series functions

qeText <- function(data,yName,kTop=50,
   stopWords=tm::stopwords('english'),qeName,opts=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   yNameSave <- yName
   if (ncol(data) > 2) stop('must have only 1 text column and 1 label column')

   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (!is.factor(y)) stop('y must be an R factor')
   
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      splitData(holdout,data)
   }

   res <- list()  # ultimately, the return value

   textToXYout <- regtools::textToXY(data[,3-ycol],data[,ycol],kTop,stopWords)
   ## textToXYout$y <- data[,2]  # convert back to factor
   res$textToXYout <- textToXYout

   # form data for ML call
   qeData <- textToXYout$x
   qeData <- as.data.frame(qeData)
   qeData <- cbind(qeData,y[-holdIdxs])
   # names(qeData)[ncol(qeData)] <- yName
   ncx <- ncol(qeData) - 1
   names(qeData) <- paste0('keyword',1:ncx)
   names(qeData)[ncx+1] <- 'label'
   # yName <- names(data)[2]

   # now call the ML function, forming the call in string form first
   cmd <- paste0(qeName,'(qeData,','"label",')
   if (!is.null(opts)) cmd <- paste0(cmd,opts,',')
   cmd <- paste0(cmd,',holdout=NULL')
   cmd <- paste0(cmd,')')
   cmdout <- eval(parse(text=cmd))
   res$cmdout <- cmdout
   res$classif <- TRUE
   class(res) <- 'qeText'
   if (!is.null(holdout)) {
      predictHoldout(res)
      res$holdIdxs <- holdIdxs
   }
   res$yName <- yNameSave
   res
}

qetext <- qeText

predict.qeText <- function(object,newDocs,...) 
{
   xyout <- object$textToXYout
   if (!is.vector(newDocs)) newDocs <- as.vector(newDocs[,1])
   newDocsOut <- regtools::textToXYpred(xyout,newDocs)
   newDocsOut <- as.data.frame(newDocsOut)
   names(newDocsOut) <- paste0('keyword',1:ncol(newDocsOut))
   predict(object$cmdout,newDocsOut)
}

###################  utilities for qe*()  #########################

# see note on factor features at top of this file
setTrainFactors <- function(x,newx) 
{
   tmp <- rbind(x$trainRow1,newx)
   newx <- tmp[-1,,drop=FALSE]
   newx
}

# see note on factor features at top of this file
getRow1 <- function(data,yName) 
{
   ycol <- which(names(data) == yName)
   data[1,-ycol]
}

# some predict.qe*() functions call this for cleanup at end; see
# list() below for values; intended for settings in which the base
# algorithm returns probabilities, from which this function will
# computed predicted classes
collectForReturn <- function(x,probs) 
{
   classNames <- x$classNames
   colnames(probs) <- classNames
   predClasses <- apply(probs,1,which.max)
   predClasses <- classNames[predClasses]
   list(predClasses=predClasses,probs=probs)
}

# common code for qeLogit(), qeLin() etc. 

# preprocesses the input, returning new data frame xy, containing
# possibly new x and/or y

# x same unless xMustNumeric is TRUE and x contains factors, in which
# case x is processed by factorsToDummies 

# y changes to dummies if classif and makeYdumms is set

# if yName is null, check features only

getXY <- function(data,yName,xMustNumeric=FALSE,classif,
   factorsInfo=NULL,makeYdumms=FALSE) 
{
   if (is.vector(data) && is.null(yName)) data <- data.frame(data)
   if (!is.data.frame(data)) stop('must be a data frame')
   if (!is.null(yName)) {
      ycol <- which(names(data) == yName)
      y <- data[,ycol]
   } else y <- ycol <- NULL
   if (classif && !is.factor(y)) stop('Y must be a factor')
   if (!is.null(y)) {
      x <- data[,-ycol,drop=FALSE]
   } else x <- data
   # check for non-numeric cols in x, if necessary
   if (xMustNumeric) {
      xClasses <- regtools::getDFclasses(x)
      if (any(xClasses=='logical') || any(xClasses=='character')) {
         print('character or logical variables currently not allowed')
         print('change to factors'); return(NA)
      }
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL
   if (classif && !is.null(yName) && makeYdumms) {
      yDumms <- regtools::factorsToDummies(y,omitLast=FALSE,factorsInfo=NULL)
      classNames <- levels(y)
      colnames(yDumms) <- classNames
      xy <- cbind(x,yDumms)
   } else {
      yDumms <- NULL
      classNames <- NULL
      xy <- NULL
   }
   if (classif && !is.null(yName) && !makeYdumms) classNames <- levels(y)
   list(xy=xy,x=x,y=y,yDumms=yDumms,classNames=classNames,
      factorsInfo=factorsInfo)

}


######################  compareQE()  #############################

# arguments

#    data: as in qe*()
#    yName as in qe*()
#    qeFtnList: character vector of qe*() functions to be run
#    nReps: number of repetetions per qe*() function
#    seed: random number seed, applied to each qe*() function

# compare several qe*(data,yName,qeFtnList,nReps)!

compareQE <- function(data,yName,qeFtnList,nReps,seed=9999)
{
   nQe <- length(qeFtnList)
   meanAcc <- vector(length=nQe)
   for (i in 1:length(qeFtnList)) {
      cmd <- paste0(qeFtnList[i],'(data,yName)')
      set.seed(seed)
      ma <- replicate(nReps,eval(parse(text=cmd))$testAcc)
      meanAcc[i] <- mean(ma)
   }
   data.frame(qeFtn=qeFtnList,meanAcc=meanAcc)
}

#########################  qeFT()  #############################

# qe*(() wrapper fof fineTuning()

# arguments
#    data: as in qe*()
#    yName: as in qe*()
#    qeftn: quoted name of qe*() function to be used
#    pars: R list, with named elements specifying values of parameters
#    nTst: number of parameter combinations (see fineTuning())
#    nXval: number of cross-val folds per parameter combination (see 
#       fineTuning())

qeFT <- function(data,yName,qeftn,pars,nCombs=NULL,nTst,nXval,showProgress=TRUE)
{

   theCall <- function(dtrn,dtst,cmbi)
   {
      cmbiNames <- names(cmbi)
      qecall <- qeftn
      qecall <- paste0(qecall,'(data=data,yName=yName')
      for (i in 1:length(cmbi)) {
         tmp <- unlist(cmbi[i])
         if (!is.numeric(tmp)) {
            # make sure that a character parameter remains quoted
            tmp <- paste0('"',tmp,'"')
         }
         qecall <- paste0(qecall,',',cmbiNames[i],'=',tmp)
      }
      qecall <- paste0(qecall,',holdout=NULL)')
      qeout <- eval(parse(text=qecall))
      ycol <- which(names(dtst) == yName)
      tstY <- dtst[,ycol]
      tstX <- dtst[,-ycol]
      if (is.numeric(tstY)) {  # regression case
         preds <- predict(qeout,tstX)
         prederr <- abs(tstY - preds)
         return(mean(prederr))
      } else {  # classification case
         preds <- predict(qeout,tstX)$predClasses
         return(mean(preds != tstY))
      }
   }

   z <- regtools::fineTuning(data,pars,theCall,
      nCombs=nCombs,nTst=nTst,nXval=nXval,showProgress=showProgress)
   class(z) <- c('qeFT','tuner')
   z
}

plot.qeFT <- function(x,...) 
{
   plot.tuner(x)
}

#########################  qeDT()  #################################

# decision trees, wrapper to party::ctree(

# arguments:  see above, plus

#     alpha: threshold for p-value
#     minsplt: minimum number of data points in a node
#     minbucket: minimum number of data points in a terminal node
#     mtry: number of variables randomly tried at each split
#     maxdepth: maximum number of levels to tree

# value:  see above
 
qeDT <- function(data,yName,
   alpha=0.05,minsplit=20,minbucket=7,maxdepth=0,mtry=0,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   mincriterion <- 1 - alpha
   classif <- is.factor(data[[yName]])
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   requireNamespace('party')
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
   frml <- stats::as.formula(paste(yName,'~ .'))
   ctrl <- party::ctree_control(mincriterion=mincriterion,minsplit=minsplit,
      mtry=mtry,maxdepth=maxdepth,minbucket=minbucket)
   ctout <- party::ctree(frml,data=data,controls=ctrl)
   dtout <- list(ctout=ctout)
   dtout$classNames <- xyc$classNames
   dtout$classif <- classif
   dtout$trainRow1 <- getRow1(data,yName)
   # info on terminal nodes (tnodes) and all nodes
   whr <- ctout@where  # tnode membership for each data point
   dtout$termNodes <- sort(unique(whr))  # IDs of tnodes in tree
   dtout$termNodeMembers <- 
      split(1:nrow(data),whr)  # which data in which tnodes
   dtout$termNodeCounts <- 
      sapply(dtout$termNodeMembers,length)  # count in each tnode
   dtout$nTermNodes <- length(dtout$termNodes)  
   dtout$nNodes <- max(dtout$termNodes)
   class(dtout) <- c('qeDT','party')
   if (!is.null(holdout)) {
      predictHoldout(dtout)
      dtout$holdIdxs <- holdIdxs
   }
   dtout
}

predict.qeDT <- function(object,newx,...)
{
   ctout <- object$ctout
   newx <- setTrainFactors(object,newx)
   tmp <- predict(ctout,newx)
   if (object$classif) list(predClasses=tmp)
   else tmp
}

plot.qeDT <- function(x,...) 
{
   plot(x$ctout)
}

print.qeDT <- function(x,...) 
{
   print(x$ctout)
}

######################  qeCompare()  #############################

# compare several qe*(data,yName,qeFtnList,nReps)!

# arguments

#    data: as in qe*()
#    yName: as in qe*()
#    qeFtnList: character vector of qe*() functions to be run
#    nReps: number of repetetions per qe*() function
#    opts: R list, giving optional arguments for the qe*()
#    seed: random number seed, applied to each qe*() function

# compare several qe*(data,yName,qeFtnList,nReps)!

qeCompare <- function(data,yName,qeFtnList,nReps,opts=NULL,seed=9999)
{
   nQe <- length(qeFtnList)
   meanAcc <- vector(length=nQe)
   for (i in 1:length(qeFtnList)) {
      ftn <- qeFtnList[i]
      cmd <- paste0(ftn,'(data,yName')
      if (!is.null(opts)) {
         opt <- opts[[ftn]]
         if (!is.null(opt)) 
            cmd <- paste0(cmd,',',opt)
      }
      cmd <- paste0(cmd,')')
      cmd <- paste0(cmd,'$testAcc')
      set.seed(seed)
      ma <- replicate(nReps,eval(parse(text=cmd)))
      meanAcc[i] <- mean(ma,na.rm=TRUE)
   }
   data.frame(qeFtn=qeFtnList,meanAcc=meanAcc)
}

######################  qeROC()  #############################

# will plot ROC, print AUC; for classification problems

# arguments:

#    dataIn:  data frame that was input to a qe* ML function
#    qeOut:  return object from qe* ML functions
#    yLevelName:  name of the class to be considered positive

qeROC <- function(dataIn,qeOut,yLevelName) 
{
   if (!qeOut$classif) stop("for classification problems only")
   checkPkgLoaded('ROCR')
   holdout <- dataIn[qeOut$holdIdxs,]
   yName <- qeOut$yName
   holdY <- holdout[[yName]]
   ys <- as.numeric(holdY == yLevelName)
   if (length((ys)) == 0) stop('yLevelName not found')
   probs <- qeOut$holdoutPreds$probs
   if (is.null(probs)) stop('no holdoutPreds$probs')
   if (is.vector(probs) || nrow(probs) == 1 || ncol(probs) == 1) {
     probs <- as.vector(probs)
   } else if (yLevelName %in% colnames(probs)) {
      probs <- probs[,yLevelName]
   } else {
      colName <- paste0(yName,'.',yLevelName)
      if (colName %in% colnames(probs)) {
         probs <- probs[,colName]
      }
      else probs <- probs[,paste0('dfr.',yLevelName)]
   }

   # pROC::roc(ys,probs,plot=T)
   pred <- ROCR::prediction(probs,ys)
   perf <- ROCR::performance(pred,"tpr","fpr",colorkey.relwidth=1.0)
   alphVals <- perf@alpha.values[[1]]
   print(alphVals)
   # expand <- 1 / min(alphVals)
   # perf@alpha.values[[1]] <- expand*perf@alpha.values[[1]]
   # perf@alpha.name <- paste('Cutoffs expansion factor =',expand)
   plot(perf,colorize=TRUE)
   abline(0,1)
   qerocOut <- list(rocrOut=perf)
   tmp <- ROCR::performance(pred, measure = "auc")
   qerocOut$auc <- tmp@y.values[[1]]
   qerocOut
}

######################  qeToweranNA()  #############################

# prediction for data having NAs; see 
# https://github.com/matloff/toweranNA

#######################  qeXGBoost()()  ##############################

# XGBoost

# arguments:  see above, plus

#     nRounds: number of boosting rounds
#     params: R list of tuning parameters; see documentation fo
#        xgboost::xgboost()
 
qeXGBoost <- function(data,yName,nRounds=250,
   params=list(eta=0.3,max_depth=6,alpha=0),
   holdout=floor(min(1000,0.1*nrow(data))))
{
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

   params$objective <- objective
   if (classif) params$num_class <- length(yLevels)
   
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
   xgbOut <- xgboost::xgboost(data=xm,label=y,nrounds=nRounds,
      param=params)
   class(xgbOut) <- c('qeXGBoost','xgb.Booster')

   xgbOut$classif <- classif
   xgbOut$factorsInfo <- factorsInfo
   xgbOut$yLevels <- yLevels
   xgbOut$yName <- yName

   if (!is.null(holdout)) {
      tst[,ycol] <- tst[,ycol] + 1
      predictHoldoutXGB(xgbOut)
      xgbOut$holdIdxs <- holdIdxs
    }
   else xgbOut$holdIdxs <- NULL

   xgbOut

}

predict.qeXGBoost <- function(object,x,...) 
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

#######################  qeDeepnet()()  ##############################

# Deepnet

# using almost all available arguments from the wrappee, but not
# allowing regression cases, on which the package seems to behave
# erraticly
 
qeDeepnet <- function(data,yName,hidden=c(10),activationfun='sigm',
   learningrate=0.8,momentum=0.5,learningrate_scale=1,
   numepochs=3,batchsize=100,hidden_dropout=0,
   yesYVal=NULL,holdout=floor(min(1000,0.1*nrow(data))))
{
   checkPkgLoaded('deepnet')
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   if (!classif) stop('set up for classification problems only')
   ycol <- which(names(data) == yName)
   x <- data[,-ycol,drop=FALSE]
   y <- data[,yName]
   yLevels <- levels(y)
   y <- regtools::factorsToDummies(y,omitLast=FALSE)
   output <- 'sigm'
   
   if (!allNumeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo') 
   } else factorsInfo <- NULL

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      holdIdxs <- sample(1:nrow(data),holdout)
      tsty <- y[holdIdxs]
      tstx <- x[holdIdxs,,drop=FALSE]
      trny <- y[-holdIdxs]
      trnx <- x[-holdIdxs,,drop=FALSE]
   } else {
      trnx <- x
      trny <- y
   }
   # if holdout, x,y are now the training set

   xm <- as.matrix(x)
   nnOut <- deepnet::nn.train(x=xm,y=y,hidden=hidden, 
      activationfun=activationfun,learningrate=learningrate, 
      momentum=momentum,learningrate_scale=learningrate_scale,
      output=output,numepochs=numepochs,batchsize=batchsize, 
      hidden_dropout=hidden_dropout) 
   class(nnOut) <- c('qeDeepnet')

   nnOut$classif <- classif
   nnOut$factorsInfo <- factorsInfo
   nnOut$yLevels <- yLevels

   if (!is.null(holdout)) {
      preds <- predict(nnOut,tstx)
      nnOut$testAcc <- mean(preds$predClasses != data[holdIdxs,ycol])
      nnOut$holdIdxs <- holdIdxs
    }
   else nnOut$holdIdxs <- NULL

   nnOut

}

predict.qeDeepnet <- function(object,newx,...) 
{
   if (!allNumeric(newx)) 
      newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   else newx <- as.matrix(newx)
   probs <- deepnet::nn.predict(object,newx)
   colnames(probs) <- object$yLevels
   predClassIdxs <- apply(probs,1,which.max)
   predClasses <- object$yLevels[predClassIdxs]
   list(predClasses=predClasses,probs=probs)
}

#######################  qeNCVreg()  ##############################

# wrappers for ncvreg package, "nonconves regularization"

# qeNCVreg <- function(data,yName,
qeNCVregCV <- function(data,yName,
   family="gaussian",
   penalty="MCP",
   gamma=switch(penalty, SCAD=3.7,3),alpha=1,
   lambda.min=0.001,nlambda=100,lambda,eps=1e-04,
   max.iter=10000,
   cluster=NULL,
   nfolds=10,yesYVal=NULL,
   holdout=floor(min(1000,0.1*nrow(data)))) 
{
   checkPkgLoaded('ncvreg')
   if (!is.null(cluster)) 
      stop('currently cluster computation is not implemented')
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   if (family=='binomial' && !classif) 
      stop('binomial case needs factor Y')
   ycol <- which(names(data) == yName)
   if (classif) {
      # stop('classification case under construction')
      family <- 'binomial'
      y <- data[,yName]
      yLevels <- levels(y)
      if (length(yLevels) != 2) 
         stop('only 2-class problems are handled as of now')
      if (is.null(yesYVal)) {
         if (setequal(yLevels,c(0,1))) {
            yesYVal <- '1'
            noYVal <- '0'
         } else {
            yesYVal <- yLevels[1]
            noYVal <- yLevels[2]
         }
      } else {
         yesyval <- which(yLevels == yesYVal)
         noYVal <- yLevels[3-yesyval]
      }
      tmp <- as.integer(y == yesYVal)
      data[,yName] <- tmp
   } else {
      yLevels <- NULL
      yesYVal <- NULL
      noYVal <- NULL
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
   if (!is.null(holdout)) {
      trnx <- xm
      trny <- y
      tstx <- regtools::factorsToDummies(data[holdIdxs,-ycol],omitLast=TRUE,
         factorsInfo=factorsInfo)
      tsty <- data[holdIdxs,ycol]
   }

   cvout <- ncvreg::cv.ncvreg(xm,y,
      family=family,
      penalty=penalty,
      gamma=gamma,
      lambda.min=lambda.min,
      max.iter=max.iter,
      # cluster=cluster,
      nfolds=nfolds,
      seed=9999,
      holdout=floor(min(1000,0.1*nrow(data)))) 


   cvoutBig <- list(cvout=cvout,classif=classif,
      yesYVal=yesYVal,noYVal=noYVal)
   class(cvoutBig) <- 'qeNCVregCV'

   if (!is.null(holdout)) {
       predictHoldoutNCV(cvoutBig)
       cvoutBig$holdIdxs <- holdIdxs
   }
   else cvoutBig$holdIdxs <- NULL

   i <- which(cvout$fit$lambda == cvout$lambda.min)
   cvoutBig$finalBetaHat <- cvout$fit$beta[,i]
   cvoutBig
}

predict.qeNCVregCV <- function(object,newx,...) 
{
   if (!is.matrix(newx)) {
      newx <- regtools::factorsToDummies(newx,omitLast = TRUE, 
         factorsInfo = object$factorsInfo)
   }

   cvout <- object$cvout
   classif <- object$classif
   type <- if (!classif) 'link' else 'response'
   preds <- as.numeric(predict(cvout,newx,type=type))
   if (!object$classif) return(preds)
   tmp <- list(probs=preds)
   predYs <- round(preds)
   predClasses <- ifelse(predYs,object$yesYVal,object$noYVal)
   list(probs=preds,predClasses=predClasses)
}

plot.qeNCVregCV <- function(x,...) 
{
   plot(x$cvout)
}

qencvregcv <- qeNCVregCV


#######################  qeParallel()  ##############################

# arguments:

#    data, yName: as in the typical qe*() functions
#    qeFtnName: name of the qe*() function, e.g. 'qeRF'
#    cls: cluster in the sense of 'parallel' package; if not of class
#       'cluster', this is either a positive integer, indicating the
#       desired number of cores, or a character vector, indicating the
#       machines on which the cluster is to be formed
#    dataName: name of the 'data' argument; code will be distributed
#       across the cluster, under this name, unless 'cls' is of
#       'cluster' class, in which case the data is assumed already 
#       distributed
#    libs: list of library needing to be loaded for the qe* ftn
#    holdout: as in other qe* functions (no default here, as the data
#       is not explicitly an argument)

# needless to say, the above structures are intended to avoid wasted
# duplication; e.g. if cls already exists, don't recreate it (the data
# would also be distributed a new, unnecessarily)

qeParallel <- function(data,yName,qeFtnName,dataName,opts=NULL,cls=1,
   libs=NULL,holdout=NULL) 
{
   getSuggestedLib('partools')

   if (!inherits(cls,'cluster')) {
      cls <- parallel::makeCluster(cls)
      partools::setclsinfo(cls)
      partools::doclscmd(cls,'library(qeML)')
      newCLS <- TRUE
   } else newCLS <- FALSE

   ### data <- get(dataName)
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      splitData(holdout, data)  # trn, tst; data <- trn
   }

   if (newCLS) {
      ### assign(dataName,data)
      partools::distribsplit(cls,dataName)
   }

   ### if (length(grep('holdout=NULL',nodeCmd)) == 0)
   ###    stop('qeFtn call must include holdout=NULL, no spaces')

   nodeCmd <- paste0(qeFtnName,
                     "(",
                     dataName,
                     ',"',
                     yName,
                     '",holdout=NULL')
   if (!is.null(opts)) nodeCmd <- paste0(nodeCmd,',',opts)
   nodeCmd <- paste0(nodeCmd,')')

   clsOut <- partools::doclscmd(cls,nodeCmd)
   clsOut$cls <- cls
   clsOut$classif <- clsOut[[1]]$classif
   clsOut$libs <- libs
   class(clsOut) <- 'qeParallel'

   nClust <- length(cls)
   if (!is.null(holdout)) {

      if (!is.null(libs)) 
         for (lb in libs) getSuggestedLib(lb)

      # predict locally, not distrib; less efficient
      ycol <- which(names(tst) == yName) 
      tstx <- tst[, -ycol, drop = FALSE]
      retVals <- clsOut[1:nClust]
      if (!clsOut$classif) {
         preds <- sapply(retVals,function(cElt) predict(cElt,tstx))
         preds <- rowMeans(preds)
         clsOut$testAcc <- mean(abs(tst[,ycol] - preds))
      } else {
         preds <- lapply(retVals,function(cElt) predict(cElt,tstx)$probs)
         probsAvg <- Reduce('+',preds) / nClust
         winners <- apply(probsAvg,1,which.max)
         guesses <- colnames(probsAvg)[winners]
         clsOut$testAcc <- mean(tst[,ycol] != guesses)
      }


   }

   clsOut

}

qepar <- qeParallel

predict.qeParallel <- function(object,newx,...) 
{
   if (!is.null(object$libs)) 
      for (lb in object$libs) getSuggestedLib(lb)

   nClust <- length(object$cls)
   retVals <- object[1:nClust]
   if (!object$classif) {
      preds <- sapply(retVals,function(cElt) predict(cElt,newx))
      return(mean(preds))
   }
   # classif case
   preds <- lapply(retVals,function(cElt) predict(cElt,newx)$probs)
   probsAvg <- Reduce('+',preds) / nClust
   winners <- apply(probsAvg,1,which.max)
   colnames(probsAvg)[winners]
}

# assess feature importance in the given setting (data, yName, qeFtn) by
# fitting the model p times, leaving out one feature each time

# holdout is done nReps times for each variable; 'opts' refers to
# nondefault argument values for qeFtn; not implemented yet

qeLeaveOut1Var <- function(data,yName,qeFtnName,nReps,opts=list()) 
{
   # full model
   fullTestAcc <- get(qeFtnName)(data,yName)$testAcc
   yCol <- which(names(data) == yName)
   output <- vector(length=ncol(data)-1)
   names(output) <- names(data)[-yCol]
   j <- 1
   for (i in 1:ncol(data)) {
      if (i != yCol) {
         datai <- data[,-i]
         cmd <- sprintf('%s(datai,"%s")$testAcc',qeFtnName,yName)
         # output[j] <- replicMeans(nReps,cmd)
         tmp <- replicate(nReps,evalr(cmd))
         output[j] <- mean(tmp)
         j <- j + 1
      }
   }
   output <- c(fullTestAcc,output)
   names(output)[1] <- 'full'
   output
}

#########################  qeRpart()  #################################

# decision trees, wrapper to rpart::rpart, one of the early CART
# implementations

# arguments:  see above, plus

#     alpha: threshold for p-value
#     minsplt: minimum number of data points in a node
#     minbucket: minimum number of data points in a terminal node
#     mtry: number of variables randomly tried at each split
#     maxdepth: maximum number of levels to tree

# value:  see above
 
qeRpart <- function(data,yName,minBucket=10,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   classif <- is.factor(data[[yName]])
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) splitData(holdout,data)
   requireNamespace('rpart')
   rp <- rpart::rpart
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
   # cmd <- sprintf("rpart::rpart(%s ~.,data=data,control=list(minbucket=%d))",
   #    yName,minBucket)
   # rpout <- evalr(cmd)
   frmla <- as.formula(paste0(yName,' ~ .'))
   rpout <- rpart::rpart(frmla,data=data,control=list(minbucket=minBucket))
   rpout$classif <- classif
   rpout$trainRow1 <- getRow1(data,yName)
   if (!is.null(holdout)) {
      predictHoldout(rpout)
      rpout$holdIdxs <- holdIdxs
   }
   class(rpout) <- c('qeRpart','rpart')
   rpout
}

predict.qeRpart <- function(object,newx,...)
{
   class(object) <- class(object)[-1] 
   newx <- setTrainFactors(object,newx)
   tmp <- predict(object,newx)
   if (object$classif) list(predClasses=tmp)
   else tmp
}

plot.qeRpart <- function(x,
   boxPalette=c('red','yellow','green','blue'),...) 
{
   requireNamespace('rpart.plot')
   class(x) <- class(x)[-1] 
   rpart.plot::rpart.plot(x,box.palette=boxPalette)
}

print.qeDT <- function(x,...) 
{
   print(x$ctout)
}


