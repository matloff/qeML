
# wrapper for hal9001::fit_hal



# in regression case, simply wraps ordinary lm()

# in classification case, uses multivariate (i.e. vector Y) lm() for
# classification; faster than glm(), and may be useful as a rough tool
# if the goal is prediction, esp. if have some quadratic terms, which
# would make the linear approximation better 

# arguments:  see above
# value:  object of class 'qeLin' -- lm() output object, plus misc.

qeHAL <- function(data,yName,holdout=floor(min(1000,0.1*nrow(data))))
{
   requireNamespace('hal9001')
   yNameSave <- yName
   qeML:::checkForNonDF(data)
   classif <- is.factor(data[[yName]])
   if (classif) stop('not covered yet')

   xy <- data
   yNames <- yName
   yCol <- which(names(data) == yName)
   x <- xy[,-yCol]
   y <- xy[,yCol]
   isF <- sapply(x,is.factor)
   if (any(isF)) {
      xnum <- factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(xnum,'factorsInfo')
      xsave <- x
      x <- xnum
      data <- factorsToDummies(data,TRUE,factorsInfo)
   } else factorsInfo <- NULL
   trainRow1 <- x[1,]

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) qeML:::splitData(holdout,data)
   trn <- as.data.frame(trn)
   tst <- as.data.frame(tst)

   halout <- fit_hal(x,y)
   halout$classif <- classif 
   halout$trainRow1 <- qeML:::getRow1(data,yName)
   class(halout) <- c('qeHAL',class(halout))
   if (!is.null(holdout)) {
      ycol <- preds <- NULL  # for CRAN "unbound globals" complaint
      qeML:::predictHoldout(halout)
      halout$holdIdxs <- holdIdxs
      if (!classif) {
         summ <- summary(halout)
      }
   }
   halout$yName <- yNameSave
   halout$factorsInfo <- factorsInfo
   halout$trainRow1 <- trainRow1
   halout
}

predict.qeHAL <- function(object,newx,useTrainRow1=FALSE,...) {
   class(object) <- class(object)[-1]
   if (!is.null(object$factorsInfo)) 
      newx <- factorsToDummies(newx,TRUE,object$factorsInfo)
   if (useTrainRow1) newx <- qeML:::setTrainFactors(object,newx)

   preds <- predict(object,newx)
   if (!object$classif) return(preds)
}

