
qeFindShowStoppers <- function(x,yName,yesYVal=NULL) 
{
   checkForNonDF(x)

   yCol <- which(names(x) == yName)
   y <- x[,yCol]
   x <- x[,-yCol]

   # restrict to factors
   factors <- which(sapply(x,is.factor))
   if (length(factors) ==0) stop('no factor variables present')
   xF <- x[,factors]

   xF <- factorsToDummies(xF,omitLast=F)
   xF <- as.data.frame(xF)
   # xF now all dummies

   # for dichotomous Y, change to 0,1 values
   classif <- is.factor(y)
   if (classif) {
      if (is.null(yesYVal)) stop('must specify yesYVal')
      y <- as.integer(y == yesYVal)
   }

   # the basic op
   checkShowStopper <- function(xcolName) 
   {
    
      xcol <- xF[[xcolName]]
      result <- mean(xcol)
      rareYs <- y[xcol == 1]
      nonrareYs <- y[xcol == 0]
      # difference in mean Ys
      serr <- sqrt((var(rareYs) + var(nonrareYs))/nrow(xF))
      c(result,mean(rareYs),mean(nonrareYs),serr)
   }

   browser()
   w <- t(sapply(names(xF),checkShowStopper))
   w <- as.data.frame(w)
   names(w) <- c('rareness','mean Y rare X','mean Y nonrare X','std. err.')
   w[order(w$rareness),]

}

