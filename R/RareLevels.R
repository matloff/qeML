


# if a predictor/feature is an R factor and some of its levels are rare,
# we might consider omitting data points with such levels; this may
# simplify our analyses, helping somewhat to avoid overfitting; however,
# such levels might on the other hand be "showstoppers" which, in the
# rare cases in which they do occur, are highly informative about Y;
# this function computes rareness and impact on Y

# example
# > qeRareLevels(pef,'wageinc')
#                 rareness Ybar, rare X Ybar, nonrare X std. err.
# occ.106       0.02493778     53639.86        60512.74  444.8219
# educ.16       0.03618716     86040.44        59376.45  576.9754
# ...

qeRareLevels <- function(x,yName,yesYVal=NULL) 
{
   checkForNonDF(x)

   yCol <- which(names(x) == yName)
   y <- x[,yCol]
   x <- x[,-yCol]

   # restrict to factors
   factors <- which(sapply(x,is.factor))
   if (length(factors) ==0) stop('no factor variables present')
   xF <- x[,factors]

   xF <- regtools::factorsToDummies(xF,omitLast=F)
   xF <- as.data.frame(xF)
   # xF now all dummies

   # for dichotomous Y, change to 0,1 values
   classif <- is.factor(y)
   if (classif) {
      if (is.null(yesYVal)) stop('must specify yesYVal')
      y <- as.integer(y == yesYVal)
   }

   # the basic op
   rareLevels <- function(xcolName) 
   {
    
      xcol <- xF[[xcolName]]
      result <- mean(xcol)
      rareYs <- y[xcol == 1]
      nonrareYs <- y[xcol == 0]
      # difference in mean Ys
      serr <- sqrt((var(rareYs) + var(nonrareYs))/nrow(xF))
      c(result,mean(rareYs),mean(nonrareYs),serr)
   }

   w <- t(sapply(names(xF),rareLevels))
   w <- as.data.frame(w)
   names(w) <- c('rareness','Ybar, rare X','Ybar, nonrare X','std. err.')
   w[order(w$rareness),]

}

