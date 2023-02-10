
qeFindShowStoppers <- function(x,yName,yesYVal=NULL) 
{
   checkForNonDF(x)

   yCol <- which(names(x) == yName)
   y <- x[,yCol]

   # for dichotomous Y, change to 0,1 values
   classif <- is.factor(y)
   if (classif) {
      if (is.null(yesYVal)) stop('must specify yesYVal')
      y <- as.integer(y == yesYVal)
   }

   # find the dichotomous columns
   isDichot <- function(xCol) length(levels(xCol)) == 2
   dichots <- which(sapply(x,isDichot))

   # the basic op
   checkShowStopper <- function(xCol) 
   {
     lvls <- levels(xCol)
     result <- lvls  # names of the dichtomy
     w <- table(xCol)
     wm <- which.min(w)
     rarerLevel <- lvls[wm]  # name of the rarer one
     result <- c(result,rarerLevel)
     rareYs <- y[xCol == rarerLevel]
     nonrareYs <- y[xCol != rarerLevel]
     rareness <- mean(xCol == rarerLevel)
     result <- c(result,rareness)
     # difference in mean Ys
     serr <- sqrt((var(rareYs) + var(nonrareYs))/nrow(x))
     c(result,mean(rareYs),mean(nonrareYs),serr)
   }

browser()
   w <- t(sapply(x[,dichots,drop=FALSE],checkShowStopper))
   w <- as.data.frame(w)
   names(w) <- c('level 1','level 2','rarerLevel','rareness',
      'mean Y rare X','mean Y nonrare X','std. err.')
   w

}

