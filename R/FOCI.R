
# wrapper for the FOCI package

qeFOCI <- function(data,yName) 
{
   requireNamespace('FOCI')
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (!is.numeric(y)) stop('only numeric Y allowed')
   x <- data[,-ycol]
   if (!allNumeric(x)) x <- factorsToDummies(x,omitLast=TRUE)
   foci(y,x)
}

