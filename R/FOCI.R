
# wrapper for the FOCI package

qeFOCI <- function(data,yName,
   numCores=parallel::detectCores(),parPlat="none")
{
   require('FOCI')
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (!is.numeric(y)) stop('only numeric Y allowed')
   x <- data[,-ycol]
   if (!allNumeric(x)) {
      x <- factorsToDummies(x,omitLast=TRUE)
      x <- as.data.frame(x)
   }
   ccx <- regtools::constCols(x)
   if (length(ccx) > 0) {
      x <- x[,-ccx]
      warning('const cols removed')
   }
   FOCI::foci(y,x,numCores=numCores,parPlat=parPlat)
}

