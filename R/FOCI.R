
# wrapper for the FOCI package

# addition, NM, 4/28/2022: return not just the FOCI output but also the
# modified version of 'data', consisting of only the columns suggested
# by FOCI (and Y)

qeFOCI <- function(data,yName,
   numCores=parallel::detectCores(),parPlat="none")
{
   getSuggestedLib('FOCI')
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
   FOCIout <- FOCI::foci(y,x,numCores=numCores,parPlat=parPlat)
   selIdxs <- FOCIout$selectedVar$index
   x <- x[,selIdxs]
   newData <- as.data.frame(cbind(x,y))
   names(newData)[ncol(newData)] <- yName
   FOCIout$newData <- newData
   FOCIout
}

