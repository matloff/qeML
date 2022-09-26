
# wrapper for the FOCI package

# addition, NM, 4/28/2022: return not just the FOCI output but also the
# modified version of 'data', consisting of only the columns suggested
# by FOCI (and Y)

qeFOCI <- function(data,yName,
   numCores=parallel::detectCores(),parPlat="none",yesYLevel=NULL)
{
   getSuggestedLib('FOCI')
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   if (!is.numeric(y)) {
      if (is.factor(y) && !is.null(yesYLevel)) {
         y <- as.integer(y == yesYLevel)
      } else stop('Y must be numeric, or factor with yesYLevel defined')
   }
   x <- data[,-ycol]
   if (!allNumeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
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

# qeFOCIrand() runs qeFOCI() nXSets times, on random subsets of the
# predictors of size xSetSize, outputting the union of the selected variables

# this may be useful in datasets with lots of NAs; qeFOCI() does
# na.exclude(), possibly resulting in a very large reduction in the 
# dataset; taking small subsets of the predictors avoids this problem, especially if in the end one does use only a few of the predictors

qeFOCIrand <- function(data,yName,xSetSize,nXSets)
{
   n <- nrow(data)
   allXIdxs <- which(names(data) != yName)
   nX <- length(allXIdxs)
   xSetIdxs <- t(replicate(nXSets,{sample(allXIdxs,xSetSize)}))
   yIdx <- which(names(data) == yName)
   doOneFOCI <- function(i) 
   {   xIdxs <- xSetIdxs[i,]
       fociOut <- qeFOCI(data[,c(xIdxs,yIdx)],yName)
       selectedVars <- fociOut$selectedVar$names
       steps <- fociOut$stepT
       data.frame(selectedVars=selectedVars,steps=steps)
   }
   lapply(1:nrow(xSetIdxs),doOneFOCI)

}

