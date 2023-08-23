
# apply FOCI to prediction of a categorical Y; works by calling FOCI on
# each Y level separately

# return value is either an R list, one vector of selected variable
# names per element, of the union of these vectors; the latter is
# motivated by finding variables that predict SOME of Y

qeFOCImult <- function (data,yName,numCores=parallel::detectCores(), 
   parPlat="none",coalesce='union')

{

   ycol <- which(names(data) == yName)
   y <- data[,ycol]

   do1Ylevel <- function(ylevel) 
   {
      newY <- as.factor(y == ylevel)
      data[,ycol] <- newY
      fociout <- qeFOCI(data,yName,numCores=parallel::detectCores(), 
         parPlat="none",yesYLevel='TRUE')
      fociout$selectedVar$names
   }

   res <- lapply(levels(y),do1Ylevel)
   if (coalesce == 'union') res <- Reduce(union,res)
   res

}

