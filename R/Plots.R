
# misc. plots, especially for dimension reduction

plotClassesUMAP <- function(data,classVar) 
{
   if (is.character(classVar)) {  # name of a column in 'data'
      whichClass <- which(names(data) == classVar)
      x <- data[,-whichClass]
      classVar <- data[,whichClass]
   } else {
      x <- data
   }
   if (!is.factor(classVar)) classVar <- as.factor(classVar)
   if (!allNumeric(x)) x <- factorsToDummies(x)
   ump <- uwot::umap(x)
   plot(ump[,1:2],col=classVar,xlab='UMAP1',ylab='UMAP2')
}

# plotting residuals against predictors, two at a time

# qeOut: return object from the various 

plotPairedResids <- function(data,qeOut) 
{
   qo <- qeOut
   if (qo$classif) stop('not for classification problems')
   yName <- qo$yName
   yCol <- which(names(data) == yName)  
   predErrs <- data[qo$holdIdxs,yCol] - qo$holdoutPreds
   x <- data[,-yCol,drop=FALSE]
   xF <- which(sapply(x,is.factor))
   if (length(xF) > 0) x <- x[,-xF,drop=FALSE]
   x <- x[qo$holdIdxs,]
   namesX <- names(x)
   nX <- ncol(x)
   while (1) {
      ij <- sample(1:nX,2)
      i <- ij[1]; j <- ij[2]
      autoimage::autopoints(x[,i],x[,j],predErrs,xlab=namesX[i],ylab=namesX[j])
      ans <- readline('hit Enter for next plot, "q" for quit ')
      if (ans == 'q') break
   }
}
