
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
   plot(ump[,1:2],col=classVar)
}

# plotting residuals against predictors, two at a time

# qeOut: return object from the various 

plotPairedResids <- function(data,qeOut) 
{
   yName <- qeOut$yName
   yCol <- which(names(data) == yName)  
   x <- data[,-yCol,drop=FALSE
   namesX <- names(x)
}
