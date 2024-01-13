
# eventual replacement for regtools::replicMeans

# runs cmd nReps times, take the average; cmd must be quoted, and if it
# consists of more than one statement, must be in braces

# typical intended use is that cmd returns a matrix or data frame etc.,
# but returning a scalar does work

replicMeansMatrix <- function(nReps,cmd,nCols=NULL) 
{
   ftni <- paste0('function(i) ',cmd)
   sappCmd <- sprintf("sapply(%s,%s)",'1:nReps',ftni)
   res <- evalr(sappCmd)
   if (is.null(dim(res))) return(mean(res))
   tmp <- colMeans(t(res)) 
   matrix(tmp,ncol=nCols)
}

# examples/tests

# mean of scalar quantity
# replicMeansMatrix(100,'rnorm(1)^2')
# mean of vector quantity
# replicMeansMatrix(100,'c(rnorm(1),rnorm(1)^2)',2)
# mean of matrix quantity
# replicMeansMatrix(100,
#  '{z1=rnorm(1); z2=rnorm(1); x=z1; y=z1+z2; rbind(c(x,x^2),c(y,y^2))}',2) 

