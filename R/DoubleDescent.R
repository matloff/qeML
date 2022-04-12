
# facilitates exploration of the Double Descent phenomenon

# see

## https://matloff.wordpress.com/2020/11/11/the-notion-of-double-descent/

## https://twitter.com/matloff/status/1507935158648782856

# arguments:

#    qeFtnCall: quoted string; somewhere should include 'xPts[i]'
#    xPts:  values on the X-axis showing where to run the code, e.g.
#       degree of polynomial
#    nReps: number of runs of the experiment

# will plot testAcc and trainAcc, in red and blue

# example: 

#    cmd <- 'qePolyLin(pef,"wageinc",deg=xPts[i]`)'
#    z <- doubleD(cmd,c(1:4),100)

doubleD <- function(qeFtnCall,xPts,nReps,classif=FALSE)
{
   warning('still experimental')
   if (classif) stop('not set for classification problems yet')
   cmd <- paste0('tmp <- ',cmd,'; c(tmp$testAcc,tmp$trainAcc)')
   res <- matrix(nrow=length(xPts),ncol=3)
   res[,1] <- xPts
   tmp <- matrix(nrow=nReps,ncol=2)
   colnames(res) <- c('xPts','testAcc','trainAcc')
   for (i in 1:length(xPts)) {
      # funny interaction error with replicMeans(); do "by hand"
      ## tmp <- replicMeans(eval(parse(text=qeFtnCall)),nReps)
      for (j in 1:nReps) {
         tmpeval <- eval(parse(text=qeFtnCall))
         tmp[j,] <- c(tmpeval$testAcc,tmpeval$trainAcc)
      }
      res[i,2:3] <- colMeans(tmp)
   }
   res <- list(outMatrix=res,qeFtnCall=qeFtnCall,xPts=xPts,nReps=nReps,
      classif=classif)
   class(res) <- 'doubleD'
   res
}

plot.doubleD <- function(obj,xLab='xPts') 
{
   z <- as.data.frame(obj$outMatrix)
   yLab <- if (obj$classif) 'OPM' else 'MAPE'
   plot(loess(testAcc ~ xPts,data=z),type='l',col='red',xlab=xLab)
   lines(loess(trainAcc ~ xPts,data=z),col='blue')
}
   
