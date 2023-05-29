
# facilitates exploration of the Double Descent phenomenon

# see

## https://matloff.wordpress.com/2020/11/11/the-notion-of-double-descent/

## https://twitter.com/matloff/status/1507935158648782856

# examples: 

#   polynomial model, varying degree; pef dataset from this package

#      cmd <- 'qePolyLin(pef,"wageinc",deg=xPts[i]`)'
#      z <- doubleD(cmd,c(1:4),100)
#      plot(z)

#   fixed model, vary n; here the data is modified from the crdtdataset
#   in the ISLR pkg

#      cmd <- 'qeLin(crd,"Balance",holdout=nrow(crd)-xPts[i])'
#      z <- doubleD(cmd,seq(50,3,-1),250)
#      plot(z)

#   for any i, the model will be fit on n = xPts[i] data points, and then
#   tested on the remaining data, the holdout

# arguments:

#    qeFtnCall: quoted string; somewhere should include 'xPts[i]'
#    xPts:  values on the X-axis showing where to run the code, e.g.
#       various values of degree of polynomial
#    nReps: number of runs of the experiment
#    makeDummies: call regtools::factorsToDummies() before running
#       the experiments (to avoid "new factor level found" when
#       predicting the test set; if used, must be set to the nam of the
#       dataset, as shown in cmd

# will plot testAcc and trainAcc, in red and blue

# example

# doubleD('qePolyLin(mlb[,2:3],"Weight",xPts[i])',1:4,100) 

# qePolyLin will be called as indicated, with degree = 1,2,3,4

doubleD <- function(qeFtnCall,xPts,nReps,makeDummies=NULL,classif=FALSE)
{
   if (!is.null(makeDummies)) {
      dta <- makeDummies
      cmd <- 
         paste0(dta,' <- regtools::factorsToDummies(',dta,
         ',omitLast=TRUE)')
      eval(parse(text=cmd))
      cmd <- paste0(dta, ' <- as.data.frame(',dta,')')
      eval(parse(text=cmd))
   }
   cmd <- paste0('tmp <- ',qeFtnCall,'; c(tmp$testAcc,tmp$trainAcc)')
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

plot.doubleD <- function(x,xLab='xPts',...) 
{
   z <- as.data.frame(x$outMatrix)
   yLab <- 'err'
   yLim <- c(0,1.1*max(z[,-1]))
   plot(loess(testAcc ~ xPts,data=z),type='l',col='red',xlab=xLab,
      ylim=yLim,ylab=yLab)
   lines(loess(trainAcc ~ xPts,data=z),col='blue')
}
   
