
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

doubleD <- function(qeFtnCall,xPts,nReps)
{
   warning('still experimental')
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
   res
}

