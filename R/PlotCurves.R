
###########################  qePlotCurves  #################################

# plot several curves having the same X domain; based on the inputted
# (X,Y) data; that data can be smoothed into a curve via the 'loess' 
# option, assumed here to be typical

# a legend is automatically produced, based on curveData[,3]

# example use case: 

#    plotting the -Utility or Fairness-Utility tradeoff
#    curves, for comparing several different methods

# example use case: 

#    plotting quantile regression, for several quantile levels

# arguments

#    curveData: 3-column data frame, consisting of the (X,Y) coordinates
#       and a quoted string for the name
#    xlab, ylab:  X,Y axis labels
#    loess: if TRUE, plot the loess-fitted curve, not the points
#    legendSpace: expand plot grid by this amount to fit in a legend
#    legendPos: as in R plot()

# value

#    none; this is purely a plotting routine

qePlotCurves <- function(curveData,xlab='',ylab='',loess=TRUE,
   legendSpace=1.1,legendPos='topright') 
{

   xlim <- c(min(curveData[,1]),max(curveData[,1]))
   tmp <- max(curveData[,2])
   # leave room at top for legend
   ylim <- c(min(curveData[,2]),legendSpace*tmp)
   plot(NULL,xlim=xlim,ylim=ylim,xlab=xlab,ylab=ylab)

   curves <- split(curveData,curveData[,3])
   nCurves <- length(curves)

   cols <- rainbow(nCurves)

   nms <- as.factor(names(curves))
   for (i in 1:nCurves) {
      cvsi <- curves[[i]]
      if (loess) {
         tmp <- loess(y ~ x,cvsi)
         cvsi[,2] <- predict(tmp,cvsi[,1])
      }
      cvsiOrdered <- cvsi[order(cvsi[,1]),]
      lines(cvsiOrdered,col=cols[i])
   }

   ### cd3 <- curveData[,3]
   ### if (!is.factor(cd3)) curveData[,3] <- as.factor(cd3)

   legend(legendPos,legend=levels(nms),col=cols,lty=rep(1,nCurves))

}

# generate data for f(x) = x and g(x) = x^, plot
test1 <- function() 
{
   x <- runif(100)
   y1 <- x
   y2 <- x^2
   outdf <- data.frame(x=c(x,x),y=c(y1,y2),
      z=c(rep('1',100),rep('2',100)))
   qePlotCurves(outdf)
}

# fit 4 qe* ftns on lsa data, plot; call form is 
# test2(5), or put anything else instead of 5;
# zzz just a dummy for arg 1, not used; do
#
#    w <- test2(5)
#    qePlotCurves(w)
#
# to run
test2 <- defmacro(zzz,  
   expr = {
     data(lsa)
     lsa1 <- lsa[sample(1:nrow(lsa),1000),]
     data(svcensus)
     svc <- svcensus[sample(1:nrow(lsa),1000),]
     svc <- na.exclude(svc)
     xvals <- seq(5,75,5)
     outDF <- data.frame(x=NULL,y=NULL,z=NULL)
     for (i in 1:15) {
        tmp <- replicMeans(25,"qeXGBoost(svc,'wageinc',
           params=list(max_depth=i))$testAcc")
        outDF <- rbind(outDF,data.frame(x=i,y=tmp,z='XGB'))
        tmp <- replicMeans(25,"qeKNN(svc,'wageinc',k=xvals[i])$testAcc")
        outDF <- rbind(outDF,data.frame(x=i,y=tmp,z='KNN'))
        tmp <- replicMeans(25,"qeRFranger(svc,'wageinc',
           minNodeSize=xvals[i])$testAcc")
        outDF <- rbind(outDF,data.frame(x=i,y=tmp,z='RF'))
        tmp <- replicMeans(25,"qePolyLin(svc,'wageinc',
           deg=i,maxInteractDeg=2)$testAcc")
        outDF <- rbind(outDF,data.frame(x=i,y=tmp,z='polyLin'))
     }
     outDF$z <- as.factor(outDF$z)
     outDF
   }
)

# generate data to go into qePlotCurves(); finds mean testAcc over nreps
# runs, each with a different random training set

# dataName and qeFtnName should be changed to data and qeFtn soon

genQeAcc <- function(nreps,dataName,yName,qeFtnName,opts=NULL)
{
  # goal: set up do.call
   data <- get(dataName)
   qeFtn <- get(qeFtnName)
   dcArgs <- list(data=data,yName=yName)
   if (!is.null(opts)) {
      nms <- names(opts)
      for (nm in nms) {
         dcArgs[[nm]] <- opts[[nm]]
      }
   }
   tmp <- sapply(1:nreps,function(i) 
      {cmdOut <- do.call(qeFtn,dcArgs); cmdOut$testAcc})
   mean(tmp)
}

# planned replacement of regtools::replicMeans
# extension of replicate() code
# charExpr is a quoted string
replicMeans1old <- function (n, charExpr, simplify = "array") {
   expr <- eval(parse(text=charExpr))
   tmp <- sapply(integer(n), eval.parent(substitute(function(...) expr)),
      simplify = simplify)
   if (!is.matrix(tmp)) tmp <- matrix(tmp,nrow=n)
   rowMeans(tmp)
}

