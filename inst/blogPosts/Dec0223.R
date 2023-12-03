
# could be make more efficient and automatic

library(qeML)
data(svcensus)
svc5000 <- svcensus[1:5000]

tk <- for(kval in seq(5,75,5)) genQeAcc(100,'svc5000','wageinc','qeKNN',opts=l ist(k=kval))

trf <- for(mns in seq(5,75,5)) genQeAcc(100,'svc5000','wageinc','qeRFranger',opts=list(minNodeSize=mns))

tpol <- for(degval in seq(1,15,1)) genQeAcc(100,'svc5000','wageinc','qePolyLin',opts=list(deg=degval,maxInteractDeg=2))

txgb <- for(md in seq(1,15,1)) genQeAcc(100,'svc5000','wageinc','qeXGBoost',opts=list(params=list(max_depth=md,eta=0.05)))

outdf <- data.frame(x=NULL,y=NULL,z=NULL) 
outdf <- rbind(outdf,data.frame(x=1:15,y=tk,z='KNN')) 
outdf <- rbind(outdf,data.frame(x=1:15,y=trf,z='RF')) 
outdf <- rbind(outdf,data.frame(x=1:15,y=tpol,z='Poly')) 
outdf <- rbind(outdf,data.frame(x=1:15,y=txgb,z='XGB')) 
tradeoff(outdf,'hyperparIdx','accuracy',loess=FALSE) 

genQeAcc <- function(nreps,dataName,yName,qeFtnName,opts=NULL)
{                                                     
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

tradeoff
function(curveData,xlab='',ylab='',loess=TRUE,
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
      lines(cvsi,col=cols[i])
      # print(nms[i])
      # browser()
   }

   cd3 <- curveData[,3]
   if (!is.factor(cd3)) curveData[,3] <- as.factor(cd3)

   legend(legendPos,legend=levels(curveData[,3]),
      col=cols,lty=rep(1,nCurves))

}




