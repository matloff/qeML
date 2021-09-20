
#########################  qeRFpartykit()  #################################

# random forests, wrapper to partykit::ctree(

# arguments:  see above, plus

#     mincriterion: 1-alpha, where alpha is the Type I error
#        value for the test statistic
#     minsplt: minimum number of data points in a node
#     mtry: number of variables randomly tried at each split
#     maxdepth: maximum number of levels to tree

# value:  see above
 
qeRFpartykit <- function(data,yName,
   alpha=0.05,minsplit=20,mtry=0,maxdepth=0,ntree=500,
   holdout=floor(min(1000,0.1*nrow(data))))
{
stop('under construction')
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
   require(partykit)
   xyc <- getXY(data,yName,xMustNumeric=FALSE,classif=classif)
   frml <- as.formula(paste(yName,'~ .'))
   ctrl <- ctree_control(alpha=alpha,minsplit=minsplit,
      mtry=mtry,maxdepth=maxdepth)
   # cfout <- cforest(frml,data=data,controls=ctrl,ntree=ntree)
   cfout <- cforest(frml,data=data,ntree=ntree)
   rfout <- list(cfout=cfout)
   rfout$classNames <- xyc$classNames
   rfout$classif <- classif
   rfout$trainRow1 <- getRow1(data,yName)
   # info on terminal nodes (tnodes) and all nodes
   whr <- cfout@where  # tnode membership for each data point
   rfout$termNodes <- sort(unique(whr))  # IDs of tnodes in tree
   rfout$termNodeMembers <- 
      split(1:nrow(data),whr)  # which data in which tnodes
   rfout$termNodeCounts <- 
      sapply(dtout$termNodeMembers,length)  # count in each tnode
   rfout$nTermNodes <- length(dtout$termNodes)  
   rfout$nNodes <- max(dtout$termNodes)
   class(rfout) <- c('qeRFpartykit','partyki')
   if (!is.null(holdout)) {
      predictHoldout(rfout)
      rfout$holdIdxs <- holdIdxs
   }
   rfout
}

predict.qeRFpartykit <- function(object,newx)
{
   cfout <- object$cfout
   newx <- setTrainFactors(object,newx)
   tmp <- predict(cfout,newx)
   if (object$classif) list(predClasses=tmp)
   else tmp
}

