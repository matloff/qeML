

# qeML wrapper for 'torch for r'

# arguments:

#    data: standard qeML
#    yName: standard qeML
#    layers: list of lists; layers[[i]] specifies the layer type and any
#       parameters for layer i

qeNeuralTorchModel <- function(data,yName,layers,
   learnRate=0.001,holdout=floor(min(1000,0.1*nrow(data))))
{

   ### prep general
   checkPkgLoaded('torch')
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   # for now: regression case only
   if (!numeric(y)) stop('regression case only for now')
   x <- data[,-ycol]
   yToAvg <- y
   nYcols <- 1
   # torch requires numeric data
   if (!is.numeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL

   ### form holdout if requested
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      nHold <- holdout
      holdIdxs <- sample(1:nrow(x),nHold)
      xTst <- x[holdIdxs,]
      x <- x[-holdIdxs,]
      yTst <- yToAvg[holdIdxs,]
      yToAvg <- yToAvg[-holdIdxs,]
      tst <- cbind(xTst,yTst)
      tst <- as.data.frame(tst)
      trn <- cbind(x,yToAvg)
      ycol <- ncol(trn)
      xT <- torch_tensor(x)
      yT <- torch_tensor(yToAvg)
      nrX <- nrow(xT)
      ncX <- ncol(xT)
   }

   ### set up model
   nnSeqArgs <- list()
   for (i in 1:length(layers)) {
      layer <- layers[[i]]
      nnSeqArgs[[i]] <- switch(layr[[1]],
         'linear': nn_linear(layer[[2]],layer[[3]]),
         'relu': nn_relu(inplace=FALSE)
      )

   }
   model <- do.call(nn_sequential,nnSeqArgs)

   ### training


}

