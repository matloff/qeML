
# qeML wrapper for 'torch for r', alternative to qeNeural not needing
# Python; see also the 'cito' package

# arguments:

#    data: standard qeML
#    yName: standard qeML
#    layers: list of lists; layers[[i]] specifies the layer type and any
#       parameters for layer; the fan-in may be specified as 0 for the
#       first layer, in which case it will be set the number of features
#       (after factors, if any, are converted to dummies)
#    yesYVal: for factor Y, which level is to be considered Y = 1 not 0
#    learnRate: learning rate, vital, might need to be even e-05 or 2
#    wtDecay: weight decay, regularization
#    nEpochs: number of iterations
#    dropout fraction
#    holdout: as in other QE functions

# see examples at the end of this file

qeNeuralTorch <- function(data,yName,layers,yesYVal=NULL,
   learnRate=0.001,wtDecay=0,nEpochs=100,dropout=0,
   holdout=floor(min(1000,0.1*nrow(data))))
{

   ### prep general

   checkPkgLoaded('torch')
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   ycol <- which(names(data) == yName)
   # y, yesYVal etc.
   y <- data[,ycol]
   #### if (sum(y==1) + sum(y==0) == length(y)) {
   if (is.factor(y)) {
      # maybe later do data[y==0,ycol] <- -1
      classif <- TRUE
      yLevels <- levels(y)
      if (length(yLevels) != 2) {
         stop('presently classif case only for binary Y')
      }
      if (is.null(yesYVal)) yesYVal <- yLevels[1]
      y <- as.numeric(y == yesYVal)  # 0s and 1s noww
   } else classif <- FALSE
   yToAvg <- y
   nYcols <- 1  # in classif case, binary Y only
   x <- data[,-ycol]
   # torch requires numeric data
   classes <- sapply(data,class)
   if (sum(classes=='numeric') + sum(classes=='integer') < ncol(data)) {
      #### stop('all features must be numeric; use factorsToDummies to fix')
      x <- regtools::factorsToDummies(x,omitLast=TRUE)
      factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL

   ### form holdout if requested

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) makeHoldout(0)
   x <- as.matrix(x)
   xT <- torch_tensor(x)
   yToAvg <- matrix(as.numeric(yToAvg,ncol=1))
   yT <- torch_tensor(yToAvg)
   # at this point, our training data, whether we have holdout or not,
   # is the above, i.e. x, xT, yToAvg and yT (the 'T' versions are
   # tensors for Torch); in the with-holdout case, we also have the
   # analogous entities xTst, yTst; the cbind-ed xT and yToAvg are in
   # trn, with the analogous tst

   ### set up model

   nnSeqArgs <- list()
   for (i in 1:length(layers)) {
      layer <- layers[[i]]
      if (i == 1 && layer[[2]] == 0) layer[[2]] <- ncol(x)
      nnSeqArgs[[i]] <- 
         if(layer[[1]]=='linear') nn_linear(layer[[2]],layer[[3]]) else
         if(layer[[1]]=='relu') nn_relu(inplace=FALSE) else
         if(layer[[1]]=='dropout') nn_dropout(dropout) else
         nn_sigmoid()
   }
   model <- do.call(nn_sequential,nnSeqArgs)

   optimizer <- optim_adam(model$parameters, 
      lr = learnRate,weight_decay=wtDecay)

   ### training

   for (i in 1:nEpochs) {
      preds <- model(xT)
      loss <- nnf_mse_loss(preds,yT,reduction = "sum")
      # maybe nnf_binary_cross_entropy_with_logits
      optimizer$zero_grad()
      loss$backward()
      optimizer$step()
   }

   torchout <- list()
   torchout$classif <- classif
   # torchout$classNames <- classNames
   torchout$x <- x
   torchout$xT <- xT
   torchout$yT <- yT
   # torchout$yFactor <- yFactor
   torchout$trainRow1 <- getRow1(data,yName)
   torchout$model <- model
   class(torchout) <- c('qeNeuralTorch','torch_tensor')
   if (!is.null(holdout)) {
      predictHoldoutTorch(torchout)
      torchout$holdIdxs <- holdIdxs
   }

   torchout

}

predictHoldoutTorch <- defmacro(res,
   expr={

      ycol <- which(names(tst) == 'yTst');
      tstx <- tst[,-ycol,drop=FALSE];
      trnx <- trn[,-ycol,drop=FALSE];
      # tsty <- tst[,ycol]

      preds <- predict(res,tstx);
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds
         
      if (res$classif) {
         #### stop('regression case only for now')
         preds <- round(as.numeric(preds[,1]))  
         res$testAcc <- mean(preds!=tst[,ycol])
      } else {  # regression case
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         ### res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycol])))
         predsTrn <- predict(res,trnx)
         res$trainAcc <- mean(abs(predsTrn - trn[,ycol]))
      }  
   }  # end of expr= for the macro
)        

predict.qeNeuralTorch <- function(object,newx,...)
{
   finfo <- object$factorsInfo
   if(!is.null(finfo)) {
      newx <- factorsToDummies(newx,omitLast=TRUE,factorsInfo=finfo)
   }
   newx <- as.matrix(newx)
   newxT <- torch_tensor(newx)
   object$model(newxT)
}

makeHoldout <- defmacro(placeholder,expr=
   {
      nHold <- holdout
      holdIdxs <- sample(1:nrow(x),nHold)
      xTst <- x[holdIdxs,]
      x <- x[-holdIdxs,]
      yTst <- yToAvg[holdIdxs]
      yToAvg <- yToAvg[-holdIdxs]
      tst <- cbind(xTst,yTst)
      tst <- as.data.frame(tst)
      trn <- cbind(x,yToAvg)
      ycol <- ncol(trn)
      nrX <- nrow(x)
      ncX <- ncol(x)
   }
)
