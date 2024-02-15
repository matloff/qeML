
# qeML wrapper for 'torch for r'

# arguments:

#    data: standard qeML
#    yName: standard qeML
#    layers: list of lists; layers[[i]] specifies the layer type and any
#       parameters for layer i

# draws upon https://torch.mlverse.org/technical/modules/

qeNeuralTorch <- function(data,yName,layers,
   learnRate=0.001,lossFtn=nn_mse_loss,nEpochs=100,
   holdout=floor(min(1000,0.1*nrow(data))))
{

   ### prep general
   checkPkgLoaded('torch')
   checkForNonDF(data)
   trainRow1 <- getRow1(data,yName)
   ycol <- which(names(data) == yName)
   y <- data[,ycol]
   # for now: regression case only
   classif <- FALSE
   if (!is.numeric(y)) stop('regression case only for now')
   x <- data[,-ycol]
   yToAvg <- y
   nYcols <- 1
   # torch requires numeric data
   classes <- sapply(data,class)
   if (sum(classes=='numeric') + sum(classes=='integer') < ncol(data)) {
      stop('all features must be numeric; use factorsToDummies to fix')
      # x <- regtools::factorsToDummies(x,omitLast=TRUE)
      # factorsInfo <- attr(x,'factorsInfo')
   } else factorsInfo <- NULL

   ### form holdout if requested
   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) makeHoldout(0)
   x <- as.matrix(x)
   xT <- torch_tensor(x)
   yToAvg <- matrix(as.numeric(yToAvg,ncol=1))
   yT <- torch_tensor(yToAvg)

   ### set up model
   nnSeqArgs <- list()
   for (i in 1:length(layers)) {
      layer <- layers[[i]]
      nnSeqArgs[[i]] <- 
         if(layer[[1]]=='linear') nn_linear(layer[[2]],layer[[3]]) else
         nn_relu(inplace=FALSE)
   }
   model <- do.call(nn_sequential,nnSeqArgs)

   # learning_rate <- learnRate

   optimizer <- optim_adam(model$parameters, lr = learnRate)

   ### training
   for (i in 1:nEpochs) {
      preds <- model(xT)
      loss <- nnf_mse_loss(preds,yT,reduction = "sum")
      optimizer$zero_grad()
      loss$backward()
      optimizer$step()
   }

browser()
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
      tsty <- tst[,ycol]
      newLvls <- checkNewLevels(trnx,tstx)
      if (length(newLvls) > 0) {
         tstx <- tstx[-newLvls,,drop=FALSE]
         tst <- tst[-newLvls,,drop=FALSE]
         warning(paste(length(newLvls),
            'rows removed from test set, due to new factor levels'))
      }

      preds <- predict(res,tstx);
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds
         
      if (res$classif) {
         stop('regression case only for now')
      } else {  # regression case
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycol])))
         predsTrn <- predict(res,trnx)
         res$trainAcc <- mean(abs(predsTrn - trn[,ycol]))
      }  
   }  # end of expr= for the macro
)        



predict.qeNeuralTorch <- function(object,newx,...)
{
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
