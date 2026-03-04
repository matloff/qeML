
# qeML wrapper for mlr3torch neural network

# arguments:
#    data: standard qeML
#    yName: standard qeML
#    neurons: vector specifying number of neurons in each hidden layer
#    batch_size: batch size for training
#    epochs: number of training epochs
#    device: 'cpu' or 'cuda'
#    validate: fraction of data for validation
#    learnRate: learning rate
#    activation: activation function name (e.g., 'relu', 'tanh')
#    holdout: standard qeML holdout set size

qeNNmlr3 <- function(data,yName,neurons=c(100,100),batch_size=16,
   epochs=50,device='cpu',validate=0.1,learnRate=0.001,
   activation='relu',
   holdout=floor(min(1000,0.1*nrow(data))))
{

library(mlr3)

   checkPkgLoaded('mlr3')
   checkPkgLoaded('mlr3torch')
   
   checkForNonDF(data)
   yNameSave <- yName
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   ycol <- which(names(data) == yName)
   
   if (classif) {
      y <- data[,yName]
      yLevels <- levels(y)
      # mlr3 handles factor encoding internally
   } else {
      yLevels <- NULL
   }

   holdIdxs <- tst <- trn <- NULL  # for CRAN "unbound globals" complaint
   if (!is.null(holdout)) {
      splitData(holdout,data)
   }
   
   # handle factors in predictors
   x <- data[,-ycol,drop=FALSE]
   if (!regtools::allNumeric(x)) {
      x <- regtools::factorsToDummies(x,omitLast=TRUE,dfOut=TRUE)
      factorsInfo <- attr(x,'factorsInfo') 
   } else factorsInfo <- NULL

   # reconstruct data with processed x
   data <- cbind(x, data[,ycol])
   names(data)[ncol(data)] <- yName

browser()

   # set up learner
   if (classif) {
      task <- mlr3::TaskClassif$new(
         id = "qeml_task",
         backend = data,
         target = yName
      )
      lrnr <- mlr3::lrn('classif.mlp',
         batch_size = batch_size,
         epochs = epochs,
         neurons = neurons,
         lr = learnRate,
         activation = activation,
         validate = validate,
         device = device
      )
   } else {
      task <- mlr3::TaskRegr$new(
         id = "qeml_task",
         backend = data,
         target = yName
      )
      lrnr <- mlr3::lrn('regr.mlp',
         batch_size = batch_size,
         epochs = epochs,
         neurons = neurons,
         lr = learnRate,
         activation = activation,
         validate = validate,
         device = device
      )
   }

   # train the model
   lrnr$train(task)

   # create output object
   mlr3Out <- list()
   mlr3Out$learner <- lrnr
   mlr3Out$task <- task
   mlr3Out$classif <- classif
   mlr3Out$factorsInfo <- factorsInfo
   mlr3Out$yLevels <- yLevels
   mlr3Out$yName <- yNameSave
   mlr3Out$trainRow1 <- trainRow1
   
   class(mlr3Out) <- 'qeNNmlr3'

   if (!is.null(holdout)) {
      predictHoldoutMlr3(mlr3Out)
      mlr3Out$holdIdxs <- holdIdxs
   } else {
      mlr3Out$holdIdxs <- NULL
   }

   mlr3Out
}

predict.qeNNmlr3 <- function(object,newx,...) 
{
   # handle factors
   newx <- setTrainFactors(object,newx)
   
   if (!regtools::allNumeric(newx)) 
      newx <- regtools::factorsToDummies(newx,omitLast=TRUE,
         factorsInfo=object$factorsInfo)
   
   # mlr3 needs a task for prediction
   # add dummy y column
   if (object$classif) {
      newx[[object$yName]] <- factor(rep(object$yLevels[1], nrow(newx)),
                                      levels = object$yLevels)
   } else {
      newx[[object$yName]] <- rep(0, nrow(newx))
   }
   
   # make predictions
   preds <- object$learner$predict_newdata(newx)
   
   if (object$classif) {
      predClasses <- as.character(preds$response)
      # get probabilities if available
      if (!is.null(preds$prob)) {
         probs <- as.matrix(preds$prob)
         colnames(probs) <- object$yLevels
      } else {
         probs <- NULL
      }
      list(predClasses=predClasses, probs=probs)
   } else {
      as.numeric(preds$response)
   }
}

# macro for holdout prediction
predictHoldoutMlr3 <- defmacro(res,
   expr={
      tstx <- tst[,-ycol,drop=FALSE]
      trnx <- trn[,-ycol,drop=FALSE]
      trny <- trn[,ycol,drop=FALSE]
      tsty <- tst[,ycol]
      
      preds <- predict(res,tstx)
      listPreds <- is.list(preds)
      res$holdoutPreds <- preds

      if (classif) {
         predClasses <- preds$predClasses
         probs <- preds$probs
         res$testAcc <- mean(predClasses != tsty, na.rm=TRUE)
         res$baseAcc <- 1 - max(table(data[,ycol])) / nrow(data)
      } else {  # regression case
         res$testAcc <- mean(abs(preds - tsty), na.rm=TRUE)
         meantrny <- mean(trny, na.rm=TRUE)
         res$baseAcc <- mean(abs(meantrny - tsty), na.rm=TRUE)
         predsTrn <- predict(res, trnx)
         res$trainAcc <- mean(abs(predsTrn - trny), na.rm=TRUE)
      }
   }  # end of expr= for the macro
)

