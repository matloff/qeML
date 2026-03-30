
m3torch <- function (data, yName, neurons = c(100, 100), batch_size = 16,
    epochs = 50, device = "cpu", validate = 0.1, learnRate = 0.001,
    activation = torch::nn_relu, holdout = floor(min(1000, 0.1 * nrow(data))))
{
    library(mlr3)
    library(mlr3torch)
    library(torch)
    library(qeML)
    checkPkgLoaded("mlr3")
    checkPkgLoaded("mlr3torch")
    checkForNonDF(data)
    yNameSave <- yName
    trainRow1 <- qeML:::getRow1(data, yName)
    classif <- is.factor(data[[yName]])
    ycol <- which(names(data) == yName)
    if (classif) {
        y <- data[, yName]
        yLevels <- levels(y)
    }
    else {
        yLevels <- NULL
    }
    # holdIdxs <- tst <- trn <- NULL
    # if (!is.null(holdout)) {
    #     qeML:::splitData(holdout, data)
    # }
    x <- data[,-ycol,drop =FALSE]
    if (!regtools::allNumeric(x)) {
        x <- regtools::factorsToDummies(x, omitLast = TRUE, dfOut = TRUE)
        factorsInfo <- attr(x, "factorsInfo")
    }
    else factorsInfo <- NULL
    origData <- data
    data <- cbind(x, data[, ycol])
    names(data)[ncol(data)] <- yName
    mlr3Out <- list()
    if (!is.null(holdout)) {
       holdIdxs <- sample(1:nrow(x),holdout)
       data <- data[-holdIdxs,]
       xTst <- x[holdIdxs,]
       yTst <- origData[holdIdxs,ycol]
       mlr3Out$holdIdxs <- holdIdxs
    }
    else {
        mlr3Out$holdIdxs <- NULL
    }
    if (classif) {
        task <- mlr3::TaskClassif$new(id = "qeml_task", backend = data,
            target = yName)
        lrnr <- mlr3::lrn("classif.mlp", batch_size = batch_size,
            epochs = epochs, neurons = neurons, lr = learnRate,
            activation = activation, validate = validate, device = device)
    }
    else {
        task <- as_task_regr(data,target=yName)
        lrnr <- lrn("regr.mlp",batch_size = batch_size,
            epochs=epochs,neurons=neurons,
            activation <- activation,validate=validate,device=device,
            loss <- t_opt("adam",lr=learnRate))
    }
    lrnr$train(task)
    if (!is.null(holdout)) {
       preds <- lrnr$predict_newdata(xTst)
       testAcc <-
          if (!classif) mean(abs(yTst-preds))
          else mean(preds != yTst)
    }
    mlr3Out$learner <- lrnr
    mlr3Out$task <- task
    mlr3Out$classif <- classif
    mlr3Out$factorsInfo <- factorsInfo
    mlr3Out$yLevels <- yLevels
    mlr3Out$yName <- yNameSave
    mlr3Out$trainRow1 <- trainRow1
    mlr3Out$testAcc <- testAcc
    class(mlr3Out) <- "qeNNmlr3"
    mlr3Out
}

