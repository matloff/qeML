
# by N. Matloff, Kenneth Lee and Rongkui Han


#######################################################################
#######################  calibration  #################################
#######################################################################

# the *Calib() functions generate estimated conditional class
# probabilities from scores, e.g.  SVM decision values

# useful for classification methodology that does not inherently
# generate those probabilities, or for which those probabilities are
# biased or otherwise inaccurate

#########################  knnCalib()  ################################
#' Calibrate probability by k-NN
#'
#' for a given new case, the k nearest neighbors in the training set are
#' obtained; the class labels for the neighbors are then obtained, and
#' the resulting proportions for the different labels are then used as
#' the estimated probabilities for the binary class.

#' @param y R factor of labels in training set
#' @param trnScores vector/matrix of scores in training set
#' @param tstScores scores of new case(s)
#' @param k number of nearest neighbors
#' @param loclin indicate whether local linear regression should be applied
#' @param smoothingFtn a smoothing function whether it should be applied.
#' @return vector of estimated probabilities for the new cases
#' @export
knnCalib <- function(y,
                     trnScores,
                     tstScores,
                     k,
                     loclin=FALSE,
                     scaleX=NULL,
                     smoothingFtn=NULL)
{
  if (!is.factor(y)) stop('Y must be an R factor')
  if (is.vector(trnScores))
    trnScores <- matrix(trnScores,ncol=1)
  if (is.vector(tstScores))
    tstScores <- matrix(tstScores,ncol=1)
  if (length(levels(y)) < 2) stop("Y should have at least 2 levels.")

  if(loclin){
    # loclin case
    # turn multcase into 0 and 1 for lm()
    classes <- levels(y)
    probMat <- matrix(NA, nrow(tstScores), length(classes))
    count <- 1

    for(class in classes){

      tmp_y <- ifelse(y == class, 1, 0)

      dat <- as.data.frame(cbind(tmp_y, trnScores))

      knnout <- qeKNN(dat, colnames(dat)[1], k=k, scaleX=scaleX,
                      smoothingFtn=smoothingFtn,
                      holdout=NULL)

      tstS  <- as.data.frame(tstScores)
      # ensure the test set has the same column names
      colnames(tstS) <- colnames(dat)[2:ncol(trnScores)]

      # knn model prediction
      pred <- predict(knnout, tstS)

      # set all values that are greater than 1 to 1
      pred[pred > 1] <- 1
      # set all values that are lower than 0 to 0
      pred[pred < 0] <- 0

      pred <- as.vector(pred)
      probMat[, count] <- pred
      count <- count + 1
    }

  }else{

    # mean case instead of loclin
    original_class <- levels(y)
    dat <- as.data.frame(cbind(trnScores, y))
    yname <- colnames(dat)[ncol(dat)]
    dat[[yname]] <- as.factor(dat[[yname]])
    levels(dat[[yname]]) <- original_class
    knnout <- qeKNN(dat, yname, k=k, scaleX=scaleX, holdout=NULL)
    tstS  <- as.data.frame(tstScores)
    out <- predict(knnout, tstS)
    probMat <- out$probs
  }
  probMat
}

scoresToProbs <- knnCalib



#########################   ovaCalib() ###################################
#' Calibration for one-vs-all case
#'
#' a wrapper to implement calibration methods via one-vs-all approach
#'
#' @param trnY the training labels
#' @param trnScores a matrix of decision values or probabilities given by the classifier
# for the training sample
#' @param tstScores a matrix of decision values or probabilities given by the classifier
# for the testing sample
#' @param calibMethod a string that indicate which calibration method to use.
#' options: "knnCalib", "locLinknnCalib", "plattCalib", "isoCalib", "BBQCalib", "ELiTECalib"
#' @param deg an integer to indicate the degree of polynomial for platt scaling
#' @param K an integer to indicate the number of nearest neighbor
#' @param scaleX an integer to indicate the number of nearest neighbor
#' @param smoothingFtn a smoothing function whether it should be applied
#' @param isProb indicate whether the scores are probabilities
#' @param fitAllX to determine whether you want to use all scores for fitting algorithms that are not knn-based.
#' @return matrix of estimated probabilities for the new cases
#' @export

ovaCalib <- function(trnY,
                     trnScores,
                     tstScores,
                     calibMethod,
                     deg=NULL,
                     se=FALSE,
                     K= NULL,
                     scaleX = NULL,
                     smoothingFtn=NULL,
                     isProb = FALSE,
                     fitAllX = FALSE)
{
  if (!is.factor(trnY)) stop('Y must be an R factor')

  if (nrow(trnScores) != length(trnY)) {
    stop('fewer scores than Ys; did you have nonnull holdout?')
  }

  if (is.vector(trnScores)){
    trnScores <- matrix(trnScores,ncol=1)
  }

  if (is.vector(tstScores)){
    tstScores <- matrix(tstScores,ncol=1)
  }

  # Get all the levels of y
  classes <- levels(trnY)

  # Compute posterior probabilities matrix probMat
  if(calibMethod == 'knnCalib'){
    if (is.null(K)) stop('k needs to be provided for k-NN')

    probMat <- knnCalib(trnY, trnScores, tstScores, k=K, scaleX=scaleX)

  }else if(calibMethod == 'locLinknnCalib'){
    probMat <- knnCalib(trnY, trnScores, tstScores, k=K, loclin=TRUE,
                        scaleX=scaleX, smoothingFtn=smoothingFtn)

  }else{
    # create a empty matrix for storing probabilities
    probMat <- matrix(NA, nrow(tstScores), length(classes))

    if(se){
      # create a empty matrix for storing standard errors
      seMat <-matrix(NA, ncol(tstScores), length(classes))
    }

    # Using all covariates
    if(fitAllX){
      for(i in 1:length(classes)){
        # select the class1
        class1 <- classes[i]
        if(calibMethod == 'plattCalib') {
          # create 1's and 0's for OVA
          y <- as.factor(ifelse(trnY == class1, 1, 0))
          if(se){
            result <- plattCalib(y, trnScores, tstScores, deg, se=se)
            prob <- result$probs
            seMat[,i] <- result$SE
          }else{
            prob <- plattCalib(y, trnScores, tstScores, deg, se=se)
          }

        } else if (calibMethod == 'isoCalib') {
          # this package CORElearn uses y as 1 and 2
          y <- as.factor(ifelse(trnY == class1, 1, 2))
          prob <- isoCalib(y, trnScores, tstScores, isProb)

        } else if (calibMethod == 'BBQCalib') {
          # create 1's and 0's for OVA
          y <- ifelse(trnY == class1, 1, 0)
          # the paper suggests that However, model averaging
          # is typically superior to model selection (Hoeting et al. 1999)
          # so we use option = 1 for predict_BBQ
          prob <- bbqCalib(y, trnScores, tstScores, option = 1)

        } else if (calibMethod == 'ELiTECalib') {
          # create 1's and 0's for OVA
          y <- ifelse(trnY == class1, 1, 0)

          prob <- eliteCalib(y, trnScores, tstScores)

        } else stop('invalid calibration method')

        probMat[,i] <- prob
      }

      # Use 1 class of scores at a time
    }else{

      for(i in 1:length(classes)){
        # select the class1
        class1 <- classes[i]
        trnDV <- trnScores[,i]
        tstDV <-  tstScores[,i]

        # Compute posterior probabilities vector prob for class i
        if(calibMethod == 'plattCalib') {

          # create 1's and 0's for OVA
          y <- as.factor(ifelse(trnY == class1, 1, 0))
          if(se){

            result <- plattCalib(y, trnDV, tstDV, deg, se=se)
            prob <- result$probs
            seMat[,i] <- result$SE

          }else{
            prob <- plattCalib(y, trnDV, tstDV, deg, se=se)

          }

        } else if (calibMethod == 'isoCalib') {

          # this package CORElearn uses y as 1 and 2
          y <- as.factor(ifelse(trnY == class1, 1, 2))

          prob <- isoCalib(y, trnDV, tstDV, isProb)

          # GUESS can only take values within [0,1] as the scores
          # } else if (calibMethod == 'guessCalib'){

          #    y <- ifelse(trnY == class1, 1, 0)

          #    prob <- guessCalib(y, trnDV, tstDV)

        } else if (calibMethod == 'BBQCalib') {

          # create 1's and 0's for OVA
          y <- ifelse(trnY == class1, 1, 0)
          # the paper suggests that However, model averaging
          # is typically superior to model selection (Hoeting et al. 1999)
          # so we use option = 1 for predict_BBQ
          prob <- bbqCalib(y, trnDV, tstDV, option = 1)

        } else if (calibMethod == 'ELiTECalib') {

          # create 1's and 0's for OVA
          y <- ifelse(trnY == class1, 1, 0)

          prob <- eliteCalib(y, trnDV, tstDV)

        } else stop('invalid calibration method')

        probMat[,i] <- prob
      }
    }
    # Simple normalization
    probMat <- t(apply(probMat, 1 ,function(x) x/sum(x, na.rm=TRUE)))
  }

  if(se){
    list(probMat= probMat, seMat= seMat)
  }else{
    # Return the probability matrix
    probMat
  }
}



#########################   avaCalib() ###################################

#' Calibration for all-vs-all case
#'
#' a wrapper to implement calibration methods via all-vs-all approach
#' this is experimental, not fully tested!
#'
#' @param trnY the training labels
#' @param trnScores a matrix of decision values or probabilities given by the classifier
#' for the training sample
#' @param tstScores a matrix of decision values or probabilities given by the classifier
#' for the testing sample
#' @param calibMethod a string that indicate which calibration method to use.
#' options: "knnCalib", "locLinknnCalib", "plattCalib","isoCalib","BBQCalib", "ELiTECalib"
#' @param deg an integer to indicate the degree of polynomial for platt scaling
#' @param K an integer to indicate the number of nearest neighbor
#' @param scaleX to determine whether we should standardize the data before using k-NN
#' @param smoothingFtn a smoothing function whether it should be applied
#' @param isProb indicate whether the scores are probabilities
#' @return matrix of estimated probabilities for the new cases

avaCalib <- function(trnY,
                     trnScores,
                     tstScores,
                     calibMethod,
                     deg=NULL,
                     K= NULL,
                     se = FALSE,
                     isProb = NULL)
{
  require(regtools)
  if (!is.factor(trnY)) stop('Y must be an R factor')

  if (nrow(trnScores) != length(trnY)) {
    stop('fewer scores than Ys; did you have nonnull holdout?')
  }

  if (is.vector(trnScores)){
    trnScores <- matrix(trnScores,ncol=1)
  }

  if (calibMethod == 'knnCalib') {

    if (is.null(K)) stop('k needs to be provided for k-NN')

    probMat <- knnCalib(trnY,trnScores,tstScores, K)

  } else {
    # Get all the levels of y
    classes <- levels(trnY)
    # AVA case
    probMat <- matrix(NA, nrow(tstScores), choose(length(classes), 2))

    if(se){
      seMat <- matrix(NA, ncol(tstScores), choose(length(classes), 2))
    }
    counter <- 1
    for(i in 1: (length(classes)-1)){
      # Get the class 1
      class1 <- classes[i]
      for(k in (i+1):length(classes)){
        # Get the class 2
        class2 <- classes[k]

        # Get all corresponding row indicies
        rowIdx <- which(trnY==class1 | trnY== class2)

        # Filter train scores matrix
        trnDV <- trnScores[rowIdx,counter]

        tstDV  <- tstScores[,counter]


        # we only use the corresponding column
        # for training, assuming the columns follows the order
        # class1vsclass2, class1vsclass3...class2vsclass3...

        if (calibMethod == 'plattCalib') {

          if (is.null(deg)) stop('Deg needs to be provided for platt')

          # create 1's and 0's for OVA
          y  <- as.factor(ifelse(trnY[rowIdx]==class1, 1, 0))

          if(se){

            result <- plattCalib(y, trnDV, tstDV, deg, se=se)
            prob <- result$probs
            seMat[,i] <- result$SE

          }else{
            prob <- plattCalib(y, trnDV, tstDV, deg, se=se)
          }

        } else if (calibMethod == 'isoCalib') {

          y <- ifelse(trnY[rowIdx]==class1, 1, 0)

          prob <- isoCalib(y, trnDV, tstDV, isProb)

        } else if (calibMethod == 'BBQCalib') {

          # create 1's and 0's for OVA
          y <- ifelse(trnY[rowIdx]==class1, 1, 0)
          # the paper suggests that However, model averaging
          # is typically superior to model selection (Hoeting et al. 1999)
          # so we use option = 1 for predict_BBQ
          prob <- bbqCalib(y, trnDV, tstDV, option = 1)

        } else if (calibMethod == 'ELiTECalib') {

          # create 1's and 0's for OVA
          y <- ifelse(trnY[rowIdx]==class1, 1, 0)

          prob <- eliteCalib(y, trnDV, tstDV)

        } else stop('invalid calibration method')

        # Store the probability in a matrix
        probMat[,counter] <- prob
        counter <- counter + 1
      }
    }
    # Simple normalization
    probMat <- t(apply(probMat, 1 ,function(x) x/sum(x, na.rm=TRUE)))
  }
  if(se){
    list(probMat= probMat, seMat= seMat)
  }else{
    # Return the probability matrix
    probMat
  }
}



#########################  fitPlatt()  ################################

#' Fitting a losgitic model
#'
#' run the logit once and save, rather doing running repeatedly, each
#' time we have new predictions to make
#'
#' @param y the training labels
#' @param trnScores a vector of decision values or probabilities given by the classifier for the training sample
#' @param degree an integer to indicate the degree of polynomial for the logistic model
#' @return a logistic model
#' @export
fitPlatt <- function(y,trnScores,deg)
{
  if (!is.factor(y)) stop('Y must be an R factor')
  if (is.vector(trnScores))
    trnScores <- matrix(trnScores,ncol=1)
  tsDF <- as.data.frame(trnScores)
  if (nrow(trnScores) != length(y)) {
    stop('fewer scores than Ys; did you have nonnull holdout?')
  }
  dta <- cbind(y,tsDF)
  res <- regtools::qePolyLog(dta,'y',deg=deg,maxInteractDeg=0,holdout=NULL)
}

ppc <- fitPlatt

#########################  predictPlatt()  ################################
#' Predict the new case based on a trained logistic model
#'
#' Taking the logistic model and predict on the test scores given the classifier
#'
#' @param prePlattCalibOut the logisitc model produced by fitPlatt()
#' @param tstScores a vector of decision values or probabilities given by the classifier for the testing sample
#' @param se to determine whether we want to compute the standard error through the delta method
#' @return a list that contains the following 1. prob: a vector of probabilities; 2. se: a vector of standard errors if se is set to TRUE
#' @export
predictPlatt <- function(prePlattCalibOut,tstScores,se=FALSE)
{
  if (is.vector(tstScores)) {
    tstScores <- matrix(tstScores,ncol=1)
  }
  tsDF <- as.data.frame(tstScores)
  probs <- predict(prePlattCalibOut,tsDF)$probs
  if(se) {
    SEs = list()
    for (i in 1:length(levels(as.factor(prePlattCalibOut$y))))
    {
      model = prePlattCalibOut$glmOuts[[i]]
      nscores = ncol(tstScores)

      alg = "1/(1+exp((-1)*(b0"
      for (j in 1:nscores) {
        if (j != nscores){
          alg = paste(alg,"+b",j,"*",colnames(tstScores)[j], sep = "")
        }
        else{
          alg = paste(alg,"+b",j,colnames(tstScores)[j], sep = "")
        }
      }
      alg = paste(alg,")))", sep = "")
      SE = RcmdrMisc::DeltaMethod(model,alg)$test$SE
      SEs[[i]] = SE
    }
    df.SEs = do.call(rbind, SEs)
    return(list(probs = probs, se = df.SEs))
  } else {
    return(list(probs = probs))
  }
}

#########################  plattCalib()  ################################

#' Calibrate probability based on Platt scaling
#'
#' apply ploynomial platt scaling to the posterior probabilities or decision values from SVM or other algorithms
#'
#' @param trnY the training labels
#' @param trnScores a matrix of decision values or probabilities given by the classifier for the training sample
#' @param tstScores a matrix of decision values or probabilities given by the classifier for the test sample
#' @param deg an integer to indicate the degree of polynomial for platt scaling
#' @param se to determine whether standard errors should be computed via delta method
#' @return a list that contains the following 1. prob: a vector of probabilities  2. se: a vector of standard errors if se is set to TRUE
#' @export
plattCalib <- function(trnY,
                       trnScores,
                       tstScores,
                       deg,
                       se=FALSE){
  if(length(levels(trnY)) > 2){
    stop("For multi-class case, please use ovaCalib()")
  }

  plattMod <- fitPlatt(trnY, trnScores, deg=deg)
  pred <- predictPlatt(plattMod, tstScores, se=se)
  if(se){
    list(probs = pred$probs[,2], SE = pred$se)
  }else{
    pred$probs[,2]
  }
}


# isotonic regression, AVA

# y, trnScores, newScores, value as above

ExperimentalisoCalib <- function(y,trnScores,newScores)
{
  stop('under construction')
  require(Iso)
  # find estimated regression function of yy on xx, at the values
  # newxx
  predictISO <- function(xx,yy,newxx)  # xx, yy, newxx numeric vectors
  {
    xo <- order(xx)
    xs <- xx[xo]  # sorted xx
    ys <- yy[xo]  # yy sorted according to xx
    newxxs <- matrix(newxx[xo],ncol=1)  # same for newxx
    isoout <- pava(ys)
    # get est. reg. ftn. value for each newxx; could interpolate for
    # improved accuracy, but good enough here
    minspots <- apply(newxxs,1,
                      function(newxxsi) which.min(abs(newxxsi - xs)))
    isoout[minspots]
  }
  yn <- as.numeric(y)
  do1Pair <- function(ij)
  {
    # require Iso
    # get pair
    i <- ij[1]
    j <- ij[2]
    # which has y = i or j?
    idxs <- which(yn == i | yn == j)
    # form subsets
    ys <- yn[idxs] - 1  # yn is 1s and 2s
    trnscores <- trnScores[idxs]
    # return probs for this pair
    predictISO(trnscores,ys,newScores)
  }
  pairs <- combn(length(levels(y)),2)
  apply(pairs,2,do1Pair)
}


#########################  isoCalib()  ################################
#' Calibrate probabilities based on isotonic regression
#'
#' wrapper calibrate either training scores or probability by
#' isotonic regression in binary case
#'
#' @param trnY the training labels
#' @param trnScores the training scores output by the classifier
#' @param tstScores the test scores
#' @param isProb a boolean value to determine whether scores are probabilities
#' @return a vector of probabilities
#' @export
isoCalib <- function(trnY,trnScores,tstScores, isProb)
{
  # note this package uses PAV-based isotonic regression
  if (is.vector(trnScores)){
    trnScores <- matrix(trnScores,ncol=1)
  }

  if (nrow(trnScores) != length(trnY)) {
    stop('fewer scores than Ys; did you have nonnull holdout?')
  }

  if (length(levels(trnY)) != 2){
    stop('This function can only handle binary case. For multiclass case, please use ovaCalib()')
  }

  # change the name of the levels for calibrate()
  levels(trnY) <- c("1","2")

  calibration <- CORElearn::calibrate(trnY, trnScores, class1=1,
                                      method="isoReg", assumeProbabilities=isProb)
  # apply the calibration to the testing set
  calibratedProbs <- CORElearn::applyCalibration(tstScores, calibration)
  calibratedProbs
}

#########################  bbqCalib()  ################################

#' Calibrate probabilities based on bayesian binning quantile
#'
#' wrapper calibrate either training scores or probability by
#' Bayesian Binning
#'
#' @param y R factor of labels in training set; vector of observed class labels (0/1)
#' @param trnScores vector/matrix of scores in training set
#' @param tstScores scores of new cases option either 1 or 0; averaging=1, selecting=0
#' @return a vector of probabilities
#' @export
bbqCalib <- function(trnY,trnScores, tstScores, option=1)
{
  bbqmod <-  CalibratR:::build_BBQ(trnY, trnScores)
  pred <- CalibratR:::predict_BBQ(bbqmod, tstScores, option)
  prob <- pred$predictions
  prob
}

#########################  guessCalib()  ################################

# wrapper calibrate either training scores or probability by
# a GUESS calibration model
# reference: https://cran.r-project.org/web/packages/CalibratR/CalibratR.pdf

# arguments

#    y: R factor of labels in training set;
#        vector of observed class labels (0/1)
#    trnScores: vector/matrix of scores in training set
#    tstScores: scores of new case(s)


# guessCalib <- function(trnY,trnScores, tstScores)
# {
#    require(CalibratR)

#    GUESSmod <-  CalibratR:::build_GUESS(trnY,trnScores)
#    pred <- CalibratR:::predict_GUESS(GUESSmod, tstScores)
#    prob  <- pred$predictions
#    return(prob)

# }



#########################  eliteCalib()  ################################

#' Calibration by ELiTe method
#'
#' wrapper calibrate probability by ELiTE
#'
#' @param y vector of corresponding true class 1 indicates positive class and 0 indicates negative class.
#' @param trnScores vector of uncalibrated decisions scores for training
#' @param tstScore vector of uncalibratd decisions scores for testing
#' @param build_opt 'AIC', or 'AICc' scoring functions (the Elite paper uses AICc).
#' @param pred_opt set it to 1  for running model averaging, or to 0 for running model selection
#' @return a vector of probabilities
#' @export
eliteCalib <- function(trnY,trnScores, tstScores, build_opt = "AICc", pred_opt=1)
{
  #require(devtools)
  #install_github("statsmaths/glmgen", subdir="R_pkg/glmgen")
  #require(glmgen)
  #follow instruction on
  # https://github.com/pakdaman/calibration/tree/master/ELiTE/R
  # to install EliTE
  #require(ELiTE)

  if (is.vector(trnScores)){
    trnScores <- matrix(trnScores,ncol=1)
  }


  if (nrow(trnScores) != length(trnY)) {
    stop('fewer scores than Ys; did you have nonnull holdout?')
  }

  # use the same function parameter as the elite paper
  eliteMod <- ELiTE::elite.build(trnScores, trnY, build_opt)
  prob <- ELiTE::elite.predict(eliteMod, tstScores, pred_opt)
  prob
}


#########################  getCalibMeasures()  ################################

#' Wrapper of EliTe error measure for calibration
#'
#' provides RMSE, AUROC, ACC, MCE, ECE to measure the calibration performance
#'
#' @param y vector of true class of instances {0,1}
#' @param scores vector of predictions (classification scores) which is in the closed interval 0 and 1
#' @return a dataframe that shows RMSE, AUROC, ACC, MCE, ECE
#' @export
getCalibMeasures <- function(y, scores){

  df <- as.data.frame(ELiTE::elite.getMeasures(scores, y))

  # change the colname names so that we state AUROC clearly
  colnames(df) <- c("RMSE","AUROC","ACC","MCE","ECE")

  # Compute the area under precision and recall curve
  tmp_df <- data.frame(predictions = scores, labels = y)
  pr <- PRROC::pr.curve(scores.class0=tmp_df[tmp_df$labels=="1",]$predictions,
                        scores.class1=tmp_df[tmp_df$labels=="0",]$predictions,
                        curve=F)
  # Add to the dataframe
  df$AUPRC  <- pr$auc.integral

  df
}



#########################  combineMeasures()  ################################
#' Combine all dataframes of the calibration performance measures
#'
#' the function extends getCalibMeasures to handle multi-class case when
#' the test label has more than 2 classes. It also combines results
#' from combineMeasures() for comparing different algorithms by passing the
#' output of combinMeasures() to the argument called "prev_result" in the
#' function
#'
#' @param y_test vector of true class of instances
#' @param algorithm give the name of the algorithm you used e.g. "Platt1"
#' @param probMat a matrix with each row being a probability distribution of the classes from the test data
#' @param prev_result pass dataframe returned by combineMeasures() to combine several results into one dataframe if any
#' @return a dataframe that lists all metrics for the given test labels and the calibrated probability matrix
#' @export
combineMeasures <- function(y_test, algorithm, probMat, prev_result=NULL){

  if (!is.matrix(probMat)) {
    stop('probMat must be a matrix')
  }
  count <- 1
  ls_result <- list()
  for(l in levels(y_test)){
    tmp <- ifelse(y_test==l, 1, 0)
    res <- getCalibMeasures(tmp, probMat[,count])
    res$class <- l
    res$Algorithm <- algorithm
    ls_result[[count]] <- res
    count <- count + 1
  }
  ls_result <- do.call("rbind", ls_result)
  if(!is.null(prev_result)){
    out <- rbind(prev_result, ls_result)
    return(out)
  }else{
    return(ls_result)
  }
}

####################  calibWrap() and preCalibWrap()  #############################

#' Prepare the data the for calibwrap
#'
#' preCalibWrap can be used on the orginal dataset, to set the holdout
# set, run the model etc.
#' @param dta a dataframe for the input data
#' @param yName give the column name of the label in the data
#' @param qeFtn select the desired qe function
#' @param qeArgs
#' @param holdout the number of sample being held out
#' @return a dataframe that lists all metrics for the given test labels and the calibrated probability matrix
#' @export
preCalibWrap <- function(dta,yName,qeFtn='qeSVM',qeArgs=NULL,holdout=500)
{
  qecall <- paste0('qeout <- ',qeFtn,'(dta,"',yName,'",',qeArgs,',
                   holdout=',holdout,')')
  eval(parse(text=qecall))

  tmp <- substitute({
    tstIdxs <- qeout$holdIdxs
    trnIdxs <- setdiff(1:nrow(dta),tstIdxs)
    ycol <- which(names(dta) == yName)
    trnX <- dta[trnIdxs,-ycol]
    trnY <- dta[trnIdxs,ycol]
    tstX <- dta[tstIdxs,-ycol]
    tstY <- dta[tstIdxs,ycol]

    if (qeFtn == 'qeSVM') {

      trnScores <- qeout$decision.values
      tstScores <- getDValsE1071(qeout,tstX)
      trnScores <- makeANDconjunction(trnScores,'/')
      tstScores <- makeANDconjunction(tstScores,'/')

      # e.g. polyreg feature names can't be numbers, so need to rename
      startsWithDigit <- function(s) {
        s <- substr(s,1,1)
        s >= '0' && s <= '9'
      }
      cols <- colnames(trnScores)
      if (any(sapply(cols,startsWithDigit))) {
        prependPair <- function(s) {
          tmp <- strsplit(s,'AND')[[1]]
          paste0('a',tmp[1],'AND','a',tmp[2])
        }
        colnames(trnScores) <- paste0('a',cols)
        colnames(tstScores) <- colnames(trnScores)
      }
    }
  })

  eval(tmp, parent.frame())
}

#' Calibrate probabilities with reliability diagrams
#'
#' plot the reliability diagrams and compute the calibrated probability matrix for test data.
#' @param trnScores vector/matrix of scores output from running the classification method on the training set; will have either c
#' or c(c-1)/2 columns, where c is the number of classes
#' @param tstScores scores for the data to be predicted
#' @param calibMethod currently knnCalib or plattCalib
#' @param opts R list of classification-specific parameters, e.g.list(k = 50) for knnCalib
#' @param plotsPerRow number of plots per row; 0 means no trellis plotting
#' @param oneAtATime if TRUE, show the plots one at a time, and give the user the option to print and/or zoom in.
#' @param OVA a boolean value to denote whether this is one-vs-all approach if set to False, it will use all-vs-all approach
#' @param isProb a boolean value to denote whether scores are probabiltiies
#' @param smoothingFtn a smoothing function whether it should be applied. style: 1 or 2 to denote which multicalss reliability diagram style you prefer
#' @return a list that contains a probability matrix
#' @return the reliability diagrams for each class
#' @export
calibWrap <- function(trnY,tstY, trnScores,tstScores,calibMethod,
                      opts=NULL,nBins=25,se=FALSE,plotsPerRow=0,oneAtATime=TRUE, OVA=TRUE,
                      isProb = FALSE, fitAllX = FALSE, smoothingFtn=NULL, style= NULL)
{
  classNames <- levels(trnY)
  nClass <- length(classNames)
  ym <- regtools::factorToDummies(tstY,fname='y')
  if(OVA){

    prob <- ovaCalib(trnY,
                     trnScores,
                     tstScores,
                     calibMethod,
                     deg=opts$deg,
                     K= opts$k,
                     se=se,
                     scaleX = opts$scaleX,
                     smoothingFtn = smoothingFtn,
                     isProb = isProb,
                     fitAllX=fitAllX)

  }else{
    prob <- avaCalib(trnY,
                     trnScores,
                     tstScores,
                     calibMethod,
                     deg=opts$deg,
                     K= opts$k,
                     se=se,
                     isProb = isProb)
  }

  res <- list(probs=prob)


  if (plotsPerRow) {
    nRow <- ceiling(nClass/plotsPerRow)
    par(mfrow=c(nRow,plotsPerRow))
    for (rw in 1:nRow) {
      start <- (rw - 1) * plotsPerRow + 1
      end <- min(rw * plotsPerRow,nClass)
      for (cls in start:end) {
        tmp <-
          reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE)
      }
    }
    par(mfrow=c(1,1))
  }else if (oneAtATime) {
    for (cls in 1:nClass) {
      reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE,classNum=cls)
      while (1) {
        print('you can go to the next plot, or zoom, or print to file')
        cmd <-
          readline('hit Enter for next plot, or low hi or fname: ')
        if (cmd == '') break
        cmdParts <- regtools::pythonBlankSplit(cmd)
        if (length(cmdParts) == 1)
          regtools::prToFile(cmd)
        else {
          if (cmdParts[2] == 'ly') cmdParts[2] <- as.numeric(nrow(ym))
          zoom <- as.numeric(cmdParts)
          reliabDiagram(ym[,cls],res$probs[,cls],nBins,TRUE,zoom=zoom,
                        classNum=cls)
        }
      }
    }
  }else{

    # Plot all in one plot
    # if it is not one at a time and plots per row are not specified

    reliabDiagram(tstY,res$probs,nBins,TRUE,
                  multiclass=TRUE, multiclassStyle= style)
    while (1) {
      print('you can zoom, or print to file')
      cmd <-
        readline('hit Enter for next plot, or low hi or fname: ')
      if (cmd == '') break
      cmdParts <- regtools::pythonBlankSplit(cmd)
      if (length(cmdParts) == 1)
        prToFile(cmd)
      else {
        if (cmdParts[2] == 'ly') cmdParts[2] <- as.numeric(nrow(ym))
        zoom <- as.numeric(cmdParts)
        reliabDiagram(tstY,res$probs, nBins,TRUE,zoom=zoom,
                      multiclass=TRUE, multiclassStyle= style)
      }
    }


  }
  res
}

#' replace old delimiter by 'AND' in the colnames of the decision values
#'
#' need column names of pairs of features to have 'AND' as delimiter,
#' e.g. 'CatcherANDFirst_Baseman' in the mlb data, replacing the original
#' delimiter, such as '/ for e1071
#' replace old delimiter by 'AND' in the colnames of the decision values
#'
#' @param dvals column names with delimiters
#' @param oldDelim a delimiter
#' @return column names of pairs of features to have 'AND' as delimiter
#' @export
makeANDconjunction <- function(dvals,oldDelim)
{
  colnames(dvals) <- gsub(oldDelim,'AND',colnames(dvals))
  dvals
}

##########################################################################
########################  e1017 routines  ################################
##########################################################################

#' calculcate decision values ("scores") for new cases on previously-fit
#' e1071 SVM
#'
#' for those users of the probability calibration functions on output
#' from the e1071 package, here are useful utilities
#' @param object the object output by e1071 svm
#' @param newx the test set
#' @return decision values based on SVM
#' @export
getDValsE1071 <- function(object,newx)
{
  require(e1071)
  # need to strip off non-SVM classes, if any
  toDelete <- NULL
  for (i in 1:length(class(object))) {
    if (!class(object)[i] %in% c('svm.formula','svm'))
      toDelete <- c(toDelete,i)
  }
  if (length(toDelete) > 0) class(object) <- class(object)[-toDelete]
  tmp <- predict(object,newx,decision.values=TRUE)
  attr(tmp,'decision.values')
}

#' Reliability diagram and associated compilation of statistics
#'
#' Draw the reliability diagram
#' @param y vector of Y values from training set, 0s and 1s
#' @param probs vector of estimated cond. class probabilities
#' @param nBins number of bins
#' @param plotGraph TRUE means plotting is desired
#' @param zoom a tuple of values to set the limit of the axis e.g. (0, 100)
#' @param classNum an integer to specify the class number in the title of the plot
#' @param multiclass determine whether it is a multiclass case.
#' @param multiclassStyle 1 or 2 to denote which multicalss reliability diagram style you prefer
#' @return one reliability diagram
#' @export
reliabDiagram <- function(y,probs,nBins,plotGraph=TRUE,zoom=NULL,classNum=NULL,
                          multiclass=FALSE, multiclassStyle= 1)
{
  if(multiclass){
    # Change the margin
    # this probably needs to be fixed so that it can scale with num of classes
    par(mar=c(5,4,2,6))

    ym <- regtools::factorToDummies(y,fname='y')
    nClass <- length(levels(y))
    tokencode<- rep(1:25,5)
    sizecode <- seq(0.1, 1, length.out = nClass)
    breaks <- seq(0,1,1/nBins)

    # generate colors
    if(nClass > 8){
      # requires a lot of colors
      colorParlette <- colors()[c(seq(5,150,5), 655, seq(360, 655,5))]
    }else{
      # get the most distinct 8 colors default
      colorParlette <- 1:8
    }

    record = list()
    for(class in 1:nClass){

      probsBinNums <- findInterval(probs[,class],breaks)
      fittedYCounts <- tapply(probs[,class],probsBinNums,mean)
      actualYCounts <- tapply(ym[,class],probsBinNums,mean)
      axisLimit <- 1

      # record the counts
      record[[levels(y)[class]]] <- cbind(fittedYCounts, actualYCounts)

      color <- colorParlette[class]
      size <- sizecode[class]
      token <- tokencode[class]

      if(plotGraph){

        if (nClass > 25 & multiclassStyle == 2){
          print("Repeated tokens will be used for over 25 classes")
        }

        if (is.null(zoom)) {
          zoomTo <- 1:nBins
          lims <- c(0,axisLimit)
        }else {
          ftdy <- fittedYCounts
          zoomTo <- which(ftdy >= zoom[1] & ftdy <= zoom[2])
          lims <- zoom
        }

        if (class == 1){
          if (multiclassStyle == 1){

            # Style 1: color and size varying
            plot(fittedYCounts[zoomTo], actualYCounts[zoomTo],
                 col = rep(color, length(fittedYCounts[zoomTo])), pch = 1,
                 cex = rep(size, length(fittedYCounts[zoomTo])),
                 xlim=lims,ylim=lims, xlab='Predicted Probability',
                 ylab='Actual Probability')

          }else if(multiclassStyle == 2){

            #Style 2: black dots with varying tokens
            plot(fittedYCounts[zoomTo], actualYCounts[zoomTo],
                 pch = rep(token , length(fittedYCounts[zoomTo])),
                 xlim=lims,ylim=lims, xlab='Predicted Probability',
                 ylab='Actual Probability')

          }else{
            # Style 3: color and size fixed
            plot(fittedYCounts[zoomTo], actualYCounts[zoomTo],
                 col = rep(color, length(fittedYCounts[zoomTo])), pch = 1,
                 xlim=lims,ylim=lims, xlab='Predicted Probability',
                 ylab='Actual Probability')
          }
        }else{
          if (multiclassStyle == 1){
            points(fittedYCounts[zoomTo], actualYCounts[zoomTo],
                   col =rep(color, length(fittedYCounts[zoomTo])), pch = 1,
                   cex = rep(size, length(fittedYCounts[zoomTo])))

          }else if(multiclassStyle == 2){

            points(fittedYCounts[zoomTo], actualYCounts[zoomTo],
                   pch = rep(token, length(fittedYCounts[zoomTo])))

          }else{
            points(fittedYCounts[zoomTo], actualYCounts[zoomTo],
                   col =rep(color, length(fittedYCounts[zoomTo])), pch = 1)
          }
        }
      }
    }
    if (plotGraph){
      # determine the size of the font
      if(nClass <= 20){
        fontsize = 1
      }else if(nClass > 20 & nClass <= 30){
        fontsize = 0.7
      }else if(nClass > 30 & nClass <= 40){
        fontsize = 0.5
      }else{
        fontsize = 0.2
      }

      if (multiclassStyle == 1 | multiclassStyle == 3){
        # a dash diagonal line
        abline(0,1, lty=2)
        legend("topleft", legend = levels(y), col = colorParlette[1:nClass], pch = 19,
               inset=c(1,0), xpd=TRUE, bty="n", cex = fontsize)
      }else{
        abline(0,1,col='red')
        legend("topleft", legend = levels(y), pch = tokencode[1:nClass],
               inset=c(1,0), xpd=TRUE, bty="n", cex = fontsize)
      }
    }
    # Reset the margin back to default
    par(mar=c(5,4,4,2)+0.1)
    record

  }else{
    breaks <- seq(0,1,1/nBins)
    probsBinNums <- findInterval(probs,breaks)
    fittedYCounts <- tapply(probs,probsBinNums,sum)
    actualYCounts <- tapply(y,probsBinNums,sum)
    axisLimit <- max(max(fittedYCounts),max(actualYCounts))
    if (plotGraph) {
      if (is.null(zoom)) {
        zoomTo <- 1:nBins
        lims <- c(0,axisLimit)
      }else {
        ftdy <- fittedYCounts
        zoomTo <- which(ftdy >= zoom[1] & ftdy <= zoom[2])
        lims <- zoom
      }

      plot(fittedYCounts[zoomTo],actualYCounts[zoomTo],
           xlim=lims,ylim=lims,xlab='fittedYCounts',ylab='actualYCounts')
      abline(0,1,col='red')
      if (!is.null(classNum)) {
        topLabel <- paste('Class',classNum)
        title(main=topLabel,col='blue')
      }
    }

    cbind(fittedYCounts,actualYCounts)
  }
}

#' Fit a logistic model
#'
#' Convert solved x to probability via logistic model
#' @param x the input data
#' @return the y output by a logistic model
#' @export
logOddsToProbs <- function(x)
{
  u <- exp(-x)
  1 / (1+u)
}

#' Plot ROC
#'
#' @param y labels in training set; a 0s and 1s vector
#' @param scores values that your ML algorithm predicts from
#' @return a ROC plot
ROC <- function(y,scores)
{
  n <- length(y)
  numPos <- sum(y)
  numNeg <- n - numPos
  scoreOrder <- order(scores,decreasing=T)
  tpr <- vector(length = n)
  fpr <- vector(length = n)
  for (i in 1:n) {
    # scoresSorted = sort(scores); h = scoresSorted[i]
    whichGteH <- scoreOrder[1:i]
    numTruePos <- sum(y[whichGteH] == 1)
    numFalsePos <- i - numTruePos
    tpr[i] <- numTruePos / numPos
    fpr[i] <- numFalsePos / numNeg
  }
  plot(fpr,tpr,type='l',pch=2,cex=0.5)
  abline(0,1)

}


#########################  crossEntropy()  ################################

#' calculates crossEntropy for probability calibrationn algorithms
#'
#' @param calibWrapOut output object of calibWrap
#' @return cross entropy
crossEntropy = function(calibWrapOut) {
  p = calibWrapOut$ym
  phat = calibWrapOut$probs
  x = 0
  for (i in 1:nrow(p)) {
    x = x - sum(p[i,]*log(phat[i,]))
  }
  return(x)
}

#########################  KLDivergence()  ################################

#' calculates Kullback_Leibler divergence for probability calibration
# algorithms
#'
#' @param calibWrapOut output object of calibWrap
#' @return KL divergence
KLDivergence = function(calibWrapOut) {
  require(philentropy)
  p = calibWrapOut$ym
  phat = calibWrapOut$probs
  x = 0
  for (i in 1:ncol(p)) {
    df = rbind(p[i,], phat[i,])
    x = x + philentropy::KL(df)
  }
  return(x)
}
