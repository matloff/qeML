

# part of the 'calibtools' package by Norm Matloff, Kenneth Lee and
# Rongkui Han

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

