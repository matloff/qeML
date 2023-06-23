\name{qe-Series Predictive Functions} 
\alias{qeLogit}
\alias{qeLin}
\alias{qeKNN}
\alias{qeRF}
\alias{qeRFranger}
\alias{qeRFgrf}
\alias{qeSVM}
\alias{qeGBoost}
\alias{qeAdaBoost}
\alias{qeLightGBoost}
\alias{qeNeural}
\alias{qeLASSO}
\alias{qePolyLin}
\alias{qePolyLog}
\alias{qePoly}
\alias{qeIso}
\alias{qePCA}
\alias{qeUMAP}
\alias{qeskRF}
\alias{qeskSVM}
\alias{qeParallel}
\alias{qeDT}
\alias{qeFOCI}
\alias{qeFOCIrand}
\alias{qeLinKNN}
\alias{qePolyLASSO}
\alias{qeROC}
\alias{qeXGBoost}
\alias{qeDeepnet}
\alias{qeNCVregCV}
\alias{qePolyLinKNN}
\alias{qeliquidSVM}
\alias{checkPkgLoaded}

\alias{predict.qeLogit}
\alias{predict.qeLin}
\alias{predict.qeKNN}
\alias{predict.qeRF}
\alias{predict.qeRFranger}
\alias{predict.qeRFgrf}
\alias{predict.qeSVM}
\alias{predict.qeGBoost}
\alias{predict.qeLightGBoost}
\alias{predict.qeAdaBoost}
\alias{predict.qeNeural}
\alias{predict.qeLASSO}
\alias{predict.qePolyLin}
\alias{predict.qePolyLog}
\alias{predict.qeIso}
\alias{predict.qePCA}
\alias{predict.qeUMAP}
\alias{predict.qeskRF}
\alias{predict.qeskSVM}
\alias{predict.qeDeepnet}
\alias{predict.qeParallel}
\alias{predict.qeNCVregCV}
\alias{predict.qePolyLinKNN}
\alias{predict.qeliquidSVM}
\alias{predict.qePoly}
\alias{plot.qePoly}
\alias{plot.qeRF}
\alias{plot.qeLASSO}

\title{Quick-and-Easy Machine Learning Wrappers}

\description{
Quick access to machine learning methods, with a very simple
interface.  "Works right out of the box!":
Just one call needed to fit, no preliminary
setup of model etc.  The simplicity also makes the series useful
for teaching.  

}

\usage{
qeLogit(data,yName,holdout=floor(min(1000,0.1*nrow(data))),yesYVal=NULL)
qeLin(data,yName,noBeta0=FALSE,holdout=floor(min(1000,0.1*nrow(data))))
qeKNN(data,yName,k,scaleX=TRUE,smoothingFtn=mean,yesYVal=NULL,
   expandVars=NULL,expandVals =NULL,holdout=floor(min(1000,0.1*nrow(data))))
qeRF(data,yName,nTree=500,minNodeSize=10,mtry=floor(sqrt(ncol(data)))+1,
   holdout=floor(min(1000,0.1*nrow(data))))
qeRFranger(data,yName,nTree=500,minNodeSize=10,
   mtry=floor(sqrt(ncol(data)))+1,deweightPars=NULL,
   holdout=floor(min(1000,0.1*nrow(data))),yesYVal="") 
qeRFgrf(data,yName,nTree=2000,minNodeSize=5,mtry=floor(sqrt(ncol(data)))+1,
   ll=FALSE,lambda=0.1,splitCutoff=sqrt(nrow(data)),
   holdout=floor(min(1000,0.1*nrow(data))))
qeskRF(data,yName,nTree=500,minNodeSize=10,
   holdout=floor(min(1000,0.1*nrow(data))))
qeSVM(data,yName,gamma=1.0,cost=1.0,kernel='radial',degree=2,
   allDefaults=FALSE,holdout=floor(min(1000,0.1*nrow(data))))
qeskSVM(data,yName,gamma=1.0,cost=1.0,kernel='rbf',degree=2,
   holdout=floor(min(1000,0.1*nrow(data))))
qeGBoost(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
qeAdaBoost(data, yName, treeDepth = 3, nRounds = 100, rpartControl = NULL, 
    holdout = floor(min(1000, 0.1 * nrow(data)))) 
qeLightGBoost(data,yName,nTree=100,minNodeSize=10,learnRate=0.1,
   holdout=floor(min(1000,0.1*nrow(data))))
qeNeural(data,yName,hidden=c(100,100),nEpoch=30,
   acts=rep("relu",length(hidden)),learnRate=0.001,
   conv=NULL,xShape=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
qeLASSO(data,yName,alpha=1,holdout=floor(min(1000,0.1*nrow(data))))
qePolyLin(data,yName,deg=2,maxInteractDeg = deg,
   holdout=floor(min(1000,0.1*nrow(data))))
qePolyLog(data,yName,deg=2,maxInteractDeg = deg,
   holdout=floor(min(1000,0.1*nrow(data))))
qeFT(data,yName,qeftn,pars,nCombs,nTst,nXval,showProgress=TRUE)
qePCA(data,yName,qeName,opts=NULL,pcaProp,
   holdout=floor(min(1000,0.1*nrow(data))))
qeUMAP(data,yName,qeName,opts=NULL,
   holdout=floor(min(1000,0.1*nrow(data))),scaleX=FALSE,
   nComps=NULL,nNeighbors=NULL)
qeDT(data,yName,alpha=0.05,minsplit=20,minbucket=7,maxdepth=0,mtry=0,
   holdout=floor(min(1000,0.1*nrow(data))))
qeFOCI(data,yName,numCores=parallel::detectCores(),parPlat="none",
   yesYLevel=NULL)
qeFOCIrand(data,yName,xSetSize,nXSets)
qeLinKNN(data,yName,k=25,scaleX=TRUE,smoothingFtn=mean,
   expandVars=NULL,expandVals=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
qePolyLASSO(data,yName,deg=2,maxInteractDeg=deg,alpha=0,
   holdout=floor(min(1000,0.1*nrow(data))))
qeROC(dataIn,qeOut,yName,yLevelName)
qeXGBoost(data,yName,nRounds=250,params=list(),
   holdout=floor(min(1000,0.1*nrow(data))))
qeDeepnet(data,yName,hidden=c(10),activationfun="sigm",
   learningrate=0.8,momentum=0.5,learningrate_scale=1,
   numepochs=3,batchsize=100,hidden_dropout=0,yesYVal=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
checkPkgLoaded(pkgName,whereObtain='CRAN') 
\method{predict}{qeLogit}(object,newx,...)
\method{predict}{qeLin}(object,newx,useTrainRow1=TRUE,...)
\method{predict}{qeKNN}(object,newx,newxK=1,...)
\method{predict}{qeRF}(object,newx,...)
\method{predict}{qeRFranger}(object,newx,...)
\method{predict}{qeRFgrf}(object,newx,...)
\method{predict}{qeSVM}(object,newx,...)
\method{predict}{qeGBoost}(object,newx,newNTree=NULL,...)
\method{predict}{qeLightGBoost}(object,newx,...)
\method{predict}{qeNeural}(object,newx,k=NULL,...)
\method{predict}{qeLASSO}(object,newx,...)
\method{predict}{qePoly}(object,newx)
\method{predict}{qePCA}(object,newx,...)
\method{predict}{qeUMAP}(object,newx,...)
\method{predict}{qeDeepnet}(object,newx,...)
\method{plot}{qeLASSO}(x,...)
\method{plot}{qeRF}(x,...)
}

\arguments{
  \item{...}{Further arguments.}
  \item{hidden_dropout}{Drop out fraction for hidden layer.}
  \item{batchsize}{Batch size.}
  \item{numepochs}{Number of iterations to conduct.}
  \item{learningrate}{Learning rate.}
  \item{momentum}{Momemtum}
  \item{learningrate_scale}{Learning rate will be multiplied by this at
     each iteration, allowing for decay.}
  \item{activationfun}{Can be 'sigm', 'tanh' or 'linear'.}
  \item{newNTree}{Number of trees to use in prediction.}
  \item{newxK}{If predicting new cases, number of nearest neighbors to
     smooth in the object returned by \code{qeKNN}.}
  \item{useTrainRow1}{If TRUE, take names in \code{newx} from row 1 in
     the training data.}
  \item{newx}{New data to be predicted.}
  \item{object}{An object returned by a qe-series function.}
  \item{minsplit}{Minimum number of data points in a node.}
  \item{minbucket}{Minimum number of data points in a terminal node.}
  \item{maxdepth}{Maximum number of levels in a tree.}
  \item{qeName}{Name of qe-series predictive function.}
  \item{conv}{R list specifying the convolutional layers, if any.}
  \item{deweightPars}{Values for de-emphasizing variables in a split,
     e.g. 'list(age=0.2,gender=0.5)'.}
  \item{allDefaults}{Use all default values of the wrapped function.}
  \item{expandVars}{Columns to be emphasized.}
  \item{expandVals}{Emphasis values; a value less than 1 means de-emphasis.}
  \item{mtry}{Number of variables randomly tried at each split.}
  \item{yesYVal}{Y value to be considered "yes," to be coded 1 rather than 0.}
  \item{yesYLevel}{Y value to be considered "yes," to be coded 1 rather than 0.}
  \item{noBeta0}{No intercept term.}
  \item{pcaProp}{Desired proportion of overall variance for the PCs.`}
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{dataIn}{See \code{data}.}
  \item{qeOut}{Output from a qe-series function.}
  \item{yName}{Name of the class labels column.}
  \item{holdout}{If not NULL, form a holdout set of the specified size.
     After fitting to the remaining data, evaluate accuracy on the test set.}
  \item{k}{Number of nearest neighbors. In functions other than
     \code{qeKNN} for which this is an argument, it is the number of 
     neighbors to use in finding conditional probabilities via 
     \code{knnCalib}.} 
  \item{smoothingFtn}{As in \code{kNN}.}
  \item{scaleX}{Scale the features.} 
  \item{nTree}{Number of trees.} 
  \item{minNodeSize}{Minimum number of data points in a tree node.} 
  \item{learnRate}{Learning rate.} 
  \item{hidden}{Vector of units per hidden layer.  Fractional values
     indicated dropout proportions.  Can be specified as a string, e.g.
     '100,50', for use with \code{qeFT}.} 
  \item{nEpoch}{Number of iterations in neural net.}
  \item{acts}{Vector of names of the activation functions, one per
     hidden layer.  Choices inclde 'relu', 'sigmoid', 'tanh', 'softmax',
     'elu', 'selu'.}
  \item{alpha}{In the case of \code{qeDT}, a p-value cutoff criterion.
     Otherwise 1 for LASSO, 2 for ridge.}
  \item{gamma}{Scale parameter in \code{e1071::svm}.}
  \item{cost}{Cost parameter in \code{e1071::svm}.}
  \item{kernel}{In the case of \code{qeSVM}, this is 
     One of 'linear','radial','polynomial' and 'sigmoid'.}  
  \item{degree}{Degree of SVM polynomial kernel, if any.}
  \item{opts}{R list of optional arguments for none, some or all of th
     functions in \code{qeFtnList}.}
  \item{qeftn}{Quoted string, specifying the name of a qe-series
     machine learning method.}
  \item{pars}{R list of hyperparameter ranges.  See 
     \code{regtools::fineTuning}.}
  \item{nCombs}{Number of hyperparameter combinations to run.  
     See \code{regtools::fineTuning}.}
  \item{nTst}{Number of data points to be in the test set..  
     See \code{regtools::fineTuning}.}
  \item{nXval}{Number of cross-validations to run.  
     See \code{regtools::fineTuning}.}
  \item{showProgress}{If TRUE, show results as they arise.  
     See \code{regtools::fineTuning}.}
  \item{nComps}{Number of UMAP components to extract.}
  \item{nNeighbors}{Number of nearest neighbors to use in UMAP.}
  \item{ll}{If TRUE, use local linear forest.}
  \item{lambda}{Ridge lambda for local linear forest.}
  \item{splitCutoff}{For leaves smaller than this value, do not fit
     linear model.  Just use the linear model fit to the entire dataset.}
  \item{xShape}{Input X data shape, e.g. c(28,28) for 28x28 grayscale
     images.  Must be non-NULL if \code{conv} is.}
  \item{treeDepth}{Number of levels in each tree.}
  \item{nRounds}{Number of boosting rounds.}
  \item{rpartControl}{An R list specifying properties of fitted trees.}
  \item{numCores}{Number of cores to use in parallel computation.}
  \item{parPlat}{Parallel platforParallel platform.  Valid values are
     'none', 'cluster' (output of \code{parallel::makeCluster}), and 
     'locThreads' (local cores).}
  \item{xSetSize}{Size of subsets of the predictor variables.}
  \item{nXSets}{Number of subsets of the predictor variables.}
  \item{deg}{Degree of a polynomial.}
  \item{maxInteractDeg}{Maximul degree of interaction terms in
     a polynomial.}
  \item{yLevelName}{Name of the class to be considered a positive
     response in a classification problem.}
  \item{params}{Tuning parameters for \code{xgboost}.}
  \item{pkgName}{Name of wrapped package.}
  \item{whereObtain}{Location.}
  \item{x}{A qe-series function return object.}
}

\details{

As noted, these functions are intended for quick, first-level analysis
of regression/machine learning problems.  Emphasis here is
on convenience and simplicity.  

The idea is that, given a new dataset, the analyst can quickly and
easily try fitting a number of models in succession, say first k-NN,
then random forests: 

\preformatted{
# built-in data on major league baseball players
> data(mlb)  
> mlb <- mlb[,3:6]  # position, height, weight, age

# fit models
> knnout <- qeKNN(mlb,'Weight',k=25)
> rfout <- qeRF(mlb,'Weight')

# mean abs. pred. error on holdout set, in pounds
> knnout$testAcc
[1] 11.75644
> rfout$testAcc
[1] 12.6787

# predict a new case
> newx <- data.frame(Position='Catcher',Height=73.5,Age=26)
> predict(knnout,newx)
       [,1]
[1,] 204.04
> predict(rfout,newx)
      11 
199.1714

# many of the functions include algorithm-specific output
> lassout <- qeLASSO(mlb,'Weight')
holdout set has  101 rows
> lassout$testAcc
[1] 14.27337
> lassout$coefs  # sparse result?
10 x 1 sparse Matrix of class "dgCMatrix"
                                    s1
(Intercept)               -109.2909416
Position.Catcher             0.4408752
Position.First_Baseman       4.8308437
Position.Outfielder          .        
Position.Relief_Pitcher      .        
Position.Second_Baseman     -0.7846501
Position.Shortstop          -4.2291338
Position.Starting_Pitcher    .        
Height                       4.0039114
Age                          0.5352793

}

The \code{holdout} argument triggers formation of a holdout set
and the corresponding cross-validation evaluation of predictive power.
Note that if a holdout is formed, the return value will consist of the
fit on the training set, not on the full original dataset.

The \code{qe*} functions do model fit.  Each of them has a
\code{predict} method, and some also have a \code{plot} method.

Arguments for \code{qe*} are at least: 

\itemize{
   \item \code{data} 
   \item \code{yName} 
   \item \code{holdout}
}

Typically there are also algorithm-specific hyperparameter arguments.

Arguments for \code{predict} are at least:

\itemize{
   \item \code{object}, the return value from \code{qe*}
   \item \code{newx}, a data frame of points to be predicted
}

For both the fitting function and the prediction function, there may be
additional algorithm-specific parameters; default values are provided.

Some notes on specific functions:

\itemize{

   \item The function \code{qeLin} handles not only the usual OLS models
   but also classification problems as multivariate-outcome linear
   models. If one's goal is prediction, it can be much faster than
   \code{qeLogit}, often with comparable accuracy.

   \item Regularization in linear/generalized linear models is
   implemented in \code{qeLASSO} and other functions with names
   containing 'LASSO', as well as \code{qeNCVregCV}.  The latter,
   wrappping the MCP and other regularization methods, wraps the package
   of the same name.
   
   \item Several functions fit polynomial models.  The \code{qePolyLin}
   function does polynomial regression of the indicated degree. In the
   above example degree 3 means all terms through degree 3, e.g.
   \code{Height * Age^2}.  Dummy variables are handled properly, e.g.
   no powers of a dummy are generatd.  The logistic polynomial
   regression version is \code{qePolyLog}, and there is a LASSO version,
   \code{qePolyLASSO}.
   
   \item Several SVM implementations are offered:  \code{qeSVM} wraps
   the SVM algorithm in the \pkg{e1071} package; \code{qeskSVM} wraps
   \code{svm.SVC} in the Python \pkg{sklearn}.  (Both \code{qeSVM} and
   \code{qeskSVM} use the \code{libsvm} C library.)  There is also
   \code{qeliquidSVM}, wrapping the package \pkg{liquidSVM}.

   \item Several random forests implementations are offered:
   \code{qeRF} wraps \code{randomForest} in the package of the same name; 
   \code{qeRFranger} wraps \code{ranger} in the package of the same name; 
   \code{qeRFgrf} wraps \code{regression_forest} and 
   \code{ll_regression_forest} in \pkg{grf} (the latter does local
   linear smoothing); \code{qeskRF} wraps \code{ensemble.RandomForestRegressor}
   and \code{RandomForestClassifier} in the Python \pkg{sklearn}.  There
   is also \code{qeDT}, using the \pkg{party} package.

   \item Several implementations of gradient boosting are offered,
   including \code{qeGBoost} using the \pkg{gbm} package, 
   \code{qelightGBoost} using \pkg{lightgbm}, and \code{qeXGBoost}
   wrapping \pkg{xgboost}.

   \item Several functions involve dimension reduction/feature
   selection.  Pre-mapping to lower-dimensional manifolds can be done via
   \code{qePCA} and \code{qeUMAP}.  For instance, the former will first
   extract the specified number of principal components, then fit the
   user's desired ML model, say k-NN (\code{qeKNN}) or gradient boosting
   (\code{qeGBoost}).  The \code{qeFOCI} function does feature selection
   in a basically assumption-free manner.

   \item Neural network models are implemented by \code{qeNeural}
   and \code{qeDeepnet}, based on \pkg{keras} and \pkg{deepnet}.

   \item The \code{qeLinKNN} function offers a hybrid approach.  It
   first fits a linear model, then applies k-Nearest Neighbors to the
   residuals.  The \code{qePolyLinKNN} function does the same in with a
   polynomial fit.
   
   \item The \code{qeIso} function is intended mainly for use as a
   smoothing method in calibration actions.  }
}

In most cases, the full basket of options in the wrapped function is not
reflected.  Use of arguments not presented in the qe function requires
direct use the relevant packages.

}

\value{

The value returned by \code{qe*} functions depends on the algorithm, but
with some commonality, e.g. \code{classif}, a logical value indicating
whether the problem was of classification type.  

If a holdout set was requested, an additional returned component will be
\code{testAcc}, the accuracy on the holdout set.  This will be Mean
Absolute Prediction Error in the regression case, and proportion of
misclassified cases in the classification case.

The value returned by the \code{predict} functions is an
R list with components as follows:

Classification case:

\itemize{

\item \code{predClasses}:  R factor instance of predicted class labels 

\item \code{probs}:  vector/matrix of class probabilities; in the 2-class
case, a vector, the probabilities of Y = 1

}

Regression case: vector of predicted values

}

\examples{

# see also 'details' above

\dontrun{

data(peFactors)  
pef <- peFactors[,c(1,3,5,7:9)]  
# most people in the dataset have at least a Bachelor's degree; so let's
# just consider Master's (code 14) and PhD (code 16) as special
pef$educ <- toSubFactor(pef$educ,c('14','16'))  

# predict occupation; 6 classes, 100, 101, 102, 106, 140, 141, using SVM
svmout <- qeSVM(pef,'occ',holdout=NULL) 
# as example of prediction, take the 8th case, but change the gender and
# age to female and 25; note that by setting k to non-null, we are
# requesting that conditional probabilities be calculated, via
# knnCalib(), here using 25 nearest neighbors
newx <- pef[8,-3] 
newx$sex <- '2'
newx$age <- 25
predict(svmout,newx,k=25)
# $predClasses
#   8 
# 100 
# Levels: 100 101 102 106 140 141
# $dvals
#      102/101    102/100   102/141  102/140  102/106    101/100  101/141
# 8 -0.7774038 -0.5132022 0.9997894 1.003251 0.999688 -0.4023077 1.000419
#    101/140   101/106  100/141  100/140  100/106   141/140    141/106   140/106
# 8 1.000474 0.9997371 1.000088 1.000026 1.000126 0.9460703 -0.4974625 -1.035721
# 
# $probs
#       100  101  102  106 140  141
# [1,] 0.24 0.52 0.12 0.08   0 0.04
#
# so, occupation code 100 is predicted, with a 0.36 conditional
# probability

# if holdout evaluation is desired as well, say 1000 cases, seed 9999:
> svmout <- qeSVM(pef,'occ',holdout=c(1000,9999)) 
> svmout$testAcc
[1] 0.622  # 62% error rate (overall rate for 6 classes)

# linear
# lm() doesn't like numeric factor levels, so prepend an 'a'
pef$occ <- prepend('a',pef$occ)
lmout <- qeLin(pef,'occ')
predict(lmout,pef[1,-3])  # occ 100, prob 0.3316
lmout <- qeLin(pef,'wageinc')
predict(lmout,pef[1,-5])  # 70857.79

}

}

\author{
Norm Matloff
}

