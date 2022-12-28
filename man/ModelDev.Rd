\name{FeatureModelSelect}
\alias{qeCompare}
\alias{qeFT}
\alias{qeText}
\alias{qeTS}

\alias{predict.qeText}
\alias{predict.qeTS}

\title{Feature Selection and Model Building}

\description{
Utilties to help build models.
}

\usage{
qeCompare(data,yName,qeFtnList,nReps,opts=NULL,seed=9999)
qeFT(data,yName,qeftn,pars,nCombs,nTst,nXval,showProgress=TRUE)
qeText(data,yName,kTop=50,stopWords=tm::stopwords("english"),
   qeName,opts=NULL,holdout=floor(min(1000,0.1*nrow(data))))
qeTS(lag,data,qeName,opts=NULL,holdout=floor(min(1000,0.1*length(data))))
\method{predict}{qeText}(object,newDocs,...)
\method{predict}{qeTS}(object,newx,...)
}

\arguments{
  \item{newx}{New data to be predicted.}
  \item{newDocs}{Vector of new documents to be predicted.}
  \item{lag}{number of recent values to use in predicting the next.}
  \item{qeName}{Name of qe-series predictive function, e.g. 'qeRF'.}
  \item{stopWords}{Stop lists to use.}
  \item{nTst}{Number of parameter combinations.}
  \item{kTop}{Number of most-frequent words to use.}
  \item{pcaProp}{Desired proportion of overall variance for the PCs.`}
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
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
  \item{alpha}{1 for LASSO, 2 for ridge.}
  \item{gamma}{Scale parameter in \code{e1071::svm}.}
  \item{cost}{Cost parameter in \code{e1071::svm}.}
  \item{kernel}{One of 'linear','radial','polynomial' and 'sigmoid'.}
  \item{degree}{Degree of SVM polynomial kernel, if any.}
  \item{qeFtnList}{Character vector of \code{qe*} names.}
  \item{nReps}{Number of holdout sets to generate.}
  \item{opts}{R list of optional arguments for none, some or all of th
     functions in \code{qeFtnList}.}
  \item{seed}{Seed for random number generation.}
  \item{qeftn}{Quoted string, specifying the name of a qe-series
     machine learning method.}
  \item{pars}{R list of hyperparameter ranges.  See 
     \code{regtools::fineTuning}.}
  \item{nCombs}{Number of hyperparameter combinations to run.  
     See \code{regtools::fineTuning}.}
  \item{nTsts}{Size of test sets.  See \code{regtools::fineTuning}.}
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
  \item{yYesName}{For a binary classification problem, the value to
     be considered "yes" in probability reports.}
}

\author{
Norm Matloff
}

