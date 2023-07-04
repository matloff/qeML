\name{Feature Model Select}
\alias{qeText}
\alias{qeTS}
\alias{qeCompare}
\alias{qeFT}

\alias{predict.qeText}
\alias{predict.qeTS}

\title{Feature Selection and Model Building}

\description{
Utilties to help build models, both in specific applications such as
time series and text analysis, and in general tools..
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
  \item{...}{Further arguments.}
  \item{object}{Object returned by a qe-series function.}
  \item{newx}{New data to be predicted.}
  \item{newDocs}{Vector of new documents to be predicted.}
  \item{lag}{number of recent values to use in predicting the next.}
  \item{qeName}{Name of qe-series predictive function, e.g. 'qeRF'.}
  \item{stopWords}{Stop lists to use.}
  \item{nTst}{Number of parameter combinations.}
  \item{kTop}{Number of most-frequent words to use.}
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{holdout}{If not NULL, form a holdout set of the specified size.
     After fitting to the remaining data, evaluate accuracy on the test set.}
  \item{qeFtnList}{Character vector of \code{qe*} function names.}
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
  \item{nXval}{Number of cross-validations to run.  
     See \code{regtools::fineTuning}.}
  \item{showProgress}{If TRUE, show results as they arise.  
     See \code{regtools::fineTuning}.}
}

\details{

Overviews of the functions:

   \itemize{
   
      \item \code{qeTs} is a tool for time series modeling
   
      \item \code{qeText} is a tool for textual modeling
   
      \item \code{qeCompare} facilitates comparison among models
   
      \item \code{qeFT} does a random grid search for optimal hyperparameter
      values
   
   }

}

\examples{

data(mlb1)
# predict Weight in the mlb1 dataset, using qeKNN, with k = 5 and 25,
# with 10 cross-validations
qeFT(mlb1,'Weight','qeKNN',list(k=c(5,25)),nTst=100,nXval=10)


}


\author{
Norm Matloff
}

