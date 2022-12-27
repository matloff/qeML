\name{Prediction with Missing Values}
\alias{qeKNNna}

\alias{predict.qeKNNna}

\title{Prediction with Missing Values}

\description{
ML methods for prediction in which features are subject to missing
values.
}

\usage{
qeKNNna(data,yName,k=25,minNonNA=1,holdout=floor(min(1000,0.1*nrow(data))))
\method{predict}{qeKNNna}(object,newx,kPred=1,...)
}

\arguments{
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{object}{An object of class 'qeKNNna'.}
  \item{newx}{New data to be predicted.}
  \item{holdout}{If not NULL, form a holdout set of the specified size.
     After fitting to the remaining data, evaluate accuracy on the test set.}
  \item{k}{Number of nearest neighbors in training set.} 
  \item{minNonNA}{For row y to be considered a neighbor of row x,
#       y must have at least \code{minNonNA} non-NAs in common with x}
  \item{kPred}{Number of nearest neighbors in prediction.}
}

\details{

The \code{qeKNNna} function may be useful in datasets with a large
portion of NA values.  For a new case to be predicted, it finds the
nearest neighbors of the intact components of that case.  As with
\code{qeKNN}, at the training stage, predicted values are found and
retained at each point in the training data, based on the \code{k}
neighbors of that point.  For a new case, the \code{kPred} neighbors of
that case are found, and their predicted values are averaged.

}

\examples{

z <- qeKNNna(airquality,'Ozone',holdout=NULL)
sum(is.na(airquality))  # 44 NAs, good test example
# example of new case, insert an NA in 1st row
aq2 <- airquality[2,-1]
aq2$Wind <- NA
predict(z,aq2)  # 32.68

}

\author{
Norm Matloff
}

