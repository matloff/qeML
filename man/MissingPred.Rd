\name{Prediction with Missing Values}
\alias{qeLinMV}
\alias{qeLogitMV}
\alias{qeKNNMV}
\alias{predict.qeLinMV}
\alias{predict.qeLogitMV}
\alias{predict.qeKNNMV}

\title{Prediction with Missing Values}

\description{
ML methods for prediction in which features are subject to missing
values.
}

\usage{
qeLinMV(data,yName)
qeLogitMV(data,yName,yesYVal)
qeKNNMV(data,yName,kmax)
\method{predict}{qeLinMV}(object,newx,...)
\method{predict}{qeLogitMV}(object,newx,...)
\method{predict}{qeKNNMV}(object,newx,...)
}

\arguments{
  \item{...}{Further arguments.}
  \item{object}{An object returned by one of the \code{qe*MV} functions.}
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{newx}{New data to be predicted.}
  \item{kmax}{Number of nearest neighbors in training set.} 
  \item{yesYVal}{Y value to be considered "yes," to be coded 1 rather than 0.}
}

\details{

These are wrappers to the \pkg{toweranNA} package.  Linear, logistic and
kNN interfaces are available.

}

\examples{

sum(is.na(airquality))  # 44 NAs, good test example
z <- qeKNNMV(airquality,'Ozone',10)
# example of new case, insert an NA in 1st row
aq2 <- airquality[2,-1]
aq2$Wind <- NA
predict(z,aq2)  # 28.1

}

\author{
Norm Matloff
}

