\name{Variable Importance Measures}
\alias{qeLeaveOut1Var}

\title{Variable Importance Measures}

\description{
Various approaches to assessing relative importance of one's features.}

\usage{
qeLeaveOut1Var(data,yName,qeFtnName,nReps,opts=list())
}

\arguments{
  \item{data}{Dataframe, training set. Classification case is signaled
     via labels column being an R factor.}
  \item{yName}{Name of the class labels column.}
  \item{qeFtnName}{Quoted \code{qe*} function name.}
  \item{nReps}{Number of holdout sets to generate.}
  \item{opts}{R list of optional arguments for none, some or all of th
     functions in \code{qeFtnList}.}
}

\details{

Many methods have been developed assessing relative importance of one's
features.  A few that we consider most useful are accessible here.

As a quick assessment, the \code{qeLeave1VarOut} function, with call
form as above, simply compares predictive ability with and without
the given feature.

Some methods rely on reweighting:

\itemize{

   \item \code{qeKNN} 

   \item \code{qeRFranger}

}

Others make use of order of entry of a variable into the prediction
model:

\itemize{

   \item \code{qeFOCI}

   \item \code{qeLASSO}

}

}

\examples{

data(pef)
qeLeaveOut1Var(pef,'wageinc','qeLin',5)
# in order of impact, wkswrkd largest, then education etc.

}


\author{
Norm Matloff
}

