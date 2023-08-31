\name{Utilities}
\alias{buildQEcall}

\title{Utilities}

\description{
Miscellaneous functions, used mainly internally in the package, but of
possible use externally.}

\usage{
buildQEcall(qeFtnName,dataName,yName=NULL,opts=NULL,holdout=NULL,
    holdoutArg=TRUE) 
}

\arguments{
  \item{qeFtnName}{Quoted name of a \pkg{qeML} predictive function.}
  \item{dataName}{Quoted name of a data frame.}
  \item{yName}{Quoted name of a column to be predicted.}
  \item{opts}{Non-default arguments for the function specified
     in \code{qeFtnName}.}
  \item{holdout}{Size of holdout set, if any.}
  \item{holdoutArg}{A value TRUE means the function specified in 
     \code{qeFtnName} has an argument 'holdout'.}
}

\details{

The function \code{qeFtnName} does what its name implies:  It assembles
a string consisting of a \pkg{qeML} function call.  Typically the latter
is then executed via \link{evalr}.  See for instance the source code of
\code{qeLeaveOut1Var}.

}

\author{
Norm Matloff
}

