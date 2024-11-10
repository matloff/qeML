\name{Utilities}
\alias{buildQEcall}
\alias{evalr}
\alias{newDFRow}
\alias{replicMeansMatrix}
\alias{Data}
\alias{wideToLongWithTime}

\title{Utilities}

\description{
Miscellaneous functions, some used mainly internally in the package, but of
possible use externally.}

\usage{
buildQEcall(qeFtnName,dataName,yName=NULL,opts=NULL,holdout=NULL,
    holdoutArg=TRUE) 
evalr(toexec) 
newDFRow(dta,yName,x,dtaRowNum=1)
replicMeansMatrix(nReps,cmd,nCols=NULL)
Data(datasetName)
wideToLongWithTime(data,timeColName,timeColPresent=TRUE,
   timeColSeq=c(1,1),grpColName=NULL,valueColName=NULL)
}

\arguments{
  \item{data}{Data frame or equivalent. Other than a "time" column,
     all columns must be numeric, one column per
     group (see below).}
  \item{qeFtnName}{Quoted name of a \pkg{qeML} predictive function.}
  \item{dataName}{Quoted name of a data frame.}
  \item{yName}{Quoted name of a column to be predicted.}
  \item{opts}{Non-default arguments for the function specified
     in \code{qeFtnName}.}
  \item{holdout}{Size of holdout set, if any.}
  \item{holdoutArg}{A value TRUE means the function specified in 
     \code{qeFtnName} has an argument 'holdout'.}
  \item{toexec}{Quoted string containing an R function call.}
  \item{nReps}{Number of replications.}
  \item{cmd}{Quoted string containing an R function call. If multiple
     statements, enclose with braces.}
  \item{nCols}{Number of columns for output.}
  \item{dta}{A data frame.}
  \item{x}{An R list specifying fields to be set.}
  \item{dtaRowNum}{Row number in 'dta' to be used as a basis.}
  \item{datasetName}{Quoted string of dataset to be loaded.}
}

\details{

The function \code{qeFtnName} does what its name implies:  It assembles
a string consisting of a \pkg{qeML} function call.  Typically the latter
is then executed via \link{evalr}.  See for instance the source code of
\code{qeLeaveOut1Var}.

R's generic \code{predict} function generally required that the input
rows match the original training data in name and class.  The
\code{newDFRow} function can be used to construct such a row.

The function \code{replicMeansMatrix} will eventually replace
\code{regtools::replicMeans}.  It runs the specified code many times,
with the code assumed to have some random component such as in
simulation or in investigation of a random algorithm.

The function \code{Data} is a convenience function that combines calls
to \code{data} and \code{str}.

The function \code{wideToLongWithTime} plot "Y" versus "X" for different
groups, one per column in \code{data} other than "X", if present. "X" is
typically time but can be any ordered numeric quantity. If "X" is not
present, the arguments specify how the user wants one created.

}

\examples{

# function to list all the objects loaded by the specified package
lsp <- function(pkg) {
   cmd <- paste('ls(package:',pkg,')')
   evalr(cmd)
}
lsp('regtools')
# outputs
#  [1] "clusterApply"        "clusterApplyLB"      "clusterCall"        
#  [4] "clusterEvalQ"        "clusterExport"       "clusterMap"         
# ...

# mean of scalar quantity
replicMeansMatrix(1000,'rnorm(1)^2')
# mean of vector quantity
replicMeansMatrix(1000,'c(rnorm(1),rnorm(1)^2)',2)
# mean of matrix quantity
replicMeansMatrix(1000,
   '{z1=rnorm(1); z2=rnorm(1); x=z1; y=z1+z2; rbind(c(x,x^2),c(y,y^2))}',2) 

data(currency)
zLong <- wideToLongWithTime(currency,'weeknum',timeColPresent=FALSE,
   grpColName='country',valueColName='rate')

}

\author{
Norm Matloff
}

