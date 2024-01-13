\name{Utilities}
\alias{buildQEcall}
\alias{evalr}
\alias{newDFRow}

\title{Utilities}

\description{
Miscellaneous functions, used mainly internally in the package, but of
possible use externally.}

\usage{
buildQEcall(qeFtnName,dataName,yName=NULL,opts=NULL,holdout=NULL,
    holdoutArg=TRUE) 
evalr(toexec) 
newDFRow(dta,yName,x,dtaRowNum=1)
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
  \item{toexec}{Quoted string containing an R function call.}
  \item{dta}{A data frame.}
  \item{x}{An R list specifying fields to be set.}
  \item{dtaRowNum}{Row number in 'dta' to be used as a basis.}
}

\details{

The function \code{qeFtnName} does what its name implies:  It assembles
a string consisting of a \pkg{qeML} function call.  Typically the latter
is then executed via \link{evalr}.  See for instance the source code of
\code{qeLeaveOut1Var}.

R's generic \code{predict} function generally required that the input
rows match the original training data in name and class.  The
\code{newDFRow} function can be used to construct such a row.

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
}

\author{
Norm Matloff
}

