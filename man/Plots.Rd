\name{Advanced Plots}
\alias{plotPairedResids}
\alias{plotClassesUMAP}
\alias{qeFreqParcoord}

\title{Advanced Plots}

\description{
Miscellaneous specialized plots.
}

\usage{
plotPairedResids(data,qeOut) 
plotClassesUMAP(data,classVar) 
qeFreqParcoord(dataName,k=25,opts=NULL)
}

\arguments{
  \item{data}{A data frame or equivalent.}
  \item{qeOut}{An object returned from one of the qe-series
     predictive functions..}
  \item{classVar}{Name of the column containing class information.}
  \item{dataName}{Quoted name of a data frame.}
  \item{k}{Number of nearest neighbors.}
  \item{opts}{Options to be passed to \code{discparcoord}.}
}

\details{

The \code{plotPairedResids} function plots model residuals against pairs
of features, for example for model validation.  Pairs are chosen
randomly.

The function \code{qeFreqParcoord} is a \code{qeML} interface to the
\code{cdparcoord} package.

}

\examples{

\dontrun{
data(pef)
linout <- qeLin(pef,'wageinc')
plotPairedResids(pef,linout)
}

}

\author{
Norm Matloff
}

