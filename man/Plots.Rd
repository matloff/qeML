\name{Advanced Plots}
\alias{plotPairedResids}
\alias{plotClassesUMAP}

\title{Advanced Plots}

\description{
Miscellaneous specialized plots.
}

\usage{
plotPairedResids(data,qeOut) 
plotClassesUMAP(data,classVar) 
}

\arguments{
  \item{data}{A data frame or equivalent.}
  \item{qeOut}{An object returned from one of the qe-series
     predictive functions..}
  \item{classVar}{Name of the column containing class information.}
}

\details{

The \code{plotPairedResids} function plots model residuals against pairs
of features, for example for model validation.  Pairs are chosen
randomly.

}

\examples{

data(pef)
linout <- qeLin(pef,'wageinc')
plotPairedResids(pef,linout)


}

\author{
Norm Matloff
}

