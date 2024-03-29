\name{Advanced Plots}
\alias{plotPairedResids}
\alias{plotClassesUMAP}
\alias{qeFreqParcoord}
\alias{qePlotCurves}

\title{Advanced Plots}

\description{
Miscellaneous specialized plots.
}

\usage{
plotPairedResids(data,qeOut) 
plotClassesUMAP(data,classVar) 
qeFreqParcoord(dataName,k=25,opts=NULL)
qePlotCurves(curveData,xCol=1,yCol=2,grpCol=3,
   xlab=names(curveData)[xCol],ylab=names(curveData)[yCol],
   loess=TRUE,legendSpace=1.1,legendPos='topright')
}

\arguments{
  \item{data}{A data frame or equivalent.  "X" and "Y" columns, followed
     by a group column, an R factor.}
  \item{qeOut}{An object returned from one of the qe-series
     predictive functions..}
  \item{classVar}{Name of the column containing class information.}
  \item{dataName}{Quoted name of a data frame.}
  \item{k}{Number of nearest neighbors.}
  \item{opts}{Options to be passed to \code{discparcoord}.}
  \item{curveData}{Data to be plotted.}
  \item{xCol}{Column containing "X".}
  \item{yCol}{Column containing "Y".}
  \item{grpCol}{Column containing group name, a character vector or
     factor.}
  \item{xlab}{X-axis label.}
  \item{ylab}{Y-axis label.}
  \item{loess}{If TRUE, do loess smoothing within each group.}
  \item{legendSpace}{Factor by which to expand vertical space, to
     accommodate a top-situated legend.}
  \item{legendPos}{Position of legend within plot.}
}

\details{

The \code{plotPairedResids} function plots model residuals against pairs
of features, for example for model validation.  Pairs are chosen
randomly.

The function \code{qeFreqParcoord} is a \code{qeML} interface to the
\code{cdparcoord} package.

The function \code{qePlotCurves} plots X-Y curves for one or more
groups.  Within each group, the (X,Y) pairs are plotted, possibly with
\code{\link{loess}} smoothing.  Note that the function will sort the
pairs according to X, so that \code{\link{loess}} smoothing makes sense.
}

\examples{

\dontrun{
data(pef)
linout <- qeLin(pef,'wageinc')
plotPairedResids(pef,linout)

data(lsa)
# plot LSAT score against undergradute GPA, for each law school cluster
# (reputed quality of the law school)
qePlotCurves(lsa,6,5,9,legendSpace=1.35)
}

}

\author{
Norm Matloff
}

