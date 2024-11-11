\name{Advanced Plots}
\alias{plotPairedResids}
\alias{plotClassesUMAP}
\alias{qeFreqParcoord}
\alias{qePlotCurves}
\alias{qeMittalGraph}

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
   loess=TRUE,legendTitle,legendSpace=1.1,legendPos='topright')

qePlotCurves(curveData,xCol=1,yCol=2,grpCol=3,
   xlab=names(curveData)[xCol],ylab=names(curveData)[yCol],loess=TRUE,
   legendTitle=names(curveData)[grpCol],legendSpace=1.1,legendPos="topright",
   wide=FALSE,wideTimeColName=NULL,wideTimeColPresent=NULL,
      wideTimeColBase=1:nrow(curveData),wideGrpColName=NULL,
      wideValueColName=NULL)
qeMittalGraph(dataMitt,xlab="x",ylab="y",legendTitle="curve",loess=TRUE) 
}

\arguments{
  \item{data}{A data frame or equivalent..}
  \item{dataMitt}{A data frame or equivalent.  "X" and "Y" columns, followed
     by a group column, an R factor.}
  \item{qeOut}{An object returned from one of the qe-series
     predictive functions..}
  \item{classVar}{Name of the column containing class information.}
  \item{dataName}{Quoted name of a data frame.}
  \item{k}{Number of nearest neighbors.}
  \item{opts}{Options to be passed to \code{discparcoord}.}
  \item{curveData}{Data to be plotted.}
  \item{xCol}{Column name or number containing "X".}
  \item{yCol}{Column name or number containing "Y".}
  \item{grpCol}{Column name or number containing group name, 
    a character vector or factor.}
  \item{xlab}{X-axis label.}
  \item{ylab}{Y-axis label.}
  \item{loess}{If TRUE, do loess smoothing within each group.}
  \item{legendTitle}{Legend title.}
  \item{legendSpace}{Factor by which to expand vertical space, to
     accommodate a top-situated legend.}
  \item{legendPos}{Position of legend within plot.}
  \item{curveData}{A data frame, "X" values in column 1.}
  \item{xlab}{Label for X-axis.}
  \item{ylab}{Label for Y-axis.}
  \item{wide}{TRUE if \code{curveData} is in wide format.}
  \item{wideTimeColName}{Name to be used for "X"-axis.}
  \item{wideTimeColPresent}{If TRUE, a time column already exists.}
  \item{wideTimeColBase}{"Time" values for each group.}
  \item{wideGrpColName}{Group name.}
  \item{wideGrpValueName}{"Y" value name.}
}

\details{

The \code{plotPairedResids} function plots model residuals against pairs
of features, for example for model validation.  Pairs are chosen
randomly.

The function \code{qeFreqParcoord} is a \code{qeML} interface to the
\code{cdparcoord} package.

The function \code{qePlotCurves} plots X-Y curves for one or more
groups.  Within each group, the (X,Y) pairs are plotted, possibly with
\code{\link{loess}} smoothing. Input data format long by default, but
can be wide.

The function \code{qeMittalGraph} is similar to \code{qePlotCurves},
except that it displays multiplicative change over the X-axis. All
curves start at height 1.0. (There may be some exceptions to this if
\code{loess} is TRUE.) The X-axis could be time or some model parameter,
say in graphing prediction accuracy against number of nearest neighbors
for different datasets.  }

\examples{

\dontrun{

data(pef)
linout <- qeLin(pef,'wageinc')
plotPairedResids(pef,linout)

data(lsa)
# plot LSAT score against undergradute GPA, for each law school cluster
# (reputed quality of the law school)
qePlotCurves(lsa,6,5,9,legendSpace=1.35)

data(currency)
curr <- cbind(1:nrow(currency),currency)
names(curr)[1] <- 'weeknum'
qePlotCurves(currency,wide=TRUE,wideTimeColName='weeknum',
   wideTimeColPresent=FALSE,wideGrpColName='country',wideValueColName='rate')
qeMittalGraph(curr,'weeknum','rate','country')
# Canadian dollar and pound in one cluster, and franc, mark and
# yen in another


}

}

\author{
Norm Matloff
}

