\name{Double Descent}
\alias{doubleD}
\alias{plot.doubleD}

\title{Double Descent Phenomenon}

\description{

Belkin and others have shown that some machine learning algorithms
exhibit surprising behavior when in overfitting settings.  The classic
U-shape of mean loss plotted against model complexity may be followed by
a surprise second "mini-U."

Alternatively, one might keep the model complexity fixed while varying
the number of data points n, including over a region in which n is
smaller than the complexity value of the model.  The surprise here is
that mean loss may actually increase with n in the overfitting region.

The function \code{doubleD} facilitates easy exploration of this
phenomenon.  }

\usage{doubleD(qeFtnCall,xPts,nReps,makeDummies=NULL,classif=FALSE)}

\arguments{
   \item{qeFtnCall}{Quoted string; somewhere should include 'xPts[i]'.}
   \item{xPts}{Range of values to be used in the experiments, e.g.
      a vector of degrees for polynomial models.}
   \item{nReps}{Number of repetitions for each experiment, typically the
      number in the holdout set.}
   \item{makeDummies}{If non-NULL, call \code{regtools::factorsToDummies}
      on the dataset of this name.  This avoids the problem of some
      levels of a factor appearing in the holdout set but not the
      training set.}
   \item{classif}{Set TRUE if this is a classification problem.}
}

\details{The function will run the code in \code{qeFtnCall} \code{nreps}
times for each level specified in \code{xPts}, recording the test and
training error in each case.  So, for each level, we will have a mean
test and training error.
}

\value{Each call in \code{xPts} results in one line in the return value
of \code{doubleD}.  The return matrix can then be plotted, using the
generic \code{plot.doubleD}.  Mean test (red) and training (blue) accuracy 
will be plotted against \code{xPts}.
}

\examples{
   \dontrun{
      data(mlb1)
      hw <- mlb1[,2:3]
      doubleD('qePolyLin(hw,"Weight",deg=xPts[i])',1:20,250)
   }
}

\author{
Norm Matloff
}

