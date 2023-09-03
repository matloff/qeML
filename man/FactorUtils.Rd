\name{R Factor Utilities}
\alias{levelCounts}
\alias{dataToTopLevels}
\alias{factorToTopLevels}
\alias{cartesianFactor}
\alias{qeRareLevels}

\title{R Factor Utilities}

\description{
Utilities to manipulate R factors, extending the ones in \code{regtools.}
}

\usage{
levelCounts(data)
dataToTopLevels(data,lowCountThresholds)
factorToTopLevels(f,lowCountThresh=0)
cartesianFactor(dataName,factorNames,fNameSep = ".")
qeRareLevels(x, yName, yesYVal = NULL)
}

\arguments{
  \item{data}{A data frame or equivalent.}
  \item{f}{An R factor.}
  \item{lowCountThresh}{Factor levels will counts below this value will
     not be used for this factor.}
  \item{lowCountThresholds}{An R list of column names and their
     corresponding values of \code{lowCountThresh}.}
  \item{dataName}{A quoted name of a data frame or equivalent.}
  \item{factorNames}{A vector of R factor names.}
  \item{fNameSep}{A character to be used as a delimiter in the names of
     the levels of the output factor.}
  \item{x}{A data frame.}
  \item{yName}{Quoted name of the response variable.}
  \item{yesYVal}{In the case of binary Y, the factor level to 
     be considered positive.}
}

\details{
Often one has an R factor in which one or more levels are rare in the
data.  This could cause problems, say in performing cross-validation; a
level in the test set might be "new," not having appeared in the
training set.  Toward this end, \code{factorToTopLevels} will remove
rare levels from a factor; \code{dataToTopLevels} applies this to an
entire data frame.

Also toward this end, the function \code{levelCounts} simply applies
\code{table()} to each column of \code{data}, returning the result as an
R list. (If more than 10 levels, it returns NA.

The function \code{cartesianFactor} generates a "superfactor" from
individual ones; e.g. if factors f1 and f2 have n1 and n2 levels, the
output is a new factor with n1 * n2 levels.

The function \code{qeRareLevels} checks all columns in a data frame in
terms of being an R factor with rare levels.

}

\examples{

data(svcensus)
levelCounts(svcensus)  # e.g. finds there are 15182 men, 4908 women
f1 <- svcensus$gender  # 2 levels
f2 <- svcensus$occ  # 6 levels
z <- cartesianFactor('svcensus',c('gender','occ'))
head(z)
# [1] female.102 male.101   female.102 male.100   female.100 male.100  
# 12 Levels: female.100 female.101 female.102 female.106 ... male.141

}


\author{
Norm Matloff
}

