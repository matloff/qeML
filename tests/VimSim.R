
# simulate actions of Vim editor; maybe make this a package later

# 'doc' can be either a file name of a text vector
vimEdit <- function(doc) 
{
   if (length(doc) == 1)  # not the best test, but use it
      doc <- readLines(doc)
   doc
}

# Vim 'd' command
vimDelLines <- function(lineNums,doc) 
{
   doc[-lineNums]
}

# find line numbers of lines containg 'searchStr'
# see help(grep) rec 'fixed'
matchStrLines <- function(searchStr,fixed=TRUE,doc=myDoc) 
{
   grep(searchStr,doc,fixed=fixed)
}

# find character positions within 'docLine' containing 'searchStr'
matchStrWithinLine <- function(docLine,searchStr,fixed=TRUE,allInsts=FALSE) 
{
   theLine <- docLine
   lStr <- length(searchStr)
   charPositions <- NULL
   while (1) {
      tmp <- regexpr(searchStr,theLine,fixed=fixed)
      if (tmp != -1) charPositions <- c(charPositions,tmp)
      if (tmp == -1 || 
          !allInsts ||
          tmp+lStr >= nchar(theLine)) return(charPositions)
      theLine <- substr(theLine,tmp+lStr,nchar(theLine))
   }
}

