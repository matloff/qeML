
# check both the function list in the README and NAMESPACE file to see
# which functions are in one but not the other

source('VimSim.R')

# get all the functions from the functions list in the README

extractFromFtnList <- function() 
{
   # z <- readLines('../../README.md')
   z <- vimEdit('../../README.md')
   zfl <- matchStrLines('## Function list',fixed=TRUE,z)
   zftns <- vimDelLines(1:zfl,z)
   zHaveFtn <- matchStrLines('()',zftns,fixed=T) 
   ftnList <- NULL
   for (lineNum in zHaveFtn) {
      theLine <- zftns[lineNum]
      ftnStart <- matchStrWithinLine(theLine,'**') + 2
      ftnEnd <- matchStrWithinLine(theLine,'()') - 1
      ftn <- substr(theLine,ftnStart,ftnEnd)
      ftnList <- c(ftnList,ftn)
   }
   ftnList  
}

# get the functions (and other) list from NAMESPACE

extractFromNAMESPACE <- function() {
   ls(package:qeML)
}

fl <- extractFromFtnList()
ns <- extractFromNAMESPACE()
setdiff(fl,ns)

