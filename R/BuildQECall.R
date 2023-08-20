# builds a string for a qe*() call, with options

buildQEcall <- function(qeFtnName,dataName,yName=NULL,opts=NULL,holdout=NULL,
   holdoutArg=TRUE) 
{
   ho <- if (is.null(holdout)) 'NULL' else as.character(holdout)
   cmd <- paste0( qeFtnName,'(','data = ',dataName) 
   if (!is.null(yName)) {
      cmd <- paste0(cmd,',yName=','"',yName,'"')
   }
   if (holdoutArg) {
      ho <- if (is.null(holdout)) 'NULL' else as.character(holdout)
      cmd <- paste0(cmd,',holdout = ',ho)
   }

   if (!is.null(opts)) {  # more args?
      nms <- names(opts)
      for (i in 1:length(nms)) {
         cmd <- paste0(cmd,',')
         argval <- opts[[nms[i]]]
         arg <- paste0(nms[i],'=',argval)
         if (i == length(nms)) cmd <- paste0(cmd,arg,')')
      }
   } else cmd <- paste0(cmd,')')
   cmd

}

checkBuildQECall <- function() 
{
   data(svcensus)

   z <- qePCA(svcensus,'wageinc','qeKNN',opts=list(k=100))
   print(z$testAcc)

   z <- qePCA(svcensus,'gender','qeSVM',opts=list(kernel='"linear"'),
      pcaProp=0.5) 
   print(z$testAcc)
}

