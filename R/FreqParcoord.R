
qeFreqParcoord <- function(dataName,k,grpName=NULL) 
{
   cmd <- paste0('discparcord(',dataName,',k=k')
   if (!is.null(grpName)a) {
      cmd <- paste0(cmd,',grpcategory=',"'",grpName,"'")
   }
   # qeOut <- eval(parse(text=cmd))


}


