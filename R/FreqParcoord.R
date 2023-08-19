
qeFreqParcoord <- function(dataName,k=25,opts=NULL) 
{

   if (!is.character(dataName)) stop("dataName must be quoted")
   checkPkgLoaded('cdparcoord')

   # if (!is.null(grpName)) opts[['grpcategory']] <- grpName
   if (!is.null(opts[['grpcategory']])) 
      stop('grpcategory not currently implemented')

   cmd <- buildQEcall('discparcoord',dataName,opts=opts,holdoutArg=FALSE)
   evalr(cmd)

}


