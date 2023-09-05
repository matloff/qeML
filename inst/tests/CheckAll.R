
require(qeML)
data(svcensus)
set.seed(9999)
svc1000 <- svcensus[sample(1:nrow(svcensus),1000),]
dataName <- 'svc1000'

evalr <- function(toexec) {
   eval(parse(text=toexec),parent.frame())
}

checkAll <- function(regFtns,dataName,yName,pause=TRUE) 
{

   errMsgs <- NULL
   for (qeFtnName in regFtns) {
      if (substr(qeFtnName,1,1) == '#') {
         warning(paste0('skipping ',qeFtnName))
         next
      }
      qeCmd <- sprintf('qeOut <- %s(%s,"%s")$testAcc',qeFtnName,dataName,yName)
      res <- try(evalr(qeCmd))
      print(qeFtnName)
      if (!inherits(res,'try-error')) print(qeOut)
      else errMsgs <- c(errMsgs,res)
      if (pause) ans <- readline('next')
   }
   return(errMsgs)
}

