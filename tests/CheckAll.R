
require(qeML)
data(prgeng)
set.seed(9999)
pe1000 <- prgeng[sample(1:nrow(prgeng),1000),]

evalr <- function(toexec) {
   eval(parse(text=toexec),parent.frame())
}

checkAll <- function(pause=FALSE) 
{

   errMsgs <- NULL
   for (qeFtn in regFtns) {
      if (substr(qeFtn,1,1) == '#') {
         warning(paste0('skipping ',qeFtn))
         next
      }
      qeCmd <- sprintf('qeOut <- %s(pe1000,"sex")$testAcc',qeFtn)
      res <- try(evalr(qeCmd))
      print(qeFtn)
      if (!inherits(res,'try-error')) print(qeOut)
      else errMsgs <- c(errMsgs,res)
      if (pause) ans <- readline('next')
   }
   browser()
   return(errMsgs)
}

