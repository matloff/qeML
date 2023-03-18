
regFtns <- c(
'# qeAdaBoost',
'qeDT',
'qeGBoost',
'qeLightGBoost',
'qeRF',
'qeRFgrf',
'qeRFranger',
'qeXGBoost',
'qeKNN',
'qeLinKNN',
'qePolyLinKNN',
'# qeDeepnet',
'# qeNeural',
'qeLASSO',
'qeLin',
'# qeLogit',
'qeNCVregCV',
'qePolyLASSO',
'qePolyLin',
'# qePolyLog',
'# qeSVM',
'# qeliquidSVM'
)

require(qeML)
data(prgeng)
set.seed(9999)
pe1000 <- prgeng[sample(1:nrow(prgeng),1000),]

evalr <- function(toexec) {
   eval(parse(text=toexec),parent.frame())
}

checkAll <- function() 
{
   for (qeFtn in regFtns) {
      if (substr(qeFtn,1,1) == '#') {
         warning(paste0('skipping ',qeFtn))
         next
      }
      qeCmd <- sprintf('qeOut <- %s(pe1000,"wageinc")$testAcc',qeFtn)
      evalr(qeCmd)
      print(qeFtn)
      print(qeOut)
      ans <- readline('next')
   }
}

