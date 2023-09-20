
NumericRegFtns <- c(
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
'# qeSVM'
)
   
source('CheckAll.R')

# commented out in case the CRAN process executes all R code
# checkAll(NumericRegFtns,'svc1000','wageinc',pause=TRUE) 


