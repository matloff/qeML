
dichotRegFtns <- c(
'qeAdaBoost',
'qeDT',
'qeGBoost',
'# qeLightGBoost',
'qeRF',
'qeRFgrf',
'qeRFranger',
'qeXGBoost',
'qeKNN',
'# qeLinKNN',
'# qePolyLinKNN',
'qeDeepnet',
'# qeNeural',
'# qeLASSO',
'# qeLin',
'qeLogit',
'qeNCVregCV',
'qePolyLASSO',
'# qePolyLin',
'# qePolyLog',
'qeSVM'
)

source('CheckAll.R')

# commented out in case the CRAN process executes all R code
# checkAll(dichotRegFtns,'svc1000','gender',pause=TRUE) 

