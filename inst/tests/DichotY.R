
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

checkAll(dichotRegFtns,'svc1000','gender',pause=TRUE) 

