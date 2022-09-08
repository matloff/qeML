
data(pef)

# these should all be in the $22,000-27,000 range

# quicker

qeLin(pef,'wageinc')$testAcc
qeKNN(pef,'wageinc')$testAcc
qeRFranger(pef,'wageinc')$testAcc
qeRFgrf(pef,'wageinc')$testAcc
qeGBoost(pef,'wageinc')$testAcc
qeAdaBoost(pef,'wageinc')$testAcc
qeLightGBoost(pef,'wageinc')$testAcc
qeNeural(pef,'wageinc')$testAcc
qeLASSO(pef,'wageinc')$testAcc
qePCA(pef,'wageinc','qeKNN',pcaProp=0.5)$testAcc

# MLB, about 13
qeCompare(mlb,'Weight',
   c('qeLin','qePolyLin','qeKNN','qeRF','qeLASSO','qeNeural'),25)


# longer-running

pefSmall <- pef[sample(1:nrow(pef),5000),]

qeRF(pefSmall,'wageinc')$testAcc
qePolyLin(pefSmall,'wageinc')$testAcc
qePolyLASSO(pefSmall,'wageinc')$testAcc
qeskRF(pefSmall,'wageinc')$testAcc
qeUMAP(pefSmall,'wageinc','qeKNN',nComps=2)$testAcc

pef1 <- pef
pef1[c('age','wkswrkd')] <- makeNA(as.matrix(pef1[c('age','wkswrkd')]),0.1)
pef1Small <- pef1[sample(1:nrow(pef1),500),]
pef2Small <- as.data.frame(toAllNumeric(pef1Small))
qeKNNna(pef2Small,'wageinc')$testAcc

qePolyLASSO


# should be in the low 20% range

qeLogit(pef,'sex',yesYVal=2)$testAcc
qeSVM(pefSmall,'sex')$testAcc
qeskSVM(pefSmall,'sex')$testAcc
qeAdaBoost(pef,'sex')$testAcc
qePolyLog(pef,'sex')$testAcc


qeGBoost(pefSmall,'sex')$testAcc
