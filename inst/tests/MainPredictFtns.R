
# these should all be in the $22,000-26,000 range

data(pef)

# quicker

qeLin(pef,'wageinc')$testAcc
qeKNN(pef,'wageinc')$testAcc
qeRFranger(pef,'wageinc')$testAcc
qeRFgrf(pef,'wageinc')$testAcc
qeGBoost(pef,'wageinc')$testAcc
qeLightGBoost(pef,'wageinc')$testAcc
qeNeural(pef,'wageinc')$testAcc
qeLASSO(pef,'wageinc')$testAcc


# longer-running

qeRF(pef,'wageinc')$testAcc
qeNeuralNet(pef,'wageinc')$testAcc
qePolyLin(pef,'wageinc')$testAcc
qePolyLASSO(pef,'wageinc')$testAcc
qeskRF(pef,'wageinc')$testAcc

pef1 <- pef
pef1[c('age','wkswrkd')] <- makeNA(as.matrix(pef1[c('age','wkswrkd')]),0.1)
pef2 <- pef1[c('age','wkswrkd')]
qeKNNna(pef2,'wageinc')$testAcc


qePolyLASSO


