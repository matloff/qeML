
data(pef)


z <- qeLin(pef,'wageinc',holdout=NULL)
predict(z,pef[1,-5]) # 70695.6
set.seed(9999)
z <- qeLin(pef,'wageinc')
z$testAcc  # 25946.22
z$baseAcc  # 32691.94

