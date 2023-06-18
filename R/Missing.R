
# routines for dealing with missing values, in a prediction context

qeLinMV <- function(data,yName) 
{   
   requireNamespace('toweranNA')
   obj <- toweranNA::makeTower(data,yName,regFtnName='lm')
   class(obj) <- c('qeLinMV',class(obj))
   obj
}

predict.qeLinMV <- function(object,newx,...) 
{
    class(object) <- class(object)[-1]
    predict(object,newx)
}

qeLogitMV <- function(data,yName,yesYVal) 
{   
   requireNamespace('toweranNA')
   obj <- toweranNA::makeTower(data,yName,regFtnName='glm',yesYVal=yesYVal)
   class(obj) <- c('qeLogitMV',class(obj))
   obj
}

predict.qeLogitMV <- predict.qeLinMV


qeKNNMV <- function(data,yName,kmax) 
{   
   requireNamespace('toweranNA')
   obj <- toweranNA::makeTower(data,yName,regFtnName='towerKNN',
      opts=list(kmax=kmax))
   class(obj) <- c('qeKNNMV',class(obj))
   obj
}

predict.qeLogitMV <- predict.qeLinMV
predict.qeKNNMV <- predict.qeLinMV
