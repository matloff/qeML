
# routines for dealing with missing values, in a prediction context

# fits the specified qe* function, saving the predictd values for the
# training set, so that they can be used by toweranNA in predicting

# example:

# engl <- english[,c(2,5:8,10)] 
# linanna <- qeLinMV(engl,'vocab') 
# newx <- engl[1,-6] 
# predict(linanna,newx) 
# newx[1] <- NA 
# predict(linanna,newx) 
# newx[2] <- NA 
# predict(linanna,newx) 

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
