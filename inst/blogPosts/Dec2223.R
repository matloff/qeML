
data(mlb)
library(quantreg)
z <- rq(Weight ~ Height,data=mlb,tau=0.80)
summary(z)


sftn <- function(nearIdxs,x,y,predpt)
{
   nearYs <- y[nearIdxs]
   quantile(nearYs,0.80)
}

u <- mlb[c('Height','Age','Weight')]
set.seed(9999) # qeML ftns do random holdout
z <- qeKNN(u,'Weight',smoothingFtn=sftn)
predict(z,c(70,28)) # prints 200


makeSmFtn <- 
   function(q) function(newIdxs,x,y,predpt) quantile(y[newIdxs],q)

z <- qeKNN(u,’Weight’,smoothingFtn=makeSmFtn(q))


