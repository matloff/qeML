
# basically a wide-to-long reshape, but with various options for a time
# column

# arguments:

#    data: data frame or equivalent
#    timeColName: name of time column, including if not yet formed
#    timeColPresent: TRUE means column already in 'data'
#    timeColSeq:  (m,n) means created column will have the values
#       m,m+n,m+2n,m+3n,...

wideToLongWithTime <- function(data,timeColName,timeColPresent=TRUE,
   timeColSeq=c(1,1)) 
{
   qeML:::getSuggestedLib('reshape2')

   if (!timeColPresent) {
      first <- timeColSeq[1]
      inc <- timeColSeq[2]
      tmp <- 1:nrow(data)
      timecol <- first + inc*tmp
      newdata <- cbind(timecol,data)
      names(newdata)[1] <- timeColName
   }

   return(reshape2::melt(newdata,id.vars=timeColName))

}





