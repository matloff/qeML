

# plots several curves, against a common X-axis, as in qePlotCurves, but
# showing the change in each variable, relative to the variable's value
# at min X

# 'data' must be a data frame or equivalent, with this structure

#  column 1 is the "X" variable to serve in the output plot

#  the remaining columns will serve as "Y" columns, one for each curve
#  to be plotted,  divided by the value of this
#  variable at min X; it is presumed that the sequence is sorted in
#  ascending order, with the first element corresponding to X = 0

# example:

#    w <- data.frame(x=c(3:5,2),y1=c(5:7,4),y2=c(4,12,15,5),y3=10:7)
#    qeMittalGraph(w)

qeMittalGraph <- function(data) 
{

   x <- data[,1]
   nc <- ncol(data)
   # reshape, esp. including the curve number
   argMinX <- which.min(data$x)
   z <- lapply(1:(nc-1),
      function(i) {
         tmp <- data[,i+1] / data[argMinX,i+1]
         tmpDF <- data.frame(x=x,curveNum=i,y=tmp)
         tmpDF
      }
   )
   zz <- do.call(rbind,z)
   qePlotCurves(zz,1,3,2)
}

