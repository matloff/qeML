

# plots several curves, one for each gorup, against a common X-axis, as
# in qePlotCurves, but showing the change in each variable, relative to
# the variable's value at min X

# X is typically input in ascending numerical order, but need not be

# X is required to be in col 1; col 2 is for Y of group 1, etc.

# 'data' must be a data frame or equivalent, in which for each X value
# there is exactly one Y value for each group; format is wide, e.g.

#    x y1 y2 y3

#    w <- data.frame(x=c(3:5,2),y1=c(5:7,4),y2=c(4,12,15,5),y3=10:7)
#    qeMittalGraph(w)

qeMittalGraph <- function(data,xlab='x',ylab='y',legendTitle='curve')
{

   x <- data[,1]
   nc <- ncol(data)
   argMinX <- which.min(data$x)

   z <- lapply(1:(nc-1),
      function(i) {
         tmp <- data[,i+1] / data[argMinX,i+1]
         tmpDF <- data.frame(x=x,curveNum=i,y=tmp)
         tmpDF
      }
   )
   zz <- do.call(rbind,z)

   qePlotCurves(zz,1,3,2,xlab=xlab,ylab=ylab,legendTitle=legendTitle)
}

