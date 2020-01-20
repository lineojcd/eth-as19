
#############  R-code to generate a training sample such that y =1 for <w,x>  > 0 and y=-1 for <w,x>  < 0
par(mfrow=c(1,1))
wtrue=c(-1,2)

generatexy = function(m, w=wtrue){
d=length(w)
Matx = matrix(0,nrow=d, ncol=m)
vecy = rep(0, m)
for(i in 1:m){
Matx[,i]  = rnorm(d)
vecy[i]  =  ifelse(sum(Matx[,i]*w) > 0, 1, -1)
}
List = list(X = Matx, y = vecy)
return(List)
}



##########R-code for the perceptron algorithm 

Perceptron = function(dataX=Data$X, dataY= Data$y, verbose = FALSE){

d = dim(dataX)[1]
w0 = rep(0, d)
w.old = w0 
detect = 1
count = 0
while(detect ==1){
count = count+1
if (verbose) {
  cat("iteration =", count, "\n") 
}

check = dataY*(w.old%*%dataX)
ind = which.min(check)
w.new = w.old + dataY[ind]*dataX[,ind]
check = dataY*(w.new%*%dataX)
detect = ifelse(min(check) <=0,1, 0)
if (verbose) {
  par(mfrow=c(1,1))
  plot(dataX[1,],dataX[2,], type="n", xlab="", ylab="")
  title("m=10, w*=(-1,2)^T")
  #points(Data.m10$X[1,],Data.m10$X[2,], type="p", pch=20, xlab="", ylab="")
  Condp =wtrue[1]*dataX[1,]+ wtrue[2]*dataX[2,]  > 0
  Condm =wtrue[1]*dataX[1,]+ wtrue[2]*dataX[2,]  < 0
  points(dataX[1,][Condp],dataX[2,][Condp], type="p", pch="+", col="blue", lwd=2)
  points(dataX[1,][Condm],dataX[2,][Condm], type="p", pch= "-", col="red", lwd=2)
  #abline(a=0, b=1/2, col="black", lwd=2)
  abline(a=0, b=-w.new[1]/w.new[2], col="magenta", lwd=2.5, lty=2)
  require(profvis)
  pause(seconds = 0.1)
  
}
w.old = w.new
}
return(list(what=w.old, nbiter = count))

}


########Run the perceptron on the simulated data and visualizing the data and the classifier: example with m=10

Data.m10 = generatexy(m=10, w=wtrue)

###  To extract the matrix where the examples "x_i" are stored, do Data.m10$X
#### To extact the vector "y", do Data.m10$y


#### The output of the perceptron: 

wpercp.m10 = Perceptron(dataX=Data.m10$X, dataY= Data.m10$y, verbose = TRUE) 
#what is the minimal value of y < wpercep, x> ?

min(Data.m10$y*(wpercp.m10$what%*%Data.m10$X))

######## Visualization

plot(Data.m10$X[1,],Data.m10$X[2,], type="n", xlab="", ylab="")
title("m=10, w*=(-1,2)^T")
#points(Data.m10$X[1,],Data.m10$X[2,], type="p", pch=20, xlab="", ylab="")
Condp =wtrue[1]*Data.m10$X[1,]+ wtrue[2]*Data.m10$X[2,]  > 0
Condm =wtrue[1]*Data.m10$X[1,]+ wtrue[2]*Data.m10$X[2,]  < 0
points(Data.m10$X[1,][Condp],Data.m10$X[2,][Condp], type="p", pch="+", col="blue", lwd=2)
points(Data.m10$X[1,][Condm],Data.m10$X[2,][Condm], type="p", pch= "-", col="red", lwd=2)
abline(a=0, b=1/2, col="black", lwd=2)
abline(a=0, b=-wpercp.m10$what[1]/wpercp.m10$what[2], col="magenta", lwd=2.5, lty=2)




########Run the perceptron on the simulated data and visualizing the data and the classifier: example with m=50

Data.m50 = generatexy(m=50, w=wtrue)

###  To extract the matrix where the examples "x_i" are stored, do Data.m50$X
#### To extact the vector "y", do Data.m50$y


#### The output of the perceptron: 

wpercp.m50 = Perceptron(dataX=Data.m50$X, dataY= Data.m50$y) 

#what is the minimal value of y < wpercep, x> ?

min(Data.m50$y*(wpercp.m50$what%*%Data.m50$X))

######## Visualization

plot(Data.m50$X[1,],Data.m50$X[2,], type="n", xlab="", ylab="")
title("m=50, w*=(-1,2)^T")
#points(Data.m10$X[1,],Data.m10$X[2,], type="p", pch=20, xlab="", ylab="")
Condp =wtrue[1]*Data.m50$X[1,]+ wtrue[2]*Data.m50$X[2,]  > 0
Condm =wtrue[1]*Data.m50$X[1,]+ wtrue[2]*Data.m50$X[2,]  < 0
points(Data.m50$X[1,][Condp],Data.m50$X[2,][Condp], type="p", pch="+", col="blue", lwd=2)
points(Data.m50$X[1,][Condm],Data.m50$X[2,][Condm], type="p", pch= "-", col="red", lwd=2)
abline(a=0, b=1/2, col="black", lwd=2)
abline(a=0, b=-wpercp.m50$what[1]/wpercp.m50$what[2], col="magenta", lwd=2.5, lty=2)


########Run the perceptron on the simulated data and visualizing the data and the classifier: example with m=100


Data.m100 = generatexy(m=100, w=wtrue)

###  To extract the matrix where the examples "x_i" are stored, do Data$X
#### To extact the vector "y", do Data$y


#### The output of the perceptron: 

wpercp.m100 = Perceptron(dataX=Data.m100$X,dataY= Data.m100$y, verbose = TRUE) 

#what is the minimal value of y < wpercep, x> ?

min(Data.m100$y*(wpercp.m100$what%*%Data.m100$X))

######## Visualization

plot(Data.m100$X[1,],Data.m100$X[2,], type="p", pch=20, xlab="", ylab="")
title("m=100")
abline(a=0, b=1/2, col="blue", lwd=2)
abline(a=0, b=-wpercp.m100$what[1]/wpercp.m100$what[2], col="red", lwd=2, lty=2)




# Question to students what is happening?
iter.vec <- c()
d.vec <- rep(2:200, each=20)
R.vec <- c()
min.vec <- c()
what.vec <- c()
m <- 100
for (d in d.vec) {
  wtrue <- runif(d, min = -1, max = 1)
  Data <- generatexy(m=m, w=wtrue)
  wperc <- Perceptron(dataX=Data$X,dataY= Data$y) 
  iter.vec <- c(iter.vec, wperc$nbiter)

}

# compute summaries
iter.means <- tapply(iter.vec, d.vec, mean)

# plot
par(mfrow=c(1,1))
plot(names(iter.means), iter.means,pch=19,xlab="dimension", ylab="nb iterations")
points(names(iter.max), iter.max,pch=19,col="red")

