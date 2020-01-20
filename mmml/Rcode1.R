# Mathematical tools for machine learning
# Loris Michel, ETHZ

# function to generate a training set
generateS <- function(n=1000) {
  x <- cbind(runif(n),runif(n))
  y <- ifelse(x[,1] <= 0.75 & x[,1] >= 0.25 & x[,2] <= 0.75 & x[,2] >= 0.25,1,0)
  return(list(x=x,y=y))
}

# learner
A <- function(S) {
  ind.positive <- which(S$y==1)
  x1.max <- max(S$x[ind.positive,1])
  x1.min <- min(S$x[ind.positive,1])
  x2.max <- max(S$x[ind.positive,2])
  x2.min <- min(S$x[ind.positive,2])
  return(list(h=function(x) ifelse(x[1] <= x1.max & x[1] >= x1.min & x[2] <= x2.max & x[2] >= x2.min,1,0),
              x1.max = x1.max,
              x1.min = x1.min,
              x2.max = x2.max,
              x2.min = x2.min))
}

# compute true error
Ld <- function(A.output) {
  return((0.5*0.5)-(A.output$x1.max-A.output$x1.min)*(A.output$x2.max-A.output$x2.min))
}

# empirical error
Ls <- function(h, S) {
  return(mean(apply(S$x,1,h)!=S$y))
}

# some analyses

# 1) generate training and see what happens (reference to ex 3.2)
S <- generateS(1000)
A.output <- A(S)
Ld(A.output)
Ls(A.output$h, S)

# 2) look at how the loss behaves with increasing sample size (reference to ex 4.1)
losses.mat <- c()
for (i in 1:10) {
  losses <- c()
  for (n in 10^{seq(2,6,length.out = 20)}) {
    S <- generateS(n)
    A.output <- A(S)
    losses <- c(losses, Ld(A.output))
  }
  losses.mat <- cbind(losses.mat, losses)
}
plot(seq(2,6,length.out = 20),apply(losses.mat,1,mean),
     type="b",xlab="log_10(n)",ylab="average loss")




