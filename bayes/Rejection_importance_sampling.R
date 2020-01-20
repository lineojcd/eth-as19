###########################
## 1. Rejection sampling ##
###########################
# Explaining the underlying principle
par(mfrow=c(2,1))
z <- seq(-5,5,0.01)
f <- dnorm(z) # Target density (f "=" pi, Gaussian)
g <- 0.5*exp(-abs(z)) # Proposal density (g "=" tau, laplace aka 2-sided exponential)
M <- 1/sqrt(pi*0.5)*exp(0.5)
plot(z,g*M,type="l",ylab="density",lwd=2,col=4,main=
       "normal and 2-sided exponential density")
abline(h=0)
lines(z,f,col=2,lwd=2) 
lines(z,g)
legend("topright",c("target f","envelope Mg","proposal g"),col=c(2,4,1),lwd=c(2,2,1),bt="n")
plot(z,f/g/M,type="l",ylab="f(x)/(g(x)M)",main=
       "acceptance probabilities")
1/M # acceptance probability = 1/M

# Implementing 
n <- 10000
par(mfrow=c(1,1))
x <- (2*(runif(n) < 0.5)-1)*log(runif(n)) # Sample from g (laplace distribution) using quantile transform
qqnorm(x) # g has heavier tails than f
qqline(x)
a <- exp(-0.5*(abs(x)-1)^2) # Vector of acceptance probabilities
y <- x[runif(n)<a] # Accept/reject step
qqnorm(y) # Check for normality 
qqline(y)
length(y) # Number of accepted values

br <- c(-12,seq(-4,4,0.2),12) # Histogram boundaries 
hg <- hist(x,breaks=br,plot=F)$counts # Frequencies of proposed values
hf <- hist(y,breaks=br,plot=F)$counts # Frequencies of accepted values
par(mfrow=c(2,1))
barplot(height=rbind(hf,hg-hf),main=
          "Histogram of proposed and accepted values")
plot(z,g*M,type="l",ylab="density",lwd=2,col=4,main=
       "normal and 2-sided exponential density") 
abline(h=0)
lines(z,f,col=2,lwd=2) 


###########################
# 2. Importance sampling ## (for the same pair target/proposal)
###########################
n <- 10000
x <- (2*(runif(n) < 0.5)-1)*log(runif(n)) #Sample from g (laplace distribution)
w <- exp(-0.5*(abs(x)-1)^2)
w <- w/sum(w) #Weights for importance sampling
plot(x,w,type="h",ylab="weights")
plot(sort(w),ylab="sorted weights",type="l")
abline(h=0.0001,col=2) #Shows non-uniformity of weights.

##Show that weighted sample has the correct the target density
par(mfrow=c(1,1)) #Histogram of the weighted sample
kmax <- ceiling(10*(max(x)-min(x))) #Number of classes with length 0.1
c0 <- min(x) - 0.05 
c1 <- c0 + kmax*0.1
z <- ceiling((x-c0)*10) # Assigning observations to intervals
h <- rep(0,kmax)
for (k in (1:kmax)) h[k] <- sum(w[z==k]) # Counting frequencies
plot(c0+((1:kmax)-0.5)*0.1,h*10,type="h",xlab="x",ylab="density" )
xx <- seq(-3,3,0.01)
lines(xx,dnorm(xx),col=2)

##SIR
y <- as.vector(x%*%rmultinom(n, size=1, prob=w))
qqnorm(y)
qqline(y)
length(unique(y))##Number of unique values

