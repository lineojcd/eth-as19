###########################################################
## Simulation from the posterior using the Gibbs sampler ##
###########################################################

# Observations normal(mu,1/tau).
# Prior for mu = normal(xi,1/k), 
# prior for tau (so-called precision) = Gamma(gam,lam), independent.

set.seed(1)
n=4
x <- rnorm(n) #generating data
xb <- mean(x)
xs <- var(x)*(n-1)/n  

log.lik.contour <- function(n,xb,xs){
  ## Plots the log likelihood for mu and tau of normal observations
  ## Arguments: xb,xs = MLEs for mean and variance, n=sample size
  
  # First, determine appropriate plotting range for mu and tau
  mu <- seq(xb-3*sqrt(xs/n),xb+3*sqrt(xs/n),length=32)
  tau <- seq(qgamma(0.001,shape=n/2+1,rate=n*xs/2),
             qgamma(0.99,shape=n/2+1,rate=n*xs/2),length=32)
  
  # Compute and plot log likelihood
  z <- 0.5*n*outer(rep(1,32),log(tau))-0.5*n*outer(xs+(mu-xb)^2,tau,FUN="*")
  contour(mu,tau,z,nlevels=20,xlab="mu",ylab="tau")
}

post.contour <- function(xi,ka,gam,lam,n,xb,xs){
  ## Plots contours of the posterior (n=0 gives the prior)
  
  # First, determine appropriate plotting range for mu and tau
  taup <- gam/lam #gam/lam is the prior mean of tau
  mup <- n*taup/(n*taup+ka)*xb+ka/(n*taup+ka)*xi # posterior mean of mu if tau=taup
  mu <- seq(mup-3*sqrt(1/(n*taup+ka)),mup+3*sqrt(1/(n*taup+ka)),length=32)
  lam_p <- lam+0.5*n*xs 
  gam_p <- gam+0.5*n
  #if mu=xb, posterior for tau is Gamma(lam_p,gam_p)
  tau <- seq(qgamma(0.001,shape=gam_p,rate=lam_p),
             qgamma(0.99,shape=gam_p,rate=lam_p),length=32)
  
  #Compute and plot log posterior (modulo a constant) on the lattice of values for mu and tau 
  z <- (gam_p-1)*outer(rep(1,32),log(tau))-0.5*ka*outer((mu-xi)^2,rep(1,32))+
    -outer(lam_p+0.5*n*(mu-xb)^2,tau,FUN="*")
  contour(mu,tau,z,nlevels=20,xlab="mu",ylab="tau")
}

f.gibbs <- function(xi,ka,gam,lam,n,xb,xs,nrep=1000){
  ## Implements the Gibbs-sampler for the posterior in a normal
  ## model with normal-inverse Gamma prior
  
  mu <- rep(xb,nrep)
  tau <- rep(1/xs,nrep)
  for (k in (2:nrep)){
    mu_k <- n*tau[k-1]/(n*tau[k-1]+ka)*xb+ka/(n*tau[k-1]+ka)*xi
    mu[k] <- rnorm(n=1,mean=mu_k,sd=sqrt(1/(n*tau[k-1]+ka)))
    lam_k <- lam+0.5*n*xs+0.5*n*(mu[k]-xb)^2
    tau[k] <- rgamma(1,shape=(gam+0.5*n),rate=lam_k)
  }
  list(mu=mu,tau=tau)
}



#Choose non informative prior for hyperparameters
xi <- 0
ka <- 1/100
gam <- 1
lam <- 1

#Beginning of the Gibbs-Sampler
gibbs <- f.gibbs(xi,ka,gam,lam,n,xb,xs,nrep=50)
post.contour(xi,ka,gam,lam,n,xb,xs)
points(gibbs$mu[1],gibbs$tau[1],col=4,pch=16)
i <- 0
i <- i+1
points(gibbs$mu[i+1],gibbs$tau[i],col=4,pch=16)
arrows(gibbs$mu[i],gibbs$tau[i],gibbs$mu[i+1],gibbs$tau[i],length=0.1,col=4)
points(gibbs$mu[i+1],gibbs$tau[i+1],col=4,pch=16)
arrows(gibbs$mu[i+1],gibbs$tau[i],gibbs$mu[i+1],gibbs$tau[i+1],length=0.1,col=4) 

plot_gibbs_output=function(xi,ka,gam,n,xb,xs){
  ## Plots prior density, likelihood und posterior density 
  ## together with samples from the Gibbs samples.
  ## Also plots marginal posteriors and priors.
  
  par (mfrow=c(2,2))
  post.contour(xi,ka,gam,lam,0,xb,xs)
  title(main="log prior density")
  log.lik.contour(n,xb,xs)
  title(main="log likelihood")
  points(xb,1/xs,pch=15)
  post.contour(xi,ka,gam,lam,n,xb,xs)
  points(xb,1/xs,pch=15)
  title(main="log posterior density")
  #adding output from the Gibbs sampler
  gibbs <- f.gibbs(xi,ka,gam,lam,n,xb,xs,nrep=1000)
  post.contour(xi,ka,gam,lam,n,xb,xs)
  title(main="Gibbs sampler and log posterior density")
  points(gibbs$mu,gibbs$tau)
  
  readline(prompt="Press [enter] to continue")
  
  #marginal posterior for mu
  par (mfrow=c(2,2))
  hist(gibbs$mu,breaks=20, freq=F,xlab="mu", main=
         "Histogram of samples of posterior of mu")
  d <- density(gibbs$mu)
  lines(d$x,d$y,lwd=2)
  from <- min(min(gibbs$mu),xi-2.5*sqrt(1/ka))
  to <- max(max(gibbs$mu),xi+2.5*sqrt(1/ka))
  x <- seq(from,to,length=500)
  plot(d$x,d$y,type="l",xlim=c(from,to),xlab="mu",ylab="density",
       main="prior and posterior marginal",lwd=2)
  lines(x,dnorm(x,xi,1/sqrt(ka)),col=2,lwd=2)
  legend("topright",legend=c("Prior","Posterior"),col=c(2,1),lty=1,bty="n",lwd=2)
  abline(h=0)
  #marginal posterior for tau
  hist(gibbs$tau,breaks=20,freq=F,xlab="tau", ylab="density",main=
         "Histogram of  samples of posterior of tau")
  d <- density(gibbs$tau)
  lines(d$x,d$y,lwd=2)
  from <- min(min(gibbs$tau),qgamma(0.01,shape=gam,rate=lam))
  to <- max(max(gibbs$tau),qgamma(0.99,shape=gam,rate=lam))
  x <- seq(from,to,length=500)
  y <- dgamma(x,shape=gam,rate=lam)
  plot(d$x,d$y,xlim=c(from,to),type="l",xlab="tau",ylab="density",
       main="prior and posterior marginal",lwd=2)
  lines(x,y,col=2,lwd=2)
  legend("topright",legend=c("Prior","Posterior"),col=c(2,1),lty=1,bty="n",lwd=2)
  abline(h=0)
}

#1. non informative prior
plot_gibbs_output(xi=0,ka=1/100,gam,n,xb,xs)
#2. informative prior for mu, in agreement with data
plot_gibbs_output(xi=xb,ka=1/xs,gam,n,xb,xs)
#3. informative prior for mu, conflicting the data
plot_gibbs_output(xi=xb+3*sqrt(xs/n),ka=1/xs,gam,n,xb,xs)


#Bayesian prediction: Effect of incorporating uncertainty about parameters
##Prediction interval for known paramters
upper_bound <- xb+qnorm(0.975)*sqrt(xs)
lower_bound <- xb+qnorm(0.025)*sqrt(xs)
# P(lower_bound <= new observation <=upper_bound)=0.95, if true parameter = MLE
pnorm(upper_bound,mean=xb,sd=sqrt(xs))-pnorm(lower_bound,mean=xb,sd=sqrt(xs))

ps=pnorm(upper_bound,mean=gibbs$mu,sd=1/sqrt(gibbs$tau))-pnorm(lower_bound,mean=gibbs$mu,sd=1/sqrt(gibbs$tau))
mean(ps)# P(lower_bound <= new observation <=upper_bound | data)


#######################################################
## Simulation from the posterior using RW Metropolis ##
#######################################################

# Generic function for Metropolis algorithm with random walk proposal
f.metrop <- function(x0,prop_sdv,nrep,r.target, ...){
  ## Purpose: Metropolis algorithm with Gaussian random walk proposals
  ## -------------------------------------------------------------------------
  ## Arguments: x0 = (vector of) starting values, prop_sdv = vector of
  ##            standard deviations for the random walk, nrep=number of
  ##            replicates, r.target = ratio of target densities
  
  d <- length(x0)
  x <- matrix(rnorm(d*nrep),nrow=nrep,ncol=d)
  x <- x%*%diag(prop_sdv,nrow=d)##Gaussian samples with diagonal covariance matrix for proposals
  x[1,] <- x0
  y <- matrix(0,nrow=nrep-1,ncol=d)
  nrej <- 0
  for (i in (2:nrep)) {
    y[i-1,] <- x[i-1,]+x[i,]##proposed value
    if (runif(1) <= r.target(y[i-1,],x[i-1,],...)) {
      x[i,] <- y[i-1,]##accept proposed value
    }else{
      x[i,] <- x[i-1,]##stay at current value
      nrej <- nrej+1
    }
  }
  list(sample=x,acc=1-nrej/nrep,prop=y)
}

ratio.post <- function(x1,x2,xi,ka,gam,lam,n,xb,xs){
  ## Purpose: Computing ratio of posteriori densities at x1 (proposed value) and x2 (last value)
  ##          for an i.i.d. normal model
  ## ----------------------------------------------------------------------
  ## Arguments: x=(mu,log(tau)) = Parameters of the normal distribution
  ##            xi,ka,gam,lam = Parameters of the prior
  ##            n=number of data, xb=sample mean, xs=sample variance
  
  ratio <- (n/2+gam-1)*(x1[2]-x2[2])-0.5*ka*((x1[1]-xi)^2-(x2[1]-xi)^2) -
    (lam+0.5*n*(xs+(x1[1]-xb)^2))*exp(x1[2]) +
    (lam+0.5*n*(xs+(x2[1]-xb)^2))*exp(x2[2])
    
  exp(ratio)
}


par(mfrow=c(1,1))
post.contour(xi,ka,gam,lam,n,xb,xs)
points(xb,1/xs,pch=15)
title(main="log posterior density")
# Do a few iterations (with log prop_sdv for sigma) to explain the idea
prop_sdv=c(0.75,1.5)##Proposal standard deviations
# prop_sdv=0.1*c(0.75,1.5)
metrop <- f.metrop(x0=c(xb,-log(xs)),prop_sdv=prop_sdv,nrep=21,
                   r.target=ratio.post,xi,ka,gam,lam,n,xb,xs)
sample <- metrop$sample
sample[,2] <- exp(sample[,2])
prop <- metrop$prop
prop[,2] <- exp(prop[,2])
points(sample[,1],sample[,2],col=2)
arrows(sample[-21,1],sample[-21,2],prop[,1],prop[,2],col=4)

N=1000##number of samples
#Result of many iterations 
metrop <- f.metrop(x0=c(xb,-log(xs)),prop_sdv=prop_sdv,nrep=N,
                   r.target=ratio.post,xi,ka,gam,lam,n,xb,xs)
metrop$acc  # acceptance rate
param <- metrop$sample
points(param[,1]+0.04*(runif(N)-0.5),
       exp(param[,2])+0.04*(runif(N)-0.5),cex=1)# Making overlapping points visible
par(mfrow=c(2,1))
plot(param[1:500,1],type="l")
plot(param[1:500,2],type="l")
#Autocorrelations 
acf(param[,1])
acf(param[,2])

#marginal posterior for mu
par (mfrow=c(2,2))
hist(param[,1],breaks=20, freq=F,xlab="mu", main=
       "Posterior of mu")
d <- density(param[,1])
lines(d$x,d$y)
from <- min(min(param[,1]),xi-2.5*sqrt(1/ka))
to <- max(max(gibbs$mu),xi+2.5*sqrt(1/ka))
x <- seq(from,to,length=500)
plot(d$x,d$y,type="l",xlim=c(from,to),xlab="mu",ylab="density",
     main="Prior and posterior of mu")
lines(x,dnorm(x,xi,1/sqrt(ka)),col=2)
abline(h=0)
#marginal posterior for tau
hist(exp(param[,2]),breaks=20,freq=F,xlab="tau", ylab="density",main=
       "Posterior of tau")
d <- density(exp(param[,2]))
lines(d$x,d$y)
from <- min(min(exp(param[,2])),qgamma(0.01,shape=gam,rate=lam))
to <- max(max(exp(param[,2])),qgamma(0.99,shape=gam,rate=lam))
x <- seq(from,to,length=500)
y <- dgamma(x,shape=gam,rate=lam)
plot(d$x,d$y,xlim=c(from,to),type="l",xlab="tau",ylab="density",
     main="Prior and posterior of tau")
lines(x,y,col=2)
abline(h=0)

