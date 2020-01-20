library(matlib)

###### infinite solutions:

A <- matrix(c(1, -2, -4, 8), 2, 2)
b <- c(0, 0)
showEqn(A, b)

par(mar = c(4, 4, 0.5, 0.5))
par(mgp = c(2.5, 1, 0))
par(cex.lab = 1.25)
plotEqn(A, b)

###### unique, trivial solution:

A <- matrix(c(1, -2, -4, 1), 2, 2)
b <- c(0, 0)
showEqn(A, b)

plotEqn(A, b)

solve(A, b)

###### no solutions:

A <- matrix(c(1, -2, -4, 8), 2, 2)
b <- c(2, 2)
showEqn(A, b)

plotEqn(A, b)

###### unique, non-trivial solution:

A <- matrix(c(1, -2, -4, 1), 2, 2)
b <- c(2, 2)
showEqn(A, b)

plotEqn(A, b)

solve(A, b)

######

parms <- c(f0 = 1, f1 = 1.5, 
           q_00 = 0.6, q_01 = 0.4, 
           q_10 = 0.2, q_11 = 0.8)
times <- c(0:200)/25
initconds <- c(a = 0.75, b = 0.25)
M = matrix(c(parms["f0"] * parms["q_00"], parms["f0"] * parms["q_01"],
             parms["f1"] * parms["q_10"], parms["f1"] * parms["q_11"]),
           2, 2)

eig = eigen(M)
eig$values
x_star = eig$vectors[, 1]/sum(eig$vectors[, 1])
x_star # normalised leading eigenvector

# phi corresponds to largest eigenvalue:
sum(x_star * c(parms["f0"], parms["f1"]))

# Solve IVP:
ivp = solve(eig$vectors, initconds)
Xa = ivp[1] * eig$vectors[1, 1] * exp(eig$values[1] * times) +
  ivp[2] * eig$vectors[1, 2] * exp(eig$values[2] * times)
Xb = ivp[1] * eig$vectors[2, 1] * exp(eig$values[1] * times) +
  ivp[2] * eig$vectors[2, 2] * exp(eig$values[2] * times)

plot(times, Xa, xlab = "time", ylab = expression(X), main = "",
     col = "dodgerblue", type = "l", lwd = 2)
lines(times, Xb, col = "#ff8c00", lwd = 2)

plot(times, Xa, xlab = "time", ylab = expression(X), main = "",
     col = "dodgerblue", type = "l", lwd = 2, log = "y")
lines(times, Xb, col = "#ff8c00", lwd = 2)

plot(times, Xa/(Xa + Xb), ylim = c(0, 1), xlab = "time", ylab = expression(X),
     main = "", col = "dodgerblue", type = "l", lwd = 2)
lines(times, Xb/(Xa + Xb), col = "#ff8c00", lwd = 2)

######

library(deSolve)
my.atol <- c(1e-06)
sdiffeqns <- function(t, x, p) {
  dxa <- x[1] * p["f0"] * p["q_00"] + x[2] * p["f1"] * p["q_10"] -
    (x[1] * p["f0"] + x[2] * p["f1"]) * x[1]
  dxb <- x[1] * p["f0"] * p["q_01"] + x[2] * p["f1"] * p["q_11"] -
    (x[1] * p["f0"] + x[2] * p["f1"]) * x[2]
  list(c(dxa, dxb))
}
out <- lsoda(initconds, times, sdiffeqns, parms, rtol = 1e-10, atol = my.atol)
plot(out[, 1], out[, 2], xlab = "time", ylab = "x", main = "", col = "dodgerblue", 
     lwd = 2, ylim = c(0, 1), xlim = c(0, 8), type = "l")
lines(out[, 1], out[, 3], col = "#ff8c00", lwd = 2)





