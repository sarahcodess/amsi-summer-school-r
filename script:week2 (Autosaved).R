bear <- read.table("bear.txt", header = TRUE)
X = bear[,5:8]
X

n <- nrow(X)
p <- ncol(X)
Xbar = apply(X,2,mean)
S = cov(X)

#T^2 Simultaneous Confidence Intervals 
LB = Xbar - sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(diag(S/n))
UB = Xbar + sqrt(p*(n-1)/(n-p)*qf(0.95,p,n-p))*sqrt(diag(S/n))

LB
UB
cbind(LB, UB)



C <- rbind(
  c(-1, 1, 0, 0),
  c(0, -1, 1, 0),
  c(0, 0, -1, 1)
)
# Build confidence intervals
lower = as.vector(C %*% Xbar) - sqrt((p * (n - 1)) / (n - p) * qf(0.95, p, n - p)) *
          sqrt(diag(C %*% S %*% t(C)) / n)

upper = as.vector(C %*% Xbar) + sqrt((p * (n - 1)) / (n - p) * qf(0.95, p, n - p)) *
          sqrt(diag(C %*% S %*% t(C)) / n)
 
 cbind(lower, upper)
 
 #Confidence Region
 #mu : n


dif23 = ((X$L3-X$L2))
dif45 = (X$L5 - X$L4)
X = cbind(dif23, dif45)
xbar = colMeans(X)
S = cov(xbar)
n <- 7; p <- 2

cutoff = (p*(n-1)/(n-p)) * qf(.95, p, n-p)
ell <- ellipse(S/n, centre = xbar, t = sqrt(cutoff))

plot(ell, type="l", asp=1,
     xlab="Mean increase 2→3",
     ylab="Mean increase 4→5")
points(xbar[1], xbar[2], pch=19)


 