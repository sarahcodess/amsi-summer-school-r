bear <- read.table("bear.txt", header = TRUE)
X = bear[,5:8]
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
 
 
 
 