library(ellipse)
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
 
 #Confidence Region Ellipse
 #mu : n


dif23 = ((X$L3-X$L2))
dif45 = (X$L5 - X$L4)
Difference = cbind(dif23, dif45)
xbar = colMeans(Difference)
S = cov(Difference)
n <- 7; p <- 4

cutoff = sqrt((p*(n-1)/(n-p)) * qf(.95, p, n-p))
ell <- ellipse(S, centre = xbar, t = (cutoff)/sqrt(n))

plot(ell, type="l", asp=1,
     xlab="Mean increase 2→3",
     ylab="Mean increase 4→5")
points(xbar[1], xbar[2], pch=19)


#Bonferroni Confidence INtervals 
X = bear[,5:8]
x_bar = apply(X, 2, mean)
m = 7
tstar = qt(0.05/(2*m), df = 6, lower.tail= FALSE)

mu.L=x_bar-tstar*matrix(sqrt(diag(S/n)),ncol=1)
mu.U = x_bar+tstar*matrix(sqrt(diag(S/n)),ncol=1)


dif23 = ((X$L3-X$L2))
dif34 =  ((X$L4-X$L3))
dif45 = (X$L5 - X$L4)

dif23 = X$L3 - X$L2

mean23 = mean(dif23)
se23   = sqrt(var(dif23) / (n))

lower23 = mean23 - tstar * se23
upper23 = mean23 + tstar * se23

dif34 = X$L4 - X$L3

mean34 = mean(dif34)
se34   = sd(dif34) / sqrt(n)

lower34 = mean34 - tstar * se34
upper34 = mean34 + tstar * se34


dif45 = X$L5 - X$L4

mean45 = mean(dif45)
se45   = sd(dif45) / sqrt(n)

lower45 = mean45 - tstar * se45
upper45 = mean45 + tstar * se45
cbind(mu.L,mu.U)
ci_23 = cbind(lower23, upper23)
ci_34= cbind(lower34, upper34)
ci_45= cbind(lower45, upper45)


plot(NA, NA, asp = 1,
     xlim = c(lower23, upper23),
     ylim = c(lower45, upper45),
     xlab = "Mean increase 2→3",
     ylab = "Mean increase 4→5")

rect(lower23, lower45, upper23, upper45, lty = 2)

## plot the mean point
points(mean23, mean45, pch = 19)
 