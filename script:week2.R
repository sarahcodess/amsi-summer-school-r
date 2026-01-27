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

plot(ell, type="l", asp=1,
     xlab="Mean increase 2→3",
     ylab="Mean increase 4→5")
rect(lower23, lower45, upper23, upper45, lty=2)
points(meandiff[1], meandiff[2], pch=19)

#KITES DATA
#find and sketch 95% confidence ellipse for population mean
kites <- read.table("kites.txt", header=TRUE)

 n_kites = nrow(kites)
 p_kites = ncol(kites)
 x_bar_kites = apply(kites, 2, mean)
 sample_cov_kites = cov(kites)
critical_value = sqrt((p_kites*(n_kites-1)/(n_kites-p_kites)) * qf(.95, p_kites, n_kites-p_kites))
ell <- ellipse(sample_cov_kites, centre = x_bar_kites, t = (critical_value)/sqrt(n_kites))

plot(ell, type="l", asp=1,
     xlab="Mean tail",
     ylab="Mean wing")
points(x_bar_kites[1], x_bar_kites[2], pch=19)
 
points(190,275,col="blue",pch=4,lwd=3)
#hypothesised vector 190 275 is in the confidence ellipse. 

#Construct simultaenous 95% for mu1 and mu2 and the 95% Bonferroni intervals for mu1 and mu2. 

#simultaiens t^2 intervals 95%
kite_mu1_lower = x_bar_kites[1] - sqrt((p_kites*(n_kites-1))/(n_kites - p_kites)*qf(0.95,p_kites, n_kites-p_kites)) * sqrt(sample_cov_kites[1,1]/n_kites)
kite_mu1_upper = x_bar_kites[1] + sqrt((p_kites*(n_kites-1))/(n_kites - p_kites)*qf(0.95,p_kites, n_kites-p_kites)) * sqrt(sample_cov_kites[1,1]/n_kites)

kite_mu2_lower = x_bar_kites[2] - sqrt((p_kites*(n_kites-1))/(n_kites - p_kites)*qf(0.95,p_kites, n_kites-p_kites)) * sqrt(sample_cov_kites[2,2]/n_kites)
kite_mu2_upper = x_bar_kites[2] + sqrt((p_kites*(n_kites-1))/(n_kites - p_kites)*qf(0.95,p_kites, n_kites-p_kites)) * sqrt(sample_cov_kites[2,2]/n_kites)

mu1_ci = c(kite_mu1_lower, kite_mu1_upper)
mu1_ci
mu2_ci = c(kite_mu2_lower, kite_mu2_upper)
mu2_ci
 
 
 #bonferroni intervals 95%
 t_value = qt(0.05/(2*p_kites), df = n_kites-1, lower.tail= FALSE)
 bon_mu1_lower = x_bar_kites[1] - t_value*sqrt(sample_cov_kites[1,1]/n_kites)
bon_mu1_upper = x_bar_kites[1] + t_value*sqrt(sample_cov_kites[1,1]/n_kites)
 bon_mu2_lower = x_bar_kites[2] - t_value*sqrt(sample_cov_kites[2,2]/n_kites)
bon_mu2_upper = x_bar_kites[2] + t_value*sqrt(sample_cov_kites[2,2]/n_kites)
bon_mu1_ci = c(bon_mu1_lower, bon_mu1_upper)
bon_mu2_ci = c(bon_mu2_lower, bon_mu2_upper)
bon_mu1_ci
bon_mu2_ci


ell <- ellipse(sample_cov_kites, centre = x_bar_kites, t = (critical_value)/sqrt(n_kites))

plot(ell, type="l", asp=1,
     xlab="tail",
     ylab="wing")
points(x_bar_kites[1], x_bar_kites[2], pch=19)

rect(mu1_ci[1], mu2_ci[1], mu1_ci[2], mu2_ci[2], lty=2)
rect(bon_mu1_ci[1], bon_mu2_ci[1], bon_mu1_ci[2], bon_mu2_ci[2], border = 'red', lty=2)
legend("bottomleft",
       legend = c("Hotelling T^2 ellipse",
                  "Hotelling SCI rectangle",
                  "Bonferroni CI rectangle"),
       lty    = c(1, 2, 2),
       col    = c("black", "black", "red"),
       lwd    = c(1, 1, 2),
       bty    = "n")
       
       
       
 ##is the bivariate normal distribution a viable population model? Explai with reference to Q-Q plot and a scatter diagram 
 
 qqnorm(kites$tail)
 qqline(kites$tail)
 
 qqnorm(kites$wing)
 qqline(kites$wing)
 
 
 plot(kites)
 
 
#Number parity data 
number.parity <- read.table("number_parity.txt", header=TRUE)
#test for treatment effects using a repeatedmeasures design. alpha - 0.05

#contrasts 
#parity effect = (mu_3+ mu_1) - (mu4 + mu2)
#formateffect = (mu2+mu1) - (mu4 + mu3)
#interactioneffect = (mu2+mu3) - (mu4+mu1)

n_number = nrow(number.parity)
p_number = ncol(number.parity)
C=matrix(c(-1,1,-1,1,-1,-1,1,1,-1,1,1,-1),nrow=3,ncol=4,byrow=TRUE)
xbar = (apply(number.parity,2,mean))
S = cov(number.parity)
#compute T^2 statistic, ie T2 observed  
T_sq = n_number * t(C %*% xbar) %*% solve(C %*% S %*% t(C)) %*% (C %*% xbar)
T_sq
# Obtain upper 5% percentile for Hotelling's T2
T2_critical=((p_number-1)*(n_number-1)/(n_number-p_number+1))*qf(0.05,p_number-1,n_number-p_number+1,lower.tail=F) 
T2_critical
 #compute p value 
 #p value is just the prob of observing more than the t2 observed
pvalue <- pf( T_sq * (n_number - p_number + 1) / ((n_number - 1) * (p_number - 1)),
              df1 = p_number - 1,
              df2 = n_number - p_number + 1,
              lower.tail = FALSE )

pvalue
 
#Construct the 95% simultaneous confidence intervals for the contrasts representing the number format effect, the parity type effect and the interaction effect. Interpret the resulting intervals.
low=up=rep(0,3) 
for (i in 1:3)
{ 
	low[i]=(C%*%xbar)[i,]-sqrt(T2_critical)*sqrt((C%*%S%*%t(C))[i,i]/n_number) 
	up[i]=(C%*%xbar)[i,]+sqrt(T2_critical)*sqrt((C%*%S%*%t(C))[i,i]/n_number)
	 } 
	cbind(low,up)
