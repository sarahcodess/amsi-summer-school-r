x_bar <- c(46.1,57.3,50.4)
S <- matrix(c(101.3, 64.0 ,71.0 ,63, 80.2, 55.6, 71, 55.6 ,97.4), nrow = 3, byrow = TRUE)

#All three indices are evaluated for each patient. Test for the equality of mean indices with α = 0.05

#one sample test
C <- matrix(c(1, -1 ,0 ,1 ,0 ,-1), nrow = 2, byrow = TRUE)
# now use T^2 linear combination 

m=2
p = 3
n = 40

T_sq = n * t(C%*%x_bar) %*% solve(C%*%S%*%t(C)) %*% ((C%*%x_bar))
T_sq
Critical_value = (((n-1)*m) / (n-m)) * qf(0.05, m, n-m, lower.tail = FALSE)
Critical_value
#Since t_sq is > critical value, we reject H_0 



#Judge the difference in pairs of mean indices using 95% simultaneous confidence intervals
#we want to find out what means are different 
C_updated <- matrix(c(1, -1 ,0 ,1 ,0 ,-1, 0, 1, -1), nrow = 3, byrow = TRUE)
m_updated = 3
Critical_value_updated = (((n-1)*m_updated) / (n-m_updated)) * qf(0.05, m_updated, n-m_updated, lower.tail = FALSE)
Critical_value
Cxbar <- as.vector(C_updated %*% x_bar)
CSC <- C_updated %*% S %*% t(C_updated)
#first difference 
first_contrast_lower <- Cxbar[1] - sqrt(Critical_value_updated) * sqrt((CSC[1,1])/n)
first_contrast_upper <- Cxbar[1] + sqrt(Critical_value_updated) * sqrt((CSC[1,1])/n)
ci_interval <- cbind(first_contrast_lower,first_contrast_upper )
ci_interval

#second difference
second_contrast_lower <- Cxbar[2] - sqrt(Critical_value_updated) * sqrt((CSC[2,2])/n)
second_contrast_upper <- Cxbar[2] + sqrt(Critical_value_updated) * sqrt((CSC[2,2])/n)
ci_interval2 <- cbind(second_contrast_lower,second_contrast_upper )
ci_interval2

#third difference
third_contrast_lower <- Cxbar[3] - sqrt(Critical_value_updated) * sqrt((CSC[3,3])/n)
third_contrast_upper <- Cxbar[3] + sqrt(Critical_value_updated) * sqrt((CSC[3,3])/n)
ci_interval3 <- cbind(third_contrast_lower,third_contrast_upper )
ci_interval3
#none of these contain 0, so means are significant to eachother 

#peanut crop question 2 

#3 varieties, 2 geographical locations, and 3 variables measured


peanuts <- matrix(scan(text="
1 5 195.3 153.1 51.4
1 5 194.3 167.7 53.7
2 5 189.7 139.5 55.5
2 5 180.4 121.1 44.4
1 6 203.0 156.8 49.8
1 6 195.9 166.0 45.8
2 6 202.7 166.1 60.4
2 6 197.6 161.8 54.1
1 8 193.5 164.5 57.8
1 8 187.0 165.1 58.6
2 8 201.5 166.8 65.0
2 8 200.0 173.8 67.2
"), ncol=5, byrow=TRUE)

colnames(peanuts) <- c("location","variety","Yield","StMatKer","SeedSize")

peanut_df <- as.data.frame(peanuts)
peanut_df$location <- factor(peanut_df$location)
peanut_df$variety  <- factor(peanut_df$variety)

peanut_manova <- manova(cbind(Yield, StMatKer, SeedSize) ~ location * variety, data = peanut_df)

summary(peanut_manova, test = "Wilks")

#q3 bivariate model 
#calculate least square estimate 
X = matrix(c(1, 1, 1, 1, 1, -2, -1, 0, 1, 2), nrow = 5)
Y = matrix(c(5,3,4,2,1,-3,-1,-1,2,3), nrow = 5)
beta_hat = solve(t(X)%*%X) %*% (t(X) %*%Y)

Y_fitted = X %*% beta_hat
error = Y - Y_fitted

#verity sum of squares and cross products sscp 
LHS <- t(Y) %*% Y 
RHS <- t(Y_fitted) %*% Y_fitted + t(error) %*% error
x0 = 0.5
#calculate 95 confidence interval for mean response e(y10) = b01+b11x0  where x0 = 0.5 

Y0hat= beta_hat[1,1] + beta_hat[2,1]*0.5
n <- nrow(Y)
p <- 2

S = (t(error) %*% error ) / (n-p)

tcrit <- qt(0.975, df = n - p)

s11 <- S[1,1]    

XtX_inv <- solve(t(X) %*% X)
c0 <- matrix(c(1, x0), nrow = 1)
varY0 <- s11 * (c0 %*% XtX_inv %*% t(c0))
seY0  <- sqrt(varY0)


CI <- c(Y0hat - tcrit*seY0, Y0hat + tcrit*seY0)
