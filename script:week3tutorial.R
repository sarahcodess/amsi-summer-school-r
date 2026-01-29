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
