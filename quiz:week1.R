#section 2 Matrix Algebra 
X <- matrix(c(1,1,-2,-1,2,1,0,1,-1), nrow = 3, byrow = TRUE)
eigen_X <- eigen(X)
eigenvalues_X = eigen_X$values
#eignvalues are [2,1,-1]
eigenvectors_X = eigen_X$vectors
eigenvector_X_1 = eigenvectors_X[,1] 
#0.3015113 0.9045340 0.3015113
eigenvector_X_2=eigenvectors_X[,2]
#-0.8017837 -0.5345225 -0.2672612
eigenvector_X_3=eigenvectors_X[,3]
#7.071068e-01 1.281975e-16 7.071068e-01
#check if they are perpendicular, compute pairwise dot product of eigenvectors 
t(eigenvector_X_1) %*%eigenvector_X_2
t(eigenvector_X_1) %*%eigenvector_X_3

t(eigenvector_X_2) %*%eigenvector_X_3

#q9 Is X positive definite?
#positive definite means all eigenvalues are positive and non zero. 
#since eigenvalues are 2, 1 -1, X is not positive definite. 

#q10. Is X singular? Singular means the determinate is 0 
detX <- det(X)
#detX = -2. Therefore, it is not singular. 


#section three : sample statistics

library(MASS)
data(crabs)
head(crabs)

fl_xbar <- mean(crabs$FL)
#fl_xbar = 15.583
rw_xbar <- mean(crabs$RW)
#rw_xbar= 12.7385
cl_xbar <- mean(crabs$CL) 
#32.1055
cw_xbar <- mean(crabs$CW)
#36.4145
bd_xbar <- mean(crabs$BD) 
#14.0305

#question compute sample covariance
X_matrix  <- matrix(c(crabs$FL, crabs$RW, crabs$CL, crabs$CW, crabs$BD), nrow = 200, byrow=TRUE)
dim(X_matrix)
S <- 1/(200-1) * t(X_matrix)%*%(diag(200)-(1/200)*rep(1,200)%*%t(rep(1,200)))%*% X_matrix
S
#alternatively use cov(X)
S_1 <- cov(X_matrix)
S_1
#[1,] 121.6804 122.8717 124.0665 125.3403 128.9379
#[2,] 122.8717 124.9961 126.1000 127.4494 131.2598
#[3,] 124.0665 126.1000 127.9446 129.0003 132.8458
#[4,] 125.3403 127.4494 129.0003 130.8131 134.4104
#[5,] 128.9379 131.2598 132.8458 134.4104 138.9959

#13. Compute the sample correlation matrix R. 
R = cor(X_matrix)
#[1,] 1.0000000 0.9963079 0.9943361 0.9934698 0.9914469
#[2,] 0.9963079 1.0000000 0.9971391 0.9966986 0.9958238
#[3,] 0.9943361 0.9971391 1.0000000 0.9971350 0.9961755
#[4,] 0.9934698 0.9966986 0.9971350 1.0000000 0.9967961
#[5,] 0.9914469 0.9958238 0.9961755 0.9967961 1.0000000


#14. Find the total sample variance.
Total_sample_variance <- sum(diag(S))
#= 644.43


# 15. Find the generalized sample variance.
generalised_sample_variance = det(S)

#35.13111
