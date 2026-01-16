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


