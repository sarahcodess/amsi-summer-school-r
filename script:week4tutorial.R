#Suppose we standardised our data using Y= XS−
# , where S is the sample covariance matrix. Is
#performing a PCA on the standardised data Y meaningful? Please justify your answer

#Answer:  no not meaningful . what that does is whitening . removing correlation betwen variables. Variance in x is 1 and variance in y direction is 1. 
#For any transformed data Y = XA, COV(Y) = A\topCov(X)A . Cov(Y) = S^{-1/2}^\top S S^{-1/2}. Since S is a covariance matrix, it is symmetric. Cov(Y) = S^{-1/2}SS^{-1/2} = S^{-1/2+1-1/2} = S^{0} . Cov(Y) = I. 

#Calculate sample principal components and their variances 
x_bar <- matrix(c(155.60, 14.70), nrow = 2, byrow = TRUE)

S <- matrix(c(7476.45, 303.62, 303.62, 26.19), nrow=2, byrow = TRUE)
n=20
eigen <- eigen(S)

eigenvalues_S <- eigen$values

eigenvectors_S <- eigen$vectors



