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


#eigenvalues : 7488.80293   13.83707
#eigenvectors : [-0.99917337   -0.04065185] ,  [-0.99917337 0.04065185]

#y1 = -0.9992x1 + 0.04065x2 , % = 0.9981557065
#y2 = - 0.04065x1 -0.99917337x2 +  % = 0.001844293475


#cor(y_1hat , x1) and cor(y_2hat,x2) 
cor_y1_x1 <- eigenvectors_S[1,1] * sqrt(eigenvalues_S[1]) / sqrt(S[1,1])
#-0.9999985
cor_y2_x2 <- eigenvectors_S[2,2] * sqrt(eigenvalues_S[2]) / sqrt(S[2,2])
#-0.7262652

#refer to question 2. convert s to the sample correlation matrix R. 

diag_elements <- diag(S)

D_inv_sqrt <- diag(diag_elements^(-1/2))


R <- D_inv_sqrt %*% S %*% D_inv_sqrt

# View the result
print(R)




