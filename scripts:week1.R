A <- matrix(c(1,2,3,4), nrow=2 , byrow=TRUE)
B <- matrix(c(5, 6, 7, 8), nrow = 2, byrow = TRUE) 
C <- matrix(c(2, 0, 1, 2), nrow = 2, byrow = TRUE)
t(A)
A+B
A-B
A %*% C

kronecker(A,C)
det(A)
D <-matrix(c(2,1,1,2), nrow=2, byrow=TRUE)
eigen_D <- eigen(D)
eigen_D$values
eigen_D$vectors
eigen_D$vectors %*% diag(eigen_D$values) - D %*% eigen_D$vectors

#compute transpose of B
t(B)
value <- A + t(A)
E <- matrix(c(2,6,1,3), nrow= 2, byrow = TRUE)
det(E)

# dot product between the two eigenvectors:
sum(eigen_D$vectors[,1] * eigen_D$vectors[,2])

data("iris")
head(iris)
mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
cov(iris$Petal.Length, iris$Sepal.Length)

#Calculate the mean and standard deviation of all numeric variables in the iris data.

mean_sepal_length <-mean(iris$Sepal.Length)
mean_sepal_width <- mean(iris$Sepal.Width)
mean_petal_length  <-mean(iris$Petal.Length)
mean_petal_width <- mean(iris$Petal.Width)

sd_sepal_length <- sd(iris$Sepal.Length)
sd_sepal_width <- sd(iris$Sepal.Width)
sd_petal_length <- sd(iris$Petal.Length)
sd_petal_width <- sd(iris$Petal.Width)

#Compute the covariance matrix for all numeric variables in the iris data.
covariance_matrix_iris = cov(iris[, 1:4], use = "everything")

#correlation matrix 
correlation_matrix_iris = cov2cor(covariance_matrix_iris )
(correlation_matrix_iris)









