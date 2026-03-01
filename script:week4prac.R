data(iris)
data(mtcars)
RNGversion("4.5.2")
set.seed(1234)
mtcars_scaled <- scale(mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")])
mtcars_fa <- factanal(mtcars_scaled, factors = 2, rotation = "varimax", scores = "regression")
mtcars_fa
#a. Extract and interpret the factor loadings.
#factor one 3.317 SS loading is the strongest factor, explaining 55.3% of the variance of the data. factor 2 explains 28.7% . wt has high loading among disp and drat (negative) and mpg (negative). Factor one might represent how big a car is . car is heavier and more powerful but mpg goes down. 

#b. Generate a biplot of the factor loadings using arrows to show the direction of each loading.
mtcar_FA_scores<-mtcars_fa$scores
plot(mtcar_FA_scores[,1], mtcar_FA_scores[,2] , xlab = "Fact 1 ", ylab = "Fact 2", pch = 19, col = "steelblue",  xlim = c(-3, 3), ylim = c(-3, 3))

# 2. Add the car names instead of dots
text(mtcars_fa$scores[,1], mtcars_fa$scores[,2], 
     labels = rownames(mtcars_fa$scores), 
     cex = 0.7, col = "steelblue")


loadings <- mtcars_fa$loadings[, 1:2]
arrows(0, 0, loadings[,1] * 2.5, loadings[,2] * 2.5, 
       length = 0.09, col = "red", lwd = 2)

text(loadings[,1] * 2.8, loadings[,2] * 2.8, 
     labels = rownames(loadings), col = "red", cex = 1.2)

#Compute factor scores and examine how cars are positioned in the latent factor space.
#already done 

#Briefly explain a key conceptual difference between Principal Component Analysis (PCA) and Factor Analysis (FA).
#FA is using underlying latent unobservable variables to explain the covariance between the variables/data. PCA uses orthogonal linear combinations to max total variance. 

# While MANOVA compares mean vectors across groups, CCA focuses on the strength and structure of association between two sets of continuous variables.

data(iris)
X <- scale(iris[, c("Sepal.Length" , "Sepal.Width")])
Y <- scale(iris[, c("Petal.Length" , "Petal.Width")])

cca_iris <- cancor(X,Y)
cca_iris


#u1 = -0.0725(sepallength - 1.457e-15) + 0.0305(sepalwidth- 1.63e-15) 

#sepal coeff
cca_iris$xcoef
#petal coeff

cca_iris$ycoef

# Loadings for X set
X_loadings <- cor(X, X %*% cca_iris$xcoef)

# Loadings for Y set
Y_loadings <- cor(Y, Y %*% cca_iris$ycoef)

U1 <- X %*% cca_iris$xcoef[,1]
#u1 represents a concept, its latent hidden. 
V1 <- Y %*% cca_iris$ycoef[,1]
#inner dimensions must match. 
plot(U1, V1)
#If I know the summary of a flower's Sepal shape (), I can predict its Petal shape ( very accurately.
#cca_iris$cor[1] 0.94 meaning sepal dimensions as a group relate to petal dimesion as a group. 


RNGversion("4.5.2")
set.seed(1234)
iris_km <- kmeans(iris[,-5], centers = 3)
#extract cluster size 
iris_km$size
#extract. cluster cetnres
iris_km$centers
table(iris$Species, iris_km$cluster)

#high accuracy of setoa, virginica low accuracy 

wss <- numeric(10) # Create a place to store the 10 WSS values

for (i in 1:10) {
  # Run k-means for each k from 1 to 10
  km_out <- kmeans(iris[, -5], centers = i, nstart = 20)
  
  # Save the Total Within-Cluster Sum of Squares
  wss[i] <- km_out$tot.withinss
}
#twcss :it measures how "tight" its own groups are. not related to the true value. withinss is the distance from each flower to the middle of its own group, not to its true species label.

plot(1:10, wss, type = "l", 
     xlab = "Number of Clusters(k)", 
     ylab = "Total Within-Cluster Sum of Squares",
     main = "Elbow Method for Optimal k")
#wss drops significantly after k=3 so 3 clsuters is optimal 

#Gaussian Mixture Models (GMM)
library(mclust)
data(iris)
data_gm = iris[,1:4]
fit_gmm = Mclust(data_gm)

plot(fit_gmm, what = "density")
#petal length creates well seperate clusters. sepal width show overlap. 
#2 clusters, VEV model. 
table(iris$Species,fit_gmm$classification)

	#Heirarchical clustering
	iris_dist <- dist(scale(iris[,1:4]), method = "euclidean")
	iris_hc <- hclust(iris_dist, method = "average")
	
	plot(iris_hc)
	rect.hclust(iris_hc,k=3)

# 1. Cut the tree into exactly 3 groups
iris_hc_clusters <- cutree(iris_hc, k = 3)

# 2. Show the sizes of each cluster
table(iris_hc_clusters)

library(MASS)
iris_lda <- lda(
Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
data = iris
)
iris_lda


summary(iris_lda)
lda_pred = predict(iris_lda)
#confusion matrix
table(lda_pred$class, iris$Species)

#plot first two linear discriminants showing species separation 
plot(iris_lda)

#LDA assumption assumes that covariance matrix betweenc lasses are the same. #Homoscedasticity.

library(klaR)

partimat(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, data = iris, method = "lda")







