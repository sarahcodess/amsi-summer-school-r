data(iris)
iris_manova <- manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)

s <- summary(iris_manova, test = "Wilks")

#p is very small, meaning we reject the null hypothesis. Suggests iris species differ jointly in the mean sepal length and sepal width. Using length and width to see if species differ. 

W <- s$SS$Residuals
B <- s$SS$Species

lambda_stat = det(B) / det(B+W)
lambda_stat

names(s$SS)
heplot(iris_manova, fill = T)



#new dataset
data(mtcars)
names(mtcars)
mtcars_manova <- manova(cbind(mpg, disp, hp) ~ am*vs, data=mtcars )
car_summary<-summary(mtcars_manova, test = "Wilks")

head(mtcars)

#mean vectors differ for am as p very small 

#mean vectors differ for vs as p very small #transmission is associated with large shifts particularly in disp and hp

#vs associated with large shifts with disp 


#Multivariate Regression 
head(iris)
iris_lm <- lm(cbind(Sepal.Length, Sepal.Width) ~ Species, data=iris)
iris_manova <- manova(iris_lm)
iris_manova
#for sepal length, SS due to species is 63.21. This means species explains alot of the total variation of sepal length Sepal.Length shows stronger separation by species than Sepal.Width does (because the Species SS is larger relative to Residual SS).

#pillai/wilks table 
summary(iris_manova, test = "Pillai")
#whether species affects joint mean vector of responses.  mu vector for each group. contains length and width. 
library(car)
#Pillai stat is 0.94531. evidence that mean differs across the different groups ie. species.
Anova(iris_lm, type = 3, test.statistic = "Pillai")
#so yes, pillai stat is high, ss of sepal length due to species is high . 

#. Compute the Mahalanobis distances based on sepal measurements. (Hint: Use the mahalanobis function.)
X <- iris[, cbind("Sepal.Length", "Sepal.Width")]
x_bar = apply(X, 2, mean)
distance <- mahalanobis(X, x_bar, cov(X))
distance


#Principal Component Analysis (PCA)
#We will now use Principal Component Analysis (PCA) to reduce the dimensionality of the iris dataset while retaining most of the variance in the data, and visualise the multivariate structure. PCA identifies new uncorrelated variables (principal components) that are linear combinations of the original variables.
iris_pca <- princomp(iris[, 1:4], cor = TRUE, score = TRUE)
summary(iris_pca)

#Interpret the proportion of variance explained by the first two principal components.
#comp1 prop variance. 0.7296 , comp2 prop variance is 0.2285 so the first to pc's explain 95.81% of the variance
#extract pc loadings and interpret the loadings for the first two principles 
#loadings is not regression coefficients, they are PCA loadings ie weights. 
iris_pca$loadings
#order Sepal Length, width, petal lnegth and width . 
#pca 1. 0.521 SL - 0.269 SW. + 0.58 PL + 0.565 PW big loadings on PL and PW. 
#pca2 = 0.377SL + 0.923 SW. Dominated by sepal width. 
#So PCA1 0.729 driven mainly by PL and PW , pca2 .2285 captures mainly sepal width and length. Along PC1, petal and speal length are positive while sw are negative. 
screeplot(iris_pca, type = "lines")

#Compute the eigenvalues of the correlation matrix directly and verify they match the variances of the principal components. Explain the relationship between eigenvalues and explained variance.

X = iris[,1:4]
corr_x = cor(X)
eigenvalues_iris = eigen(corr_x)
names(iris_pca)
iris_pca$sdev^2

#both the eigenvalues and pc's are the same. variance of pc's is eigenvalue lambda.  where varpca1 = aCor(X)a . variance of a linear combination rule Var(a^tZ) = a^tSigmaa
#Extract the principal component scores and explain what the scores represent geometrically.

scores_iris = iris_pca$scores
#observing that each subsequent score is smaller. so large pc1 score means large petals and long sepals based on my oading. #scores are where observations lie on those principal components so each score is the 'y' of the equation. according to each pc equation. 

plot(iris_pca$scores[,1], iris_pca$scores[,2],
     col = iris$Species,
     xlab = "PC1", ylab = "PC2")
legend("topright", legend=levels(iris$Species), col=1:3, pch=19, bty="n")
#biplot of scores (lots of scatter) and loading (arrow)

loadings <- (iris_pca$loading)[,1:2] * 3
text(loadings[,1], loadings[,2], labels = rownames(loadings), font = 2)

#biplot(iris_pca)



