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