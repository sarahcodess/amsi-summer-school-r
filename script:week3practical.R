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