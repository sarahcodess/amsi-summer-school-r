bird_data <- read.table("bumpus3.txt", header = TRUE)

survived_birds1 <- subset(bird_data , Survive == 1)
survived_birds <- (survived_birds1[3:7])

n_survive <- nrow(survived_birds)
mean_vector_1 <- (survived_birds[1:5])
x_bar_survived <- apply(mean_vector_1, 2, mean)

perished_birds1 <- subset(bird_data, Survive ==0)
perished_birds <- (perished_birds1[3:7])
 n_perish <- nrow(perished_birds)



S_1 = cov(survived_birds)
S_2 = cov(perished_birds)
p = ncol(survived_birds)

mean_vector_2 <- (perished_birds[1:5])
x_bar_perished <- apply(mean_vector_2, 2, mean)

S_p = ((n_survive - 1)* S_1 + (n_perish - 1) * S_2) / (n_survive + n_perish - 2)

t_statistic = t(x_bar_survived - x_bar_perished) %*% solve(((1/n_survive)+ (1/n_perish)) * S_p)  %*% (x_bar_survived - x_bar_perished)
t_statistic 
t_critical =( (  n_perish + n_survive - 2 )* p) / (n_perish + n_survive - 1 - p) * qf(0.05, n_survive + n_perish - 1-p, lower.tail = FALSE)

t_critical

#t_stat > t_crit therefore reject null hypothesis meaning that there is significant stats to suggest the mean vectors between survived birds and perished birds to be different 

#Construct 95% simultaneous confidence intervals for the components of the difference in mean vectors µS − µD using an appropriate multivariate method. Identify which variables show evidence of a difference after accounting for multiple testing.

c_2  = (p*(n_survive + n_perish - 2))/(n_survive + n_perish - p - 1) * qf(0.05, p, n_perish + n_survive-1-p, lower.tail = FALSE)
diff =  x_bar_survived- x_bar_perished

first_interval_lower = diff[1] -  sqrt(c_2 * (((1/n_survive)+ (1/n_perish) )* (S_p[1,1])))
first_interval_upper = diff[1] +  sqrt(c_2* (((1/n_survive)+ (1/n_perish)) * (S_p[1,1])))

second_interval_lower = diff[2] -  sqrt(c_2 * (((1/n_survive)+ (1/n_perish) )* (S_p[2,2])))
second_interval_upper = diff[2] +  sqrt(c_2 * (((1/n_survive)+ (1/n_perish)) * (S_p[2,2])))

third_interval_lower = diff[3] -  sqrt(c_2 * (((1/n_survive)+ (1/n_perish)) * (S_p[3,3])))
third_interval_upper = diff[3] +  sqrt(c_2 * (((1/n_survive)+ (1/n_perish)) * (S_p[3,3])))

fourth_interval_lower = diff[4] -  sqrt(c_2 * (((1/n_survive)+ (1/n_perish)) * (S_p[4,4])))
fourth_interval_upper = diff[4] +  sqrt(c_2 * (((1/n_survive)+ (1/n_perish)) * (S_p[4,4])))

fifth_interval_lower = diff[5] -  sqrt(c_2 * (((1/n_survive)+ (1/n_perish)) * (S_p[5,5])))
fifth_interval_upper = diff[5] +  sqrt(c_2 * (((1/n_survive)+ (1/n_perish) )* (S_p[5,5])))

#Select a pair of morphological variables. Construct a 95% confidence region for the difference in mean vectors. Plot the ellipse and indicate whether the origin lies inside or outside the region. What does this imply?

#ok i select total length  and alar_extend 
pair_idx <- 1:2                 # first two variables in your 3:7 selection
d2  <- diff[pair_idx]           # centre of ellipse (xbar_survive - xbar_perish)
Sp2 <- S_p[pair_idx, pair_idx]  # 2x2 pooled covariance block

n1 <- n_survive
n2 <- n_perish
alpha <- 0.05
p2 <- 2

# constant for ellipse (note p=2 here!)
c <- ((n1+n2-2)*p2)/(n1+n2-1-p2) * qf(1-alpha, p2, n1+n2-1-p2)


A <- (1/n1 + 1/n2) * Sp2 

library(ellipse)
ell <- ellipse(A, centre = d2, t = sqrt(c))

plot(ell, type="l",
     xlab="delta1 = Total_Length (Survive - Perish)",
     ylab="delta2 = Alar_Extent (Survive - Perish)")
abline(h=0, v=0, lty=2)
points(d2[1], d2[2], pch=19)     # estimated difference
points(0, 0, pch=4, cex=1.5)     # origin

# check if origin is inside ellipse
inside <- as.numeric(t(d2) %*% solve(A) %*% d2) <= c
inside
#yes itis inside so no evidence of difference for the pair of length and alar_extend 
#At the 5% level, we do not have evidence of a joint difference in the mean vector (Total_Length, Alar_Extent) between survived and perished birds.






