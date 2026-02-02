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
t_critical =( n_perish + n_survive - 2 * p) / (n_perish + n_survive - 1 - p) * qt(0.05, n_survive + n_perish - 1-p, lower.tail = FALSE)

t_critical

#t_stat > t_crit therefore reject null hypothesis meaning that there is significant stats to suggest the mean vectors between survived birds and perished birds to be different 

#Construct 95% simultaneous confidence intervals for the components of the difference in mean vectors µS − µD using an appropriate multivariate method. Identify which variables show evidence of a difference after accounting for multiple testing.



