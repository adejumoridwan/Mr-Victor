## Exercise 1
sal <- read.table("salamanders.txt")
sal
View(sal)

# structures
str(sal)

# number of rows
nrow(sal)

# number of columns/variables
ncol(sal)


colnames(sal) <- sal[1, ]

sal <- sal[-1, ]


View(sal)

## Question 3

#a

std_dev <- sd(sal$CWD)

std_dev

avg <- mean(as.numeric(sal$CWD))
avg

cv <- std_dev/avg *100

cv

iqr <- IQR(sal$CWD)

# Question 4

# ecdf graph
plot(ecdf(as.numeric(sal$CWD)), 
     main = "ECDF of CWD",
     xlab = "CWD",
     ylab = "F(x)")

# histogram
hist(as.numeric(sal$CWD),
     main = "Histogram of CWD",
     xlab = "CWD",
     col = "red")

plot(density(as.numeric(sal$CWD)),
     main = "Kernel Density of CWD",
     xlab = " CWD")

boxplot(sal$CWD,
        main = "Boxplot of CWD",
        ylab = "CWD")

## Question 5

stat_vars <- sal[, c("abund", "fecund", "CWD")]

View(stat_vars)

stat_vars$abund <- as.numeric(stat_vars$abund)

stat_vars$fecund <- as.numeric(stat_vars$fecund)

stat_vars$CWD <- as.numeric(stat_vars$CWD)

# covariance matrix
cov(stat_vars)

# pearson's correlation matrix
cor(stat_vars, method = "pearson")

# spearman correlation matrix
cor(stat_vars, method = "spearman")

# Question 6

hist(as.numeric(sal$CWD))

shapiro.test(as.numeric(sal$CWD))

hist(log(as.numeric(sal$CWD)))

hist(sqrt(as.numeric(sal$CWD)))

shapiro.test(log(as.numeric(sal$CWD)))


shapiro.test(sqrt(as.numeric(sal$CWD)))

## Question 7

sal_scaled <- scale(stat_vars)

sal_scaled

colMeans(sal_scaled)

# No, because the rows represent different observations
# and not different variables. If it were to represent
# different variables, then one can use row standardization





