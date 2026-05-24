

### Efficiency

## Question 1
set.seed(43)

rbeta(40, shape1 = 2, shape2 = 0.7)

samples <- replicate(1000, rbeta(40, shape1 = 2, shape2 = 0.7))

samples

## Question 2

add <- function(a, b){
  print(a + b)
}

add(2, 2)

## installing
install.packages("fitdistrplus")

## load package
library(fitdistrplus)

estbeta.ml<-function(y)
{
  coefficients(fitdist(y,distr="beta"))
}

ml_estimates <- apply(samples, 2, estbeta.ml)

ml_estimates <- t(apply(samples, 2, estbeta.ml))

par(mfrow = c(1,2))

hist(ml_estimates[,1], prob = TRUE)

lines(density(ml_estimates[,1]))

hist(ml_estimates[,2], prob = TRUE)

lines(density(ml_estimates[,2]))

## Question 4

# alpha
sd(ml_estimates[,1])

# beta
sd(ml_estimates[,2])

## Question 5

estbeta.mm <- function(y){
  m <- mean(y)
  v <- var(y)
  alpha_mm <- m * ((m * (1 - m)/v)-1)
  beta_mm <- (1 - m)*(((m * (1 - m))/v)-1)
  c(shape1 = alpha_mm, shape2 = beta_mm)
}

mm_estimates <- apply(samples, 2, estbeta.mm)

mm_estimates <- t(apply(samples, 2, estbeta.mm))


se_mm_alpha <- sd(mm_estimates[,1])
se_mm_beta <- sd(mm_estimates[,2])

se_mm_alpha
se_mm_beta

## The TcCB Data

## Question 1

EPA.94b.tccb.df <- read.csv("EPA.94b.tccb.df.txt")

## Question 2

cleanup <- subset(EPA.94b.tccb.df, Area == "Cleanup")

cleanup <- cleanup$TcCB

## Question 3

bc <- boxcox(cleanup ~ 1, lambda = seq(-2,2,by = 0.01))

which.max(bc$y)

lambda_opt <- bc$x[which.max(bc$y)]

cleanup_transformed <- (cleanup^lambda_opt - 1)/lambda_opt


# plot a density for the above histogram
plot(density(cleanup_transformed), 
     col = "blue", 
     main = "Density of Transformed Cleanup Data")

plot(density(cleanup), 
     col = "blue", 
     main = "Density of Transformed Cleanup Data")

## Question 4

library(fitdistrplus)

fit <- fitdist(cleanup_transformed, "norm")
fit

mean_estimate <- -0.9499
sd_estimate <- 1.3837

## Question 5

q99_transformed <- qnorm(0.99, 
                         mean = mean_estimate,
                         sd = sd_estimate)
q99_transformed

## Question 6

a <- exp(q99_transformed)
a

## Question 7

p = 0.01

pbinom(0, size = 3, prob = 0.01, lower.tail = FALSE)

