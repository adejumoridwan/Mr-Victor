
## Question 1

# a
set.seed(45)

rolls <- sample(1:6, 6000, replace = TRUE)

table(rolls)

#b

# expectation is the average or mean in statistics

# E(x) = total number of rolls/total number of die face

# E(x) = 6000/6 = 1000

# The expected value is 1000 which is close to what we
# have on the frequency of the 6000 simulated dice rolls



# c
# x > 1030
# n = 6000
# p(1) = 1/6

p_more_1030 <- pbinom(1030,
                      size = 6000,
                      prob = 1/6,
                      lower.tail = FALSE)
p_more_1030

set.seed(45)

sim <- replicate(10000,sum(sample(1:6, 6000, replace = TRUE) == 1) > 1030)

sum(sim)/10000

set.seed(1)
sum(sample(1:6, 6000, replace = TRUE) == 1)

set.seed(2)
sum(sample(1:6, 6000, replace = TRUE) == 1)

## Wood frog larval abundance in vernal pools

p = 0.3
n = 10

# a 
dbinom(6, size = 10, prob = 0.3)

#b
pbinom(2, size = 10, prob = 0.3)

#c
pbinom(4, size = 10, lower.tail = FALSE, prob = 0.3)

#d
x <- 0:10

plot(x, dbinom(x, size = 10, prob = 0.3), 
     type = "b", col = "red", 
     xlab = "No of sweeps with tadpoles",
     ylab = "Probability",
     main = "Binomial PMF: p=0.3 vs p=0.5")

lines(x, dbinom(x, size = 10, prob = 0.5), 
      type = "b", col = "blue")

legend("topright", legend = c("p=0.3","p=0.5"), 
       col = c("red","blue"), lty = 1)


## Gamma Distribution


sample_a <- rgamma(10, shape = 3, scale = 2)

sample_a

hist(sample_a)

sample_b <- rgamma(20, shape = 3, scale = 2)

sample_b

hist(sample_b)


sample_c <- rgamma(100, shape = 3, scale = 2)

sample_c

hist(sample_c)

sample_d <- rgamma(1000, shape = 3, scale = 2)

sample_d

hist(sample_d)

lines(0:999, rgamma(1000, shape = 3, scale = 2))

## Arsenic

# Expectation in statistics means "mean"
# In the normal distribution the mean = median = mode

mean <- 5
s_d <- 1

# X ~ N(mu, sigma^2)

# SE = sigma^2/n

# X ~ N(5, 1/4)

# X ~ N(5, 0.25)

#b

pnorm(7, mean = 5, sd = 1, 
      lower.tail = FALSE)

pnorm(7, mean = 5, sd = sqrt(0.25), 
      lower.tail = FALSE)


plot(0:10, pnorm(0:10, mean = 5, sd = 1, 
                 lower.tail = FALSE),
     col = "blue", type = "l")

lines(0:10, pnorm(0:10, mean = 5, sd = 0.5, 
                  lower.tail = FALSE),
      col = "red")

## Groundwater


mean <- 20
s_d <- 5

pnorm(30, mean = 20, sd = 1, lower.tail = FALSE)

#a
## Regulation

n <- 365
p <- 1/365


# For binomial distribution: Mean = np, SD = np(1-p)
expectation <- n*p
expectation

#b
# Yes the distribution of emissions satisfy the regulation?

#c
x <- 2
n <- 365
p <- 1/365

pbinom(1, size = n, prob = p, lower.tail = F)


## Poisson Distribution

# The only parameter of a poisson distribution "lambda"
# lambda = np

lambda <- 1


ppois(1, lambda = 1, lower.tail = FALSE)

## Uniform and Beta Distribution

plot(0:1, dbeta(0:1,shape1 = 1, shape2 = 1), 
     type = "l", col = "blue")


lines(0:1, dunif(0:1), col = "red")

## Exponential Distribution


# a
# P(Y = 0.5) = 0

#b
pexp(1, rate = 2, lower.tail = FALSE)

#c

# Undefined because -1 is less than our range which is [0,infiniti]

#d

# Undefined because 0.2 is less than 0.7

#c
pexp(0.6, rate = 2) - pexp(0.1, rate = 2)


## Species Dispersal

dgeom(2, prob = 0.17)


## Glaucouse winged gull


dnbinom(1, size = 3, prob = 0.76)






