
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




