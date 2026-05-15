
## Question 1

# a
set.seed(45)

rolls <- sample(1:6, 6000, replace = TRUE)

table(rolls)

#b

# expectation is the average or mean in statistics

# E(x) = total number of rolls/total number of die face

# E(x) = 6000/6 = 1000



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
mean(sim)



