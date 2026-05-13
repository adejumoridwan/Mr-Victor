## Question 1

# 1. Calculate ln(x + y)xy

a1 <- log(x + y) * x * y
a1

a2 <- log10((x*y)/2)
a2

a3 <- (2 * x^(1/3)) + (y^(1/4))
a3

a4 <- (10^(x-y)) + exp(x*y)
a4

## Question 2

x <- c(4, 6, 5, 7, 10, 9, 4, 15)

b1 <- x < 7
b1

b2 <- x == 7
b2

x = 7

b3 <- x
b3

# <=
# >= 
# !=

x <- c(4, 6, 5, 7, 10, 9, 4, 15)

mean(x)

a <- sum(x)
a

b <- length(x)
b

a/b

avg <- sum(x)/length(x)
avg


sub_vector <- x[x > avg]

max_subvector <- max(sub_vector)
max_subvector

# Question 3
A <- matrix(c(1, 3, 2, 5, 4, 3, 4, 4), nrow = 2, ncol = 4)
A
colnames(A) <- c("Col1", "Col2", "Col3", "Col4")
A

B = A + 3
B


A + B

D <- cbind(A, c(1,2))
D

D[ , 3]


# Question 4

LETTERS[18]

length(LETTERS)

LETTERS[length(LETTERS)]

Names <- c("John", "Andrew", "Thomas")
Designation <- c("Manager", "Project Head", "Marketing Head")

cbind(Names, Designation)

cbind(c("John", "Andrew", "Thomas"), 
      c("Manager", "Project Head", "Marketing Head"))


friend <- c("Peter", "John")
city <- c("Abuja", "Warri")

paste(friend,"lives in", city)

a <- "The"
b <- "Quick"
c <- "Brown"
d <- "Fox"

paste(a,b,c,d,"all over the lazy dog")
paste0(a,b,c,d, "all over the laxy dog")

# Question 5

names <- c("Bob", "Alice", "Jane")
age <- c(22, 65, 36)
flag <- c(FALSE, FALSE, TRUE)

df <- data.frame(names, age, flag)
df

## add a column
df$score <- c(50, 60, 90)

df

summary(df)

## add a row
new_name = "Tom"
new_age = 43
new_flag = FALSE
new_score = 67

new_row <- data.frame(names = new_name, 
                      age = new_age,
                      flag = new_flag,
                      score = new_score)

new_row <- data.frame(names = "Tom", 
                      age = 43,
                      flag = FALSE,
                      score = 67)

new_row

new_df <- rbind(df, new_row)
new_df

## Question 6
View(airquality)


airquality$Date_character <- paste(airquality$Day,
                         airquality$Month,
                         "1973",
                         sep = "-")

airquality$Date <- as.Date(paste(airquality$Day,
                                 airquality$Month,
                                 "1973",
                                 sep = "-"),
                           "%d-%m-%Y")

View(airquality)


names(airquality)[4] <- "Temperature"
names(airquality)[2] <- "Solar Radiation"

names(airquality)[names(airquality) == "Ozone"] <- "ozone"

Temperature <- (airquality$Temperature - 32)/1.8



## Question 7

hot <- max(airquality$Temperature)
cold <- min(airquality$Temperature)
hot
cold

which.max(airquality$Temperature)

which.min(airquality$Temperature)

airquality$Date[which.max(airquality$Temperature)]

airquality$Date[which.min(airquality$Temperature)]

airquality$ozone[which.max(airquality$Temperature)]

airquality$Date[which.max(airquality$Wind)]

mean(airquality$Wind)

airquality$Wind > mean(airquality$Wind)

sum(airquality$Wind > mean(airquality$Wind))


