# Name: Tobias Boggess
# Homework: 1
# Date: January 28, 2022
# Outline: Problems 1-5 from pdf then B.2, B.9, B.4

# Question 1:
pi                      # Output: 3.141593
round(pi)               # Output: 3
round(pi, digits = 4)   # Output: 3.1416
trunc(pi)               # Output: 3
ceiling(pi)             # Output: 4
floor(pi)               # Output: 3

sqrt(16)                # Output: 4
16^0.5                  # Output: 4
4^3                     # Output: 64

log10(1000)             # Output: 3
log(1000)               # Output: 6.907755
log2(64)                # Output: 6
? log                   # Output: 

queue <- c("Steve", "Russell", "Alison", "Liam")
queue
queue <- c(queue, "Barry")
queue
queue <- queue[! queue %in% c("Steve")]
queue
queue <- c("Pam", queue)
queue
queue <- queue[! queue %in% c("Barry")]
queue

w <- 6
x <- 7
y <- 8
z <- 9
ls()
rm(x)
ls()
rm(list = ls())
ls()

x <- c(3, 2, 0, 1, 4, 5, 9, 0, 6, 7, 2, 8)
x == 0
sum(x == 0)
proportion <- any(x == 0) / length(x)
proportion

numVec <- c(2, 4, 6, 5, 9, 8, 2, 4, 7, 8)
charVec <- c("a", "b", "c", "c", "b", "c", "a", "b", "b", "c")
myData <- data.frame(x1 = numVec, x2 = charVec, stringsAsFactors = FALSE)
myData$x1
myData[["x1"]]
myData[[1]]
is.vector(myData$x1)
is.list(myData$x1)
is.data.frame(myData$x1)
is.numeric(myData$x1)
is.character(myData$x1)
myData[2, ]
myData[, 2]
class(myData)
summary(myData)

mylist <- list(x1 = "sally", x2 = 42, x3 = FALSE, x4 = 1:5)
is.list(mylist)
names(mylist)
length(mylist)
mylist[[2]]
mylist[["x1"]]
mylist$x2
length(mylist[["x4"]])
class(mylist)
typeof(mylist)
class(mylist[[4]])
typeof(mylist[[3]])

a <- c(10, 15)
b <- c(TRUE, FALSE)
c <- c("happy", "sad")
data.frame(a, b, c)
class(data.frame(a, b, c))
cbind(a, b)
class(cbind(a, b))
rbind(a, b)
class(rbind(a,b))
cbind(a, b, c)
class(cbind(a, b, c))
list(a, b, c)[[2]]
class(list(a, b, c)[[2]])

mylist <- list(x1 = "sally", x2 = 42, x3 = FALSE, x4 = 1:5)
is.list(mylist)
names(mylist)
length(mylist)
mylist[[2]]
mylist[["x1"]]
mylist$x2
length(mylist[["x4"]])
class(mylist)
typeof(mylist)
class(mylist[[4]])
typeof(mylist[[3]])

