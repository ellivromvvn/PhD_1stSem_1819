# set the working directory
setwd("~/R/PhD_1stSem_1819")
getwd() #print working directory


#HELP
?setwd
?getwd

#Basic Computations
5+5 #addition
5-3 #subtraction
5*3 #multiplication
20/2 #division
2+6/4 #use parenthesis to quantify operations
(2+6)/4
2^2 #exponent
2^2+1 #using parenthesis
2^(2+1)
sqrt(4) #square root
8^(1/3) #cube root
x <- c(1, 2, 3)
x^2
xsquared <- x^2

#Scalars
#numeric
x <- 143 #assign 143 to x
x #print the value of x
y <- 198
y
r <- 7
x + y #print the sum of  x and y
z <- x +y #store the sum of x and y to z
class(x) #what is x?
#character
a <- "male"
class(a)
#logical
b <- TRUE
class(b)

#Vectors
#using the function c() to store values
x <- c(1, 4, 3, 4, 4) 
class(x)
#create character and logical vectors.
#character
luv1s <- c("yenyen","star","uwa")
#logical
A <- c(T, F, T)


#removing objects from the environment
ls() #show all objects in the environment
rm(x) #remove x from the environment
rm(a, b)
rm(list=ls()) #remove all objects in the environment

#writing functions
#a function for getting the average of a numeric vector
average <- function(x){
  sum(x)/length(x) #length() gets the number of elements in x
}
average #show the code for function average()
y <- c(4, 3, 5, 1, 7)
average(y) #testing
#another code for the average()
average <- function(x){
  a <- sum(x)
  b <- length(x)
  c <- a/b
  return(c) #print the value of c
}
average
average(y)
mean(y)

#Finding a Confidence Interval for a Population Mean (LS)
#Example 1.
#We create a function to automate the process
CI.pop.mean.ls <- function(x, c){
  n <- length(x) 
  xbar <- mean(x)
  s <- sd(x)
  zsubc <- qnorm((1 + c)/2) #z scores
  E <- zsubc * s/sqrt(n) #margin of error
  CI <- c(xbar - E, xbar + E) #confidence interval
  return(round(CI, digits = 2)) #print confidence interval
}
# entering n
CI.pop.mean.ls <- function(x, c, n){
  xbar <- mean(x)
  s <- sd(x)
  zsubc <- qnorm((1 + c)/2) #z scores
  E <- zsubc * s/sqrt(n) #margin of error
  CI <- c(xbar - E, xbar + E) #confidence interval
  return(round(CI, digits = 2)) #print confidence interval
}

CI.pop.mean.ls(x=x,c = 0.95, n=32)

#testing
#storing the data
x<-c(183, 183, 197, 209, 209, 212, 212, 212, 213, 213, 213,
     213, 213, 213, 213, 216, 216, 222, 222, 222, 222, 223,
     226, 226, 228, 228, 228, 228, 228, 229, 238, 238)
CI.pop.mean.ls(x, 0.95)
#Example 2.


#Confidence Intervals for the Mean (Small Samples)
#Constructing a Confidence Interval for the Mean:t-Distribution


#Confidence Intervals for Population Proportions
#Constructing a Confidence Interval for a Population Proportion


#Finding a Minimum Sample Size to Estimate p


#Confidence Intervals for Variance and Standard Deviation 
#Constructing a Confidence Interval for a Variance and Standard Deviation


