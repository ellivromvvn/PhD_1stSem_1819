# set the working directory
setwd("~/R/PhD_1stSem_1819")
getwd() #print working directory


#HELP
?setwd
?getwd

#Scalars
#numeric
x <- 143 #assign 143 to x
x #print the value of x
y <- 198
y
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


