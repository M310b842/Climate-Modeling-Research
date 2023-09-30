#FasterR Lesson 2: first R steps 
#Add two numbers together and get output 
1+1
#Given a set of data, Nile, find the average of that data set 
mean(Nile)
#Print the entire set of data under Nile 
Nile
#Find the length of a given data set (will output number of elements)
length (Nile)
#Print a histogram of the given Nile data set
hist(Nile)
#specify number of bins for the histogram by creating # of breaks
hist(Nile, breaks=20)
hist(Nile, breaks=5)
hist(Nile, breaks=50)
hist(Nile, breaks=12)
#Get help on what a certain function does/can do
?hist
?mean


#FasterR Lesson #: Vectors and indices 
#To isolate parts of the data set, break it into subscripts via brackets
Nile[2]
#The above [2] gives us the 2nd data point in the set Nile
#To isolate multiple data points concatenate the data using c()
Nile[c(2,5,6)]
#The above gives us the 2, 5, and 6 data points
#To get consecutive data poins, use the : 
Nile [2:4]
#The above gives us the values for data points 2, 3, 4
Nile[81:100]
#Suppose we want mean of a certain range of the the data
mean(Nile[81:100])
#To store this under a certain name (n81100) use <-
n81100 <- Nile[81:100]
#Now use that given name to find the mean of the data stored under that name
mean(n81100)
#find standard deviation of that data 
sd(n81100)
#Find mean of years 1945-1960
Nile [74:89]
#Define this data set under new name
n7489 <-Nile[74:89]
mean(n7489)
q()





#Hands-On-R Lesson 1: Weighted Dice
#What happens if you have an incomplete code:
5-
#R gives you a plus meaning code needs more
#what about errors?
3%5
#R gives an error message 
#basic math with R
#Multiply 2 numbers
2*3
4-1
6/(4-1)


#2.1 Exercise 
10+2
12*3
36-6
30/3

#2.2 Objects
1:6
die <- 1:6
die
#See what names I have already used ls()
ls()
#Let's manipulate the die 
#subtracts 1 from every value of die 
die-1
#divide each value of die by 2
die/2
#multiply each value of die by itself
die*die
#Lets see how R does vector operations
1:2
1:4
die
#Take die data set and add 1 to 1st value then 2 to 2nd then 1 to 3rd and so on
die + 1:2
die + 1:4
#This above input gives warning since lengths of vectors don't match
#%*% does inner multiplication
die %*% die
#%o% does outer multiplication
die %o% die 

#R uses functions 
#round a number
round(3.434353)
#Take factorial of a number
factorial(3)
#Take mean of data range 1 to 6
mean(1:6)
mean(die)
round(mean(die))


#To simulate rolling a die use sample(x=range to sample from, size= size of sample)
sample (x=die, size=1)
#Can also write as
sample (die, size=1)
#Can switch as well, as long as you define the argument name
sample (size=1, x=die)
#To look up which names to use for a function use args
args(round)
#Round to certain number of decimals using round(#, digits=)
round(3.13134, digits=2)


#2.3.1 Sample w/ replacement
#Let's simulate throwing 2 die
sample(die, size=2)
#However, this doesn't replace the 1st value so not fully accurate
#To address replacement issue, use replace=TRUE
sample(die, size=2, replace=TRUE)
#^This creates independent random sample!
#Lets store this ID random sample for 2 dice throw
dice <- sample(die, size=2, replace=TRUE)
dice
#This gives same value of dice each time since it is not rerolling
sum(dice)
dice
dice



#2.4 Writing your own functions 
#We want to "roll" the dice 
#Create a function using function () then brackets {}
roll <- function () {
  die <- 1:6
  dice <- sample (die, size=2, replace=TRUE)
  sum(dice)
  }
roll()
roll()




#Lesson 2.5 Arguments 
#Define new function
roll2 <- function (bones) {
  dice <- sample(bones, size=2, replace=TRUE)
  sum(dice)
}
#To get roll2 you need to define function for bones in the function input
roll2(bones=1:4)
#This takes the roll2 function uses range 1:4 and sums the output 
#However, error occurs if you don't define bones


#To avoid having to put in bones value, let's set a default 
roll2 <- function(bones=1:6) {
    dice <- sample(bones, size=2, replace=TRUE)
    sum (dice)}

roll2()

roll2()
roll2()




#Hands-

