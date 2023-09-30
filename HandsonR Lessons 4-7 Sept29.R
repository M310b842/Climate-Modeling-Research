#HandsonR-Lesson 4

#Ask R what type of object an object is using "typeof"
typeof(die)
#To store integer in R type a number followed by L 
int <- c(-1L, 2L, 4L)
int
typeof(int)

#Characters:stores pieces of text
#Store character using quotes 
text <- c("Hello", "World")
text
typeof (text)
typeof("Hello")
#Logicals: testing true or false statements 
3>4
logic <- c(TRUE, FALSE, TRUE)
logic
typeof(logic)
typeof(F)

#R can also utlizile complex numbers using i
comp <-c(1 +1i, 1+2i, 1+3i )
comp
typeof(comp)


#Making a vector of cards
hand <- c("ace", "king", "queen", "jack", "ten")
hand
typeof(hand)

#Attributes and names
#Look up the names of a stored vector
names(die)
#To name a stored vector use names(vector) <-
names(die) <- c("one", "two", "three", "four", "five", "six")
names(die)
#To check attributes of our stored data use attributes(vector)
attributes(die)
die
#Names don't change the value of the stored data
die+1
#to remove names use names(vector)<- NULL
names(die) <- NULL
names(die)
die

#Dimensions
#To change dimension use dim(data) <- c(?rows,?columns)
dim(die) <- c(2,3)
die
die
dim(die) <- c(3,2)
die
#We can also add "slices" using c(row, column, slice)
dim(die) <-c(1,2,3)
die


#R can also use matrices 
#use matrix(data, nrow=?) or ncol in place of row()
m <- matrix(die, nrow=2) 
m
#we can also fill up the matrix by row
#use byrow=TRUE
m <-matrix(die, nrow=2, byrow=TRUE)
m

#Arrays 
#use array(set atomic vector, se vector dimensions)
ar <-array(c(11:14, 21:24, 31:34), dim=c(2,2,3))
ar


#Exercise 5.3-Matrix with suit of royal flush
#Lets create hand for suits, and cards 
hand1 <-c("ace", "king", "queen", "jack", "ten", "spades", "spades", "spades", "spades", "spades")
#then set matrix to be hand1, with 2 columns
matrix(hand1, ncol=2)
#alternatively, if we define hand differently:
hand2 <- c("ace", "spades", "king", "spades", "queen", "spades", "jack", "spades", "ten", "spades")
#Then we need to specify byrow=TRUE
matrix(hand2, ncol=2, byrow=TRUE)


#5.5 Class
#changing dimensions changes class not type 
dim(die) <-c(2,3)
typeof(die)
class(die)

attributes(die)
class("Hello")
class(5)

#5.5 Dates and times
#time has 2 classes" POSIXct and POSIXt
now <- Sys.time ()
now
typeof(now)
class(now)
unclass(now)

#we can use POSIXct to find time from various points
#Ex. whay day is million seconds after Jan 1, 1970?
mil <- 1000000
mil
class(mil) <-c("POSIXct", "POSIXt")
mil



#Factors is an important type of class
#can be used for categorical data 
gender <- factor(c("male", "female", "female", "male"))
#This stores data as integers
typeof(gender)
attributes(gender)

#covert factor to character string with as.character
as.character(gender)

#EX. 5.4 playing cards with points
#combine ace and heart and 1 into a vector 
card <- c("ace", "heart", 1)
#vectors can only store 1 type of data, so 1 is classified as character!
typeof(card)


#5.6 Coercion
#R coerces data 
#if character present, everything else becomes a character
#if only logicals/numbers present, R converts logicals to numbers 
#True becomes 1, FALSE becomes 0
sum(c(TRUE, TRUE, FALSE, FALSE))
#SAME AS:
sum (c(1,1,0,0))
#we can convert if it makes sense
as.character(1)
as.logical(1)
as.numeric(FALSE)


#5.7 LISTS
#Group data in 1D set 
list1 <- list(100:130, "R", list(TRUE, FALSE))
list1

#Ex. playing card-storing ace of hearts worth 1 pt
card <- list("ace", "hearts", 1)
card


# 5.8 DATA FRAMES 
#creates 2D lists 
df <- data.frame(face=c("ace", "two", "six"),
  suit=c("clubs", "clubs","clubs"), value=c(1,2,3))
df
#All columns need to be same length!  
#to see what types of objects are in the list or data frame, use str
typeof(dfx)
typeof(df)
class(df)
str(df)


#To save data file 
write.csv(deck, file="cards.csv", row.names=FALSE)

getwd()








#HandsOnR-Lesson 6
#we want to extract value from a data frame deck:
#deck [i,j], i tells extraction from row, j tells extraction from column

#use head to display first n rows in the data frame
head(deck)

deal(deck)

#return value in first row and column spot
deck[1,1]

#to return multiple values: deck[i, vector]
#ex. to get full row 1 (all columns)
deck[1, c(1,2,3)]
#we can save what we just extracted using:
new <- deck[1, c(1:3)]
new

#You can use this technique for things other than data frames
vec <-c(6,1,3,6,10,5)
vec[1:3]


#6.1.2 Negative integers
#do opposite of positive
#return all objects EXCEPT the negative integer given
deck[-(2:52), 1:3]
#^this excludes rows 2:52, so returns 1st row 
#You cannot combine negative and positive integers  in same index 
#You can combine if in different indexes
deck [-1,1]
#^this returned everything but row 1, and returned everything in column 1 

#using blank space in index extracts every value in that dimension
deck [1, ]



#Exercise 6.1-Dealing a card
> #code to return first row of a data frame:
deal <-function(cards) {
    cards[1,]
  }
deal(deck)


#However, we need to shuffle this deck! 
deck2 <-deck[1:52, ]
head(deck2)

deck3 <-deck[c(2,1,3:52),]
head(deck3)
#however, we want to randomly return the rows, so use sample
random <- sample(1:52, size=52)
random
deck4 <-deck[random, ]
head(deck4)

#EXERCISE 6.2-shuffle a deck 
#let's make function to shuffle data frame adn return a copy of the shuffled data
shuffle <- function(cards) {
  random <-sample(1:52, size=52)
  cards[random, ]
}
#Now deal that deck
deal(deck)
#Setting deck 2 as a shuffled deck
deck2 <-shuffle(deck)
#deal that new shuffled deck
deal(deck2)
deal(deck)


#troubleshooting because I didn't define function corerctly earlier
deal <- function(cards) {
  cards[1, ]
}

#Still troubleshooting but I got it now
shuffle <- function(cards) {
  random <-sample(1:52, size=52)
  cards [random, ]
}
deal(deck)

#Testing new troubleshooting function definition
deck2 <-shuffle(deck)
deal(deck2)


#6.4 Dollar signs and double brackets 
#We can use $ to extract data
#deck$value extracts the colume name value from the data deck
deck$value
#this gives output as a vector for that columns data
#Hence, we can now take mean or median of that vector
mean(deck$value)
median(deck$value)

#can also use this $ for lists
lst <-list(numbers=c(1,2), logical=TRUE, strings=c("a", "b", "c"))
lst
#subset this lst
lst[1]
#however, right now we can't take sum of this since it isn't a vector
#so lets convert to vector using $
lst$numbers
sum(lst$numbers)
#You can also use lst[[]] to avoid using $ and return just the values inside the element of the list
lst[[1]]
#see the difference b/w lst[] and lst[[]]
lst["numbers"]
lst[["numbers"]]
