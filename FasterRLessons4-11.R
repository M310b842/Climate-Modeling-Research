#FasteR: Lesson 4: More on Vectors

#sum function sums all the values together
sum(c(5,12,13))
#If we want number of Nile years where level is greater than 1200 we can sum it
sum(Nile>1200)
sum(Nile>1200)
#^This sums the TRUE and FALSE as values of 0 and 1 summed together=7

#What if we want to know which years were level above 1200
#use which function, returns exact years this occured
which(Nile>1200)
#or alternatively
which1200 <- which(Nile>1200)
which1200
length(which1200)
#What if we want to display the years of which1200
Nile[which1200]
#or alternatively
Nile[Nile>1200]

#Negative indices indicate you want all values EXCEPT was is indicated
x <- c(5,12,13,8)
#This asks for all values in x except x[1]
x[-1]
#This asks for all values except x in 1st and 4th position
x[c(-1,-4)]


#Lesson 5: On to Data Frames!

#?ToothGrowth is built in dataset in R
?ToothGrowth
#to see first 6 lines of data
> head(ToothGrowth)
tg <-ToothGrowth 
#To denote a single colum use $
#This takes mean of the length column of tg 
mean(tg$len)
#to specify row or column use [ith row, jth column]
tg[3,1]
#^This gets element in 3rd row and 1st column
#Alternatively, we can use:
tg$len[3]
#What if no column name exists? Use column number
mean(tg[,1])
#^This takes mean of all of column 1 since no row is specified 

#To take subset of data fram tg we can:
z<-tg[2:5, c(1,3)]
#^This takes rows 2-5 and columns 1 and 3
z
#What if we want all of columns 1 and 3:
y <- tg[,c(1,3)]
y
#^This leaves row empty so all rows of columns 1 and 3 are taken

#To learn length of rows (number of rows) use nrow
nrow(tg)

#Use head function to show first 6 values of specified colum
head(tg$len)
#^this gives first 6 values of column length 
#to specify length of head use (column name, #)
head(tg$len, 10)
#^This prints column length's first 10 values

#To create a data frame from scratch:
#define values x y 
#use d <- data.frame(x,y) to store it as dataframe
x <- c(5,12,13)
y <- c('abc', 'de', 'z')
d <- data.frame(x,y)
d
#COLUMNS MUST HAVE SAME LENGTH!

#Negative indices still work the same way
#Example: this returns all columns EXCEPT column 2
z <-tg[,-2]
head(z)



#Lesson 6: R Factor Class
#Objects can be class: numeric, character, etc.
class(tg)
class(tg$supp)
#Levels can tell us list of categories under that variable (supp)
levels(tg$supp)


#Lesson 7: Extracting Rows/Columns from Data Frames
#ToothGrowth data:

#this tells us which supplements are OJ
whichOJ <- which(tg$supp == 'OJ')
#this tells us which supplements are VC
whichVC <-which(tg$supp == 'VC')
#this takes mean of tg values for which supp=OJ, in the first column (length)
mean (tg[whichOJ, 1])
#this takes mean of tg values for which supp=VC and are in column 1 (length)
mean (tg[whichVC, 1])

#OR alternatively this could be coded as:
#This defines tgoj as the values of tg for which supp is OJ for all columns
tgoj <- tg[tg$supp == 'OJ',]
#This defines tgvc as the values of tg for which supp=VC and for all columns
tgvc <- tg[tg$supp== 'VC',]
#this takes the mean of the tg OJ values in the length column 
mean (tgoj$len)
#This takes mean of the tg VC values in the length column
mean(tgvc$len)



#Lesson 8: Extracting Rows, Columns cont. 
#use & symbol to extract on more than one condition
#EX using ToothGrowth data

#This takes tg values for supp column=OJ and length <8.8
tg[tg$supp =='OJ' & tg$len<8.8,]

#To take rows that satifsy AT LEAST 1 of those conditions use |
#this returns tg values for rows with len>28 or with dose=1.0
tg[tg$len >28 | tg$dose ==1.0,]

#to save this data use:
w <- tg[tg$len >28 | tg$dose==1.0,]
#then you can call on this data anytime w/out having to write the full definition
w
head(w)
#To see # of cases that satisfy those conditions use:
nrow(w)

#to extract only certain columns from the data frame:
#define lendose to be the values of tg from all rows and only columns 1 and 3
lendose <- tg[, c(1,3)]
head(lendose)
#Alternatively:
lendose <- tg[,c('len', 'dose')]
head(lendose)



#Lesson 11: The R List Class
#use built in data set mtcars 

#the following takes the colum mpg from data set mtcars
mtmpg <- mtcars$mpg 
#to split this data into vectors for 4, 6, and 8 cyclinders use:
mt4 <-mtmpg[mtcars$cyl ==4]
mt6 <-mtmpg[mtcars$cyl ==6]
mt8 <-mtmpg[mtcars$cyl==8]

#Or alternatively using split function
#This defines mtl as:
#mtmpg split into coressponding values in mtcars$cyl
mtl <- split(mtmpg, mtcars$cyl)
mtl
#Now to assess each value of mtl independently use
mtl$'4'
#or alternatively
mtl[[1]]

#this gives first 6 values of mt cars in cycl column
head(mtcars$cyl)

#lists can be used in other ways
l <-list(a=c(2,5), b='sky')
#this returns each component of list (a,b)
l

#we can also rename the list elements for 4,6,8 cyclinders
names(mtl) <-c('four', 'six', 'eight')
mtl

#this gives us the 2nd row, 3rd column value
#third car in the 6 cylinder category
mtl[[2]][3]
