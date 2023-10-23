#fasteR lesson 16: Writing own functions 
#10/23/23
#mgd=mean of elements greater than d
mgd <- function(x,d) mean(x[x>d])
mgd(Nile, 1200)
mgd(tg$len, 10.2)
class (mgd)
#We can save functions:
save(mgd, file='Mean_greater_than_d')

#function to find range of vector
rng <-function(y) max(y)-min(y)
rng(Nile)
rng



#fasteR lesson 17: "For" loops
#Upload data set 
pima <- read.csv('http://heather.cs.ucdavis.edu/FasteR/data/Pima.csv',header=TRUE)
for (i in 1:9) print (sum(pima[,i]==0))
#i in 1:9 is index range 
colnames(pima)
#Let's recode the columns were 0 don't make sense
#Columns 2-6 don't make sense w/ 0
for (i in 2:6) pima[pima[,i]==0,i] <- NA
#SAME AS:
for (i in 2:6) {
  zeroIndices <- which(pima[,i] == 0)
  pima[zeroIndices,i] <- NA
}

#You can leave loop early 
#Adding cubes of #s and stop once sum exceeds s
f<-function(n,s)
{
  tot<-0
  for (i in 1:n) {
    tot <-tot +i^3
    if (tot>s) {
      print(i)
      break
    }
    if (i==n) print('failed')
    }
}
f(100, 345)
f(5,345)


#FasteR Lesson 18:Functions w/ Blocks
#Transform 0 values to NA in certain columns 
zerosToNAs <-function(d, cols)
{
  for (j in cols) {
    NArows <-which(d[,j]==0)
    d[NArows, j]<- NA
  }
  d
}

#Ex.
d <-data.frame(x=c(1,0,3), y=c(0,0,13))
d
which(d[,2]==0)

#FasteR Lesson 20: If, Else, Ifelse
#If can be used to set condition 
#ifelse uses vectors!
#Ex. Recode Nile to have values
# 1 if <800, 2 if 800 bw 1500 and 3 if >1500
nile <- ifelse(Nile>1150, 3,2)
nile <-ifelse(Nile<800, 1, nile)
table(nile)



#HandsOnR Lesson 11: Loops 
#11.1 Expected value
#EX is weighted average 
die<-c(1,2,3,4,5,6)
die
#11.2 Expand.grid
#Let's you write out every combination 
rolls <- expand.grid(die,die)
rolls
#Lets find sum of 2 rolls
rolls$value <- rolls$Var1 + rolls$Var2
head(rolls,3)
#define prob. of each event 
prob <-c("1"=1/8, "2"=1/8, "3"=1/8, "4"=1/8, "5"=1/8, "6"=3/8)
prob
#Define prob of roll 1
prob[rolls$Var1]
rolls$prob1<-prob[rolls$Var1]
head(rolls,3)
#define prob of roll 2
rolls$prob2<-prob[rolls$Var2]
head(rolls,3)
#To get prob of comb of roll 1/2, multiple prob1xprob2
rolls$prob<-rolls$prob1*rolls$prob2
head(rolls,3)
#Exp. value=value of roll*roll probability
sum(rolls$value*rolls$prob)


#Ex. Slot machine expected value 
wheel <-c("DD", "7", "BBB", "BB", "B", "C", "0")
#Get all combinations of 3 wheel vectors spun 
combos<-expand.grid(wheel, wheel, wheel,  stringAsFactors=FALSE)
combos
#Find prob. of each combination
prob<-c("DD"=.03, "7"=.03, "BBB"=.06, "BB"=.1, "B"=.25, "C"=.01, "0"=.52)
prob
combos$prob1 <-prob[combos$Var1]
combos$prob2 <-prob[combos$Var2]
combos$Prob3 <-prob[combos$Var3]
head(combos, 3)
#Now calculate total prob
combos$prob <- combos$prob1 * combos$prob2 * combos$prob3
#For loops
for (value in c("my", "first", "for", "loop"))
  print(value)
value 
#11.4: While loops
#while reruns code while certain condition is true 
while(condition) {
  code
}

#Plays till broke for slots
play_till_broke <-function(start) {
  cash <-start
  n<-0
  while (cash>0) {
    cash <-cash-1+play()
    n <-n+1
  }
  n
}
play_till_broke(100)

#Repeat loops 11.5 
#repeats code until either stop or break 

