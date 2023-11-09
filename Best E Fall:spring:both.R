
#Isolate spring data 
MyDataUseSpring=MyData[MyData[,"fall"]==0,]
MyDataAggSpring=serialAgg(MyDataUseSpring, AggCats=c("spp", "pop", "yr"))
myDataFilteredSpring=popsFilterSimp(MyDataAggSpring,envtDrivers=EnvtDatColumns)
#For both combined:
MyDataUseBoth=MyData[MyData[,"fall"]>=0,]
MyDataAggBoth=serialAgg(MyDataUseBoth, AggCats=c("spp", "pop", "yr"))
myDataFilteredBoth=popsFilterSimp(MyDataAggBoth,envtDrivers=EnvtDatColumns)


#Arguments 
data=myDataFilteredBoth
speciesID = 131
years = 1977:2008
envtCols = EnvtDatColumns
#E range is 0-6 by steps of 1 
EValues = 1:6

#define function FindBestE
#add season for later as input 
FindBestE = function(data, speciesID, years, envtCols, EValues) {
  #Initialize lowest R2 value possible 
  bestR2 <- 0
  #Initialize best E value as NA for now, so it updates as function runs 
  bestE <- NA
  #For loop will run through the possible E values 
  for (E in EValues) {
    #Make new set for just species # and specified years 
    #Where spp=speciesID set above 
    #Where yr column is in the years range 
    speciesData <- data[data[,"spp"] == speciesID & data[,"yr"] %in% years, ]
    #taken from yesterday's code 
    fiti <- fitGP(data =speciesData, y = "logn", x = c("logn", envtCols), pop="pop", E = E, tau = 1, scaling = "none", predictmethod = "loo")
    #output R2 from fiti
    R2 <- fiti$outsampfitstats[1]
    #Find greatest R2 value
    if (R2 > bestR2) {
      #Make highest R2 replace bestR2 each time it runs as higher 
      bestR2 <- R2
      #Best E will be the E value for that highest R2 
      bestE <- E
    }
  }
  #Output best E and best R2 
  return(list(E = bestE, R2 = bestR2))
}

#See what value was best

#How to "print" E and R2???? 
install.packages("knitr") 
library(knitr) 
suppressWarnings({ 
  # Code that generates warning messages 
}) 
R=suppressWarnings(fitGP)
Result <-FindBestE(data,speciesID, years, envtCols, EValues)
Result

