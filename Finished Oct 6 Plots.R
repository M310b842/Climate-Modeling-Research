library(readr)
FULLDATA<- read.csv("Downloads/NOAA_EcosystemData_10-6-2023.csv", header=TRUE)
View(FULLDATA)

serialAgg=function (x, AggCats, AggTarg = NULL, FUN = function(x) mean(x, na.rm = TRUE)){
  if (is.null(AggTarg)) {
    if (is.numeric(AggCats)) 
      AggTarg = (1:ncol(x))[-AggCats]
    if (is.character(AggCats)) 
      AggTarg = colnames(x)[!colnames(x) %in% AggCats]
  }
  Numbers = prod(apply(t(t(x[, AggCats])), 2, is.numeric))
  ncat = length(AggCats)
  if (ncat == 1) 
    Cats = as.character(x[, AggCats])
  else Cats = codify(x[, AggCats])
  agged = as.matrix(aggregate(x[, AggTarg], by = list(Cats), FUN = FUN))
  if (ncat > 1) 
    agged = cbind(matrix(unlist(strsplit(agged[, 1], 
                                         "_")), ncol = ncat, byrow = TRUE), matrix(agged[, 
                                                                                         -1], ncol = ncol(agged) - 1))
  if (Numbers) 
    agged = t(apply(agged, 1, as.numeric))
  colnames(agged) = colnames(cbind(x[, c(AggCats[1], AggCats)], 
                                   x[, c(AggTarg, AggTarg[1])]))[c(1, 3:(ncol(agged) + 1))]
  agged
}
codify=function (x, cols = 1:ncol(x), sep = "_") 
  as.matrix(cbind(Index = apply(x[, cols], 1, paste0, collapse = sep),  x[, -cols]))





#Activity 4
#Species: Spiny Dogfish
SDF <- subset(FULLDATA, spnm=="SPINY DOGFISH")
SDFAG <- serialAgg(FULLDATA, AggCats=c("GMT_YEAR"), AggTarg =c("EXPCATCHWT", "EXPCATCHNUM"), FUN = function(x) mean(x, na.rm = TRUE))
#plot of species biomass over time 
plot(SDFAG, col=2, type="l", cex=0.6, main="Spiny Dogfish Biomass over Time")
#plot of species density over time
plot(SDFAG[,c("GMT_YEAR", "EXPCATCHNUM")], col=2, type="l", cex=0.6, main="Spiny Dogfish Density over Time")





#Activity 5
#Species biomass by nearest degree of latitude
latr <-round(SDF$DECDEG_BEGLAT)
SDF$latr=round(SDF$DECDEG_BEGLAT)
?unique
#how many different latr we have:
unique(latr)
#Aggregate biomass by year and latr
YrSpaceWt <- serialAgg(SDF,AggCats=c("GMT_YEAR","latr"), AggTarg =c("EXPCATCHWT"), FUN = function(x) mean(x, na.rm = TRUE))


#Plot for 40
LATR40 <-(YrSpaceWt[YrSpaceWt[,"latr"]==40,])
plot(LATR40[,"GMT_YEAR"], LATR40[,"EXPCATCHWT"], type="l", col=1, main="Spiny Dogfish \n Biomass by Latitude 40", xlab="Year", ylab="Biomass")
#Plot for 41
LATR41 <-(YrSpaceWt[YrSpaceWt[,"latr"]==41,])
plot(LATR41[,"GMT_YEAR"], LATR41[,"EXPCATCHWT"], type="l", col=2, main="Spiny Dogfish \n Biomass by Latitude 41", xlab="Year", ylab="Biomass")
#Plot for 42
LATR42 <-(YrSpaceWt[YrSpaceWt[,"latr"]==42,])
plot(LATR42[,"GMT_YEAR"], LATR42[,"EXPCATCHWT"], type="l", col=3, main="Spiny Dogfish \n Biomass by Latitude 42", xlab="Year", ylab="Biomass")
#Plot for 43
LATR43 <-(YrSpaceWt[YrSpaceWt[,"latr"]==43,])
plot(LATR43[,"GMT_YEAR"], LATR43[,"EXPCATCHWT"], type="l", col=4, main="Spiny Dogfish \n Biomass by Latitude 43", xlab="Year", ylab="Biomass")
#Plot for 44
LATR44 <-(YrSpaceWt[YrSpaceWt[,"latr"]==44,])
plot(LATR44[,"GMT_YEAR"], LATR44[,"EXPCATCHWT"], type="l", col=5, main="Spiny Dogfish \n Biomass by Latitude 44", xlab="Year", ylab="Biomass")
#Plot for 39
LATR39 <-(YrSpaceWt[YrSpaceWt[,"latr"]==39,])
plot(LATR39[,"GMT_YEAR"], LATR39[,"EXPCATCHWT"], type="l", col=6, main="Spiny Dogfish \n Biomass by Latitude 39", xlab="Year", ylab="Biomass")
#Plot for 36
LATR36 <-(YrSpaceWt[YrSpaceWt[,"latr"]==36,])
plot(LATR36[,"GMT_YEAR"], LATR36[,"EXPCATCHWT"], type="l", col=7, main="Spiny Dogfish \n Biomass by Latitude 36", xlab="Year", ylab="Biomass")
#Plot for 37
LATR37 <-(YrSpaceWt[YrSpaceWt[,"latr"]==37,])
plot(LATR37[,"GMT_YEAR"], LATR37[,"EXPCATCHWT"], type="l", col=8, main="Spiny Dogfish \n Biomass by Latitude 37", xlab="Year", ylab="Biomass")
#Plot for 38
LATR38 <-(YrSpaceWt[YrSpaceWt[,"latr"]==38,])
plot(LATR38[,"GMT_YEAR"], LATR38[,"EXPCATCHWT"], type="l", col=9, main="Spiny Dogfish \n Biomass by Latitude 38", xlab="Year", ylab="Biomass")
#Plot for 35
LATR35 <-(YrSpaceWt[YrSpaceWt[,"latr"]==35,])
plot(LATR35[,"GMT_YEAR"], LATR35[,"EXPCATCHWT"], type="l", col=10, main="Spiny Dogfish \n Biomass by Latitude 35", xlab="Year", ylab="Biomass")





#ACTIVITY 6
par(mfrow=c(3,3))
hist(SDF$GMT_YEAR, xlab="year", ylab="Frequency", main="Year")
hist(SDF$fall, xlab="fall", ylab="Frequency", main="fall")
hist(SDF$STRATUM, xlab="stratum", ylab="Frequency", main="Stratum")
hist(SDF$SVSPP, xlab="SVSPP", ylab="Frequency", main="SVSPP")
hist(SDF$doy,xlab="doy", ylab="Frequency", main="doy")
hist(SDF$DECDEG_BEGLAT, xlab="Lat", ylab="Frequency", main="Lat")
hist(SDF$DECDEG_BEGLON,xlab="Long.", ylab="Frequency", main="Long")
hist(SDF$EXPCATCHWT, xlab="Exp Catch Wt", ylab="Frequency", main="Exp Catch Wt")

par(mfrow=c(2,2))
plot(x=SDF$GMT_YEAR,y=SDF$doy, xlab="Year", ylab="doy", main="Year vs. doy", type='l', col=2)
plot(x=SDF$GMT_YEAR,y=SDF$AVGDEPTH, xlab="Year", ylab="Average Depth", main="Year vs. Average Depth", type='l', col=3)
plot(x=SDF$GMT_YEAR,y=SDF$SVVESSEL, xlab="Year", ylab="SVVESSEL", main="Year vs. SVVESSEL", type='l', col=4)
plot(x=SDF$GMT_YEAR,y=SDF$TOWDUR, xlab="Year", ylab="TOWDUR", main="Year vs. TOWDUR", type='l', col=5)

     