#AMERICAN SHAD

LHS <-subset(FULLDATA,spnm=="AMERICAN SHAD")
head(LHS)
#Set to work with log(EXPCATCHWT)
logEXPCATCHWT <- log(1+LHS[,"EXPCATCHWT"])
logLHS <-cbind(LHS, logEXPCATCHWT)
#Specify working with Fall
FallLHS <- subset(logLHS, fall==1)


#USE AGGREGATED DATA
SPECAGG <-serialAgg(FallLHS, AggCats=c("GMT_YEAR", "DECDEG_BEGLAT"), AggTarg="logEXPCATCHWT")
#Round latitudes to nearest whole number 
SPECAGGlatrROUND <-round(SPECAGG[,"DECDEG_BEGLAT"])
#Specify latitude 
LHSlatr <- SPECAGG[,"DECDEG_BEGLAT"]
unique(round(LHSlatr))

#Round the latitudes of aggregatd species data
SPECAGG[,"DECDEG_BEGLAT"] <- round(SPECAGG[,"DECDEG_BEGLAT"])

#Automate setting latr= what you need 
LATR <- (SPECAGG[SPECAGG[,"DECDEG_BEGLAT"]==44,])
#plot fall time series of year vs. biomass 
plot(LATR[,"GMT_YEAR"], LATR[,3], type="l", lwd=2, col=4, ylim=c(0,1.6), xlab="Year", ylab="Biomass", main="American Shad=44")
lines(LATRspring[,"GMT_YEAR"], LATRspring[,3], type="l", col="red") 
abline(v=2007.5,col=4,lty=2)

#ATTEMPTING TO ADD LINES TO THIS for spring 
#Set spring data
SpringLHS <-subset(logLHS, fall==0)
SPECAGGspring <-serialAgg(SpringLHS, AggCats=c("GMT_YEAR", "DECDEG_BEGLAT"), AggTarg="logEXPCATCHWT")
#Round latitudes to nearest whole number 
SPECAGGlatrROUNDspring<-round(SPECAGGspring[,"DECDEG_BEGLAT"])
#Specify latitude 
LHSlatr <- SPECAGGspring[,"DECDEG_BEGLAT"]
unique(round(LHSlatr))
#Round the latitudes of aggregatd species data
SPECAGGspring[,"DECDEG_BEGLAT"] <- round(SPECAGGspring[,"DECDEG_BEGLAT"])
LATRspring <- (SPECAGGspring[SPECAGGspring[,"DECDEG_BEGLAT"]==44,])
