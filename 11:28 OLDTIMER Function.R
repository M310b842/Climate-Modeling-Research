#Everyone else used Function1 as name for function

sppCode=24
#Create function: 
#Given a species, E of species 
OLDTIMER=function(sppCode, E) {  
  #Step 1-select data
  dataOrig=myDataFiltered[myDataFiltered[,"spp"]==sppCode & myDataFiltered[,"yr"]%in%(1977:2008),]
  fiti=fitGP(dataOrig,y="logn",x=c("logn", EnvtDatColumns),pop="pop", E=E, tau=1, scaling="none", predictmethod="loo")
  #fiti=fitGP(dataOrig,y="logn",x=c("logn"),pop="pop", E=E, tau=1, scaling="none", predictmethod="loo")
  #set up object to store new data
  dataNew=dataOrig[dataOrig[,"yr"]>2000,]
  #For year t, first, predict new abundance
  for (t in 2009:3009) {
    dataNew_Nt=predict(fiti,newdata=dataNew[dataNew[,"yr"]%in%((t-1):(t-1-E)),])
    dataNew_Nt=na.omit(dataNew_Nt$outsampresults[,"predmean"])
    
    #Now select environment
    dataNew_t=serialAgg(dataOrig,AggCats="pop", AggTarg=colnames(dataOrig)) [,-1]
    dataNew_t[,"yr"]=t
    dataNew_t[,"logn"]=dataNew_Nt
    dataNew=rbind(dataNew,dataNew_t)
  }
  
  dataNew_twant=dataNew[dataNew[,"yr"]>(3009-40),]
  SDs=serialAgg(dataNew_twant, AggCats="pop", AggTarg="logn", FUN=sd)
  Fluctuating=SDs[,"logn"]>0.05
  print(mean(Fluctuating))
  print(SDs)
  return(dataNew_twant)
}


#Check species: 
OLDTIMER(24, 6)

#PLOT TIME SERIES: LOGN ACROSS TIME BY POPULATION
#only need clearnose skate=24
par(mfrow = c(3, 3))
for (p in unique(dataNew_twant[,"pop"])) {
  dataNew_twant1=dataNew_twant[dataNew_twant[,"pop"]==p,]
  plot(dataNew_twant1[,"yr"], dataNew_twant1[,"logn"], type="l", main=p, xlab="year", ylab="logn")
}


