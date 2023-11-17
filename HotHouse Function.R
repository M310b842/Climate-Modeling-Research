#prevent error warnings
fitGP=function(...) suppressWarnings(GPEDM::fitGP(...))
#cor=correlation 
#find syncrony 
mmu=function(m){ s=cor(m,use="pairwise.complete.obs"); mean(s[upper.tri(s)],na.rm=TRUE); }
matricize=function(di){
  tl=range(di[,2],na.rm=TRUE); ts=tl[1]+(0:diff(tl)); pops=sort(unique(di[,1])); 
  m=matrix(NA,length(pops),length(ts)); for(i in 1:nrow(di)) m[pops==di[i,1], ts==di[i,2]]=di[i,3]; m[1:nrow(m),];
}
popSync=function(dat) mmu(t(matricize(dat[,c("pop","yr","logn")])))
#Get phi values from fitted model
PhiGet=function(gpModel){ 
  vs=colSums(matrix(head(gpModel$pars,-3),nrow=gpModel$inputs$E)); 
  names(vs)=c("Phi_logn",paste0("Phi_",EnvtDatColumns)); vs; 
}

MyDataUse=MyData[MyData[,"fall"]==1,]
MyDataAgg=serialAgg(MyDataUse,AggCats=c("spp","pop","yr"))
myDataFiltered=popsFilterSimp(MyDataAgg,envtDrivers=EnvtDatColumns)

sppCode=163; E=6; 
#Change varsChange 
varsChange=c("wtemp", "stemp","zp")
ChangeLvl=c(1.5,1.5,-1.5)
#original 
#ChangeLvl=c(1.5, -1.5)

#Input: given specific species, find E for that species, which var we are changing?
# oringial hothouseSim=function(sppCode, E,varsChange=c("stemp", "zp"), ChangeLvl=c(1.5, -1.5)) {
hothouseSim=function(sppCode, E,varsChange=c("wtemp", "stemp", "zp"), ChangeLvl=c(1.5,1.5,-1.5)) {  

#Step 1-select data
  dataOrig=myDataFiltered[myDataFiltered[,"spp"]==sppCode & myDataFiltered[,"yr"]%in%(1977:2008),]

  dataOrigYears=unique(dataOrig[,"yr"])
  dataOrigYears
  fiti=fitGP(dataOrig,y="logn",x=c("logn", EnvtDatColumns),pop="pop", E=E, tau=1, scaling="none", predictmethod="loo")
  Phis=PhiGet(fiti)
  Phis

   #Within Function: run simulations w/ new environment
   #set up object to store new data
  dataNew=dataOrig[dataOrig[,"yr"]>2000,]
   #For year t, first, predict new abundance
  for (t in 2009:3009) {
   dataNew_Nt=predict(fiti,newdata=dataNew[dataNew[,"yr"]%in%((t-1):(t-1-E)),])
   #dataNew_Nt$outsampresults[,"predmean"]
   dataNew_Nt=na.omit(dataNew_Nt$outsampresults[,"predmean"])

   #Now select environment
   set.seed(t); yrUse_t=sample(dataOrigYears,1)
    dataNew_t=dataOrig[dataOrig[,"yr"]==yrUse_t,]

   dataNew_t[,"yr"]=t
   dataNew_t[,"logn"]=dataNew_Nt

   #alter environment
    for (i in varsChange) dataNew_t[,i]=dataNew_t[,i]+ChangeLvl[varsChange==i]
    dataNew=rbind(dataNew, dataNew_t)
}

  Sync=popSync(dataNew)
  SD=sd(dataNew[,"logn"])
  mu=mean(dataNew[,"logn"])

  return(c(Phis, Sync=Sync, SD=SD, mu=mu))
}

hothouseSim(sppCode=156, E=6, ChangeLvl=c(1.5,1.5,-1.5))
#Output: we want to get: synchrony, mean, sd
