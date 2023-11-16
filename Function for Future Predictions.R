
E=6


#Define function
#1. Select data
#SpeciesPrediction <- function(season, speciesID, E) {
 #Select data
 # MyDataUse=MyData[MyData[,"fall"]==season,]
 # MyDataAgg=serialAgg(MyDataUse, AggCats=c("spp", "pop", "yr"))
 # myDataFiltered=popsFilterSimp(MyDataAgg,envtDrivers=EnvtDatColumns)
 # species_data <- data[data[,"spp"] == speciesID & data[,"yr"] %in% (1977:2016),]
 # E=E
 # fitAllyears <- fitGP(data = species_data, y = "logn", x = c("logn", EnvtDatColumns), pop = "pop", E = E, tau = 1, scaling = "none", predictmethod = "loo")
#  fitAllyears_outsampfitstats <- fitAllyears$outsampfitstats[1]
  #2. Model for 1977: 2001 w/ E from column in Excel
 # fiti <- fitGP(data = species_data[species_data[,"yr"] < 2002,], y = "logn", x = c("logn", EnvtDatColumns), pop = "pop", E = E, tau = 1, scaling = "none", predictmethod = "loo")
 # fiti_outsampfitstats <- fiti$outsampfitstats[1]
  #3. For loop predict 2002:2016 
 # result <- matrix(nrow = 15, ncol = 2)
  #Cycle through all time steps and save to CSV 
 # for (i in 2002:2016) {
  #  predi <- c(i, predict(fiti, newdata = species_data[species_data[,"yr"] %in% (i:(i-E)),])$outsampfitstats[2])
  #  result[i-2001,] <- predi
 # }
 # output_file <- paste("species_", speciesID, "_results.csv", sep = "")
 # write.csv(round(t(result), 2), output_file)
 # return(output_file)  # Return the path to the saved CSV file
}

#TOOK THIS OUT ABOVE E=
# fitAllyears <- fitGP(data = species_data, y = "logn", x = c("logn", EnvtDatColumns), pop = "pop", E = E, tau = 1, scaling = "none", predictmethod = "loo")
#  fitAllyears_outsampfitstats <- fitAllyears$outsampfitstats[1]

#result_file <- SpeciesPrediction(speciesID)
#cat("Results saved to:", result_file, "\n")







#Define function
#1. Select data
SpeciesPrediction <- function(season, speciesID, E) {
  #Select data
  MyDataUse=MyData[MyData[,"fall"]==season,]
  MyDataAgg=serialAgg(MyDataUse, AggCats=c("spp", "pop", "yr"))
  myDataFiltered=popsFilterSimp(MyDataAgg,envtDrivers=EnvtDatColumns)
  species_data <- data[data[,"spp"] == speciesID & data[,"yr"] %in% (1977:2016),]
  E=E
  #2. Model for 1977: 2001 w/ E from column in Excel
  fiti <- fitGP(data = species_data[species_data[,"yr"] < 2002,], y = "logn", x = c("logn", EnvtDatColumns), pop = "pop", E = E, tau = 1, scaling = "none", predictmethod = "loo")
  fiti_outsampfitstats <- fiti$outsampfitstats[1]
  #3. For loop predict 2002:2016 
  result <- matrix(nrow = 15, ncol = 2)
  #Cycle through all time steps and save to CSV 
  for (i in 2002:(2013+3*(season==1))) {
    predi <- c(i, predict(fiti, newdata = species_data[species_data[,"yr"] %in% (i:(i-E)),])$outsampfitstats[2])
    result[i-2001,] <- predi
  }
  output_file <- paste("species_", speciesID, "_results.csv", sep = "")
  write.csv(round(t(result), 2), output_file)
  return(output_file)  # Return the path to the saved CSV file
}



#result_file <- SpeciesPrediction(speciesID)
#cat("Results saved to:", result_file, "\n")

#1=Fall
#0=Spring
SpeciesPrediction(0, 163, 6)




#Cleaned up EDM model finder
#Given code:
###installing packages - only need to do once
install.packages("rio")
install.packages("devtools")
library(devtools)
devtools::install_github("tanyalrogers/GPEDM",force=TRUE)


###Loading data
library(rio); library(GPEDM);
alldat0=import("~/Downloads/NOAA_EcosystemData_11-8-2023.csv") 
SpeciesUse=c(103,104,105,106,107,108,13,131,135,141,143,15,156,163,164,171,172,193,194,197,23,24,26,301,33,34,35,401,502,503,72,73,74,76,77,78)
DataEntriesUse=alldat0[,"SVSPP"]%in%SpeciesUse
SpeciesNamesCodes=unique(alldat0[DataEntriesUse,c("SVSPP","spnm")])
alldat=as.matrix(alldat0[DataEntriesUse, -c(17,18)])

plotGPpredSimp=function(fit,SpName){
  pred=fit$outsampresults; pred=pred[order(pred[,1]),]; up=head(unique(pred[,2]),15);
  info=round(c(E=fit$inputs$E,R2=fit$outsampfitstats[1]),2)
  pdf(paste0(SpName,"___",paste0(info,collapse="__"),".pdf"),width=12,height=9); 
  par(mfrow=c(3,3)+(length(up)>8),lheight=.8); plot(0,0,col=0,axes=FALSE,ylab="",xlab=""); text(0,1.5,cex=4,SpName,xpd=NA);
  text(0,0,cex=2,paste0("E=",info[1],"\n Leave-one-out \n global R2 = ",round(info[2],2)),col=4,xpd=NA)
  for(p in up){
    predi=na.omit(pred[pred[,2]==p,]); ttl=paste0("pop# ",p,"   pop R2=",round(cor(predi[,c("obs","predmean")])[1,2]^2,2));
    matplot(predi[,1],predi[,c("obs","predmean")],lwd=1,pch=16,lty=1,type="o",col=c(1,4),ylab="Obs (black), Predicted (blue)",main=ttl,xlab="")
    polygon(c(predi[,1],rev(predi[,1])),c(predi[,3],rev(predi[,3]))+c(predi[,5],-rev(predi[,5])),col=rgb(0,0,0.5,alpha=0.15),border=0)
  }
  dev.off()
}

zrochk=function(x,mini=min(x,na.rm=TRUE)){ if(mini==0) x=x+min(x[x>0],na.rm=TRUE); x; }
zrochkSer=function(x,cats){ for(ci in unique(cats)) x[cats==ci]=zrochk(x[cats==ci]); x; }
serialAgg=function (x, AggCats, AggTarg = NULL, FUN = function(x) mean(x, na.rm = TRUE)){
  if (is.null(AggTarg)) {
    if (is.numeric(AggCats)) AggTarg = (1:ncol(x))[-AggCats]
    if (is.character(AggCats)) AggTarg = colnames(x)[!colnames(x) %in% AggCats]
  }
  Numbers = prod(apply(t(t(x[, AggCats])), 2, is.numeric))
  ncat = length(AggCats)
  if (ncat == 1) Cats = as.character(x[, AggCats])
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

#codify: turn rows of @param x into single characters, with columns separated by @param sep
#' @cols which cols of x to use; defauls to all
codify=function(x,cols=1:ncol(x),sep="_")  as.matrix(cbind(Index=apply(x[,cols], 1, paste0, collapse=sep),  x[,-cols]))

#nrm2: Normalize x. Default is global; if levels Lvl supplied will be local.
#' @param x vector of values to normalize
#' @param Lvl optional vector denoting category of each x value for local lormalization
nrm2=function (x, Lvl = rep(1, length(x)), xuse = rep(TRUE, length(x)),zeroNullSD = FALSE, center = FALSE){
  normcent = function(x, xuse) {
    SD = sd(x[xuse], na.rm = TRUE)
    if ((zeroNullSD & SD == 0) | center) SD = 1
    (x - mean(x[xuse], na.rm = TRUE))/SD
  }
  xout = x
  for (i in unique(Lvl)) xout[Lvl == i] = normcent(x[Lvl == i], xuse[Lvl == i])
  xout
}

# popsFilterSimp requires the functions serialAgg, nrm2, and codify to be entered into console
# datfull must only have numeric (or NA) entries, and must have column names, which each contain:
# "spp" - Species number
# "pop" - Population number (can be rounded latitude)
# "yr" - Year
# "n" - Mean population biomass
# We'll also need to normalize environmental data. 
# envtDrivers are the column numbers (or names) that tell the function which columns contain environmental data.
popsFilterSimp=function(datfull,envtDrivers=NULL){
  sa=serialAgg(datfull,c("spp","pop"),"n",FUN=function(x) c(mean(x!=0 & !is.na(x)), sum(x!=0 & !is.na(x)), sum(!is.na(x))))
  colnames(sa)[3:5]=c("muPres","nPres","nObs")
  use=sa[,"nPres"]>9 & sa[,"nObs"]>12
  adi=datfull[codify(datfull[,c("spp","pop")])%in%codify(sa[use,c("spp","pop")]),]
  
  #log population biomass, call it new column logn
  #adi=cbind(adi,logn=log(zrochkSer(adi[,"n"],adi[,"spp"])))
  #normalize biomass by species and location
  ID=IDn=1e4*adi[,"spp"]+adi[,"pop"]
  adi[,"logn"]=nrm2(adi[,"logn"],IDn,zeroNullSD=TRUE)
  #if present, normalize environmental drivers by species
  if(!is.null(envtDrivers)) adi[,envtDrivers]=apply(adi[,envtDrivers],2,nrm2,Lvl=adi[,"spp"]);
  #Filter out species with only 1 pop left (never really used)
  nps=serialAgg(adi,"spp","pop",FUN=function(x) length(unique(x)))
  adi=adi[adi[,1]%in%nps[nps[,2]>1,1],]
  adi[!is.na(adi[,"logn"]),]
}

EnvtDatColumns=c("wtemp", "stemp", "zp", "nao")
NoZP=c("wtemp", "stemp", "nao")
##Creates a new matrix that is simplified to work with
MyData<-cbind(spp=alldat[,"SVSPP"], pop=round(alldat[,"DECDEG_BEGLAT"]),
              yr=alldat[,"GMT_YEAR"], n=alldat[,"EXPCATCHWT"], alldat[,c(EnvtDatColumns,"fall", "logn")])
MyDataUse=MyData[MyData[,"fall"]==1,]
MyDataAgg=serialAgg(MyDataUse,AggCats=c("spp","pop","yr"))

myDataFiltered=popsFilterSimp(MyDataAgg,envtDrivers=EnvtDatColumns)


#Function and code to use

BestER<-function(data,E,number,year, EDCi=EnvtDatColumns){
  species=data[data[,"spp"]==number & data[,"yr"]%in%(year),]
  R<-1:6
  for(E in 1:6) {
    fiti=fitGP(data=species, y="logn", x=c("logn", EDCi), pop="pop", E=E, tau=1, scaling="none", predictmethod="loo")
    R[E]<-fiti$outsampfitstats[1]}
  BestE<-which.max(R)
  BestRs<-R[BestE]
  Results<-round(c(number, BestE, BestRs),2)
  return(Results)
}

fitGP=function(...) suppressWarnings(GPEDM::fitGP(...))

for(speciesi in c(74, 76, 77, 78, 72, 141, 103)){
  print(BestER(data=myDataFiltered, E=E, number = speciesi, year = 1977:2016, EDCi=EnvtDatColumns))
}

#Graphing
numberi=103
E=6
species=myDataFiltered[myDataFiltered[,"spp"]==numberi & myDataFiltered[,"yr"]%in%(1977:2008),]
fiti=fitGP(data=species, y="logn", x=c("logn", EnvtDatColumns), pop="pop", E=E, tau=1, scaling="none", predictmethod="loo")
plotGPpredSimp(fiti,"Summer Flounder")

#Spring dataset
MyData<-cbind(spp=alldat[,"SVSPP"], pop=round(alldat[,"DECDEG_BEGLAT"]),
              yr=alldat[,"GMT_YEAR"], n=alldat[,"EXPCATCHWT"], alldat[,c(EnvtDatColumns,"fall","logn")])
MyDataUseSpring=MyData[MyData[,"fall"]==0,]
MyDataAggSpring=serialAgg(MyDataUseSpring,AggCats=c("spp","pop","yr"))

myDataFilteredSpring=popsFilterSimp(MyDataAggSpring,envtDrivers=EnvtDatColumns)

#Both dataset
MyData<-cbind(spp=alldat[,"SVSPP"], pop=round(alldat[,"DECDEG_BEGLAT"]),
              yr=alldat[,"GMT_YEAR"], n=alldat[,"EXPCATCHWT"], alldat[,c(EnvtDatColumns,"fall","logn")])
MyDataUseBoth=MyData[MyData[,"fall"]==0|1,]
MyDataAggBoth=serialAgg(MyDataUseBoth,AggCats=c("spp","pop","yr"))

myDataFilteredBoth=popsFilterSimp(MyDataAggBoth,envtDrivers=EnvtDatColumns)



#function
GiveRMSE<-function(Season, SpeciesID, E){
  MyDataUse=MyData[MyData[,"fall"]==Season,]
  MyDataAgg=serialAgg(MyDataUse,AggCats=c("spp","pop","yr"))
  myDataFiltered=popsFilterSimp(MyDataAgg,envtDrivers=EnvtDatColumns)
  species=myDataFiltered[myDataFiltered[,"spp"]==SpeciesID & myDataFiltered[,"yr"]%in%(1977:2016),]
  
  fiti=fitGP(data=species[species[,"yr"]<2002,], y="logn", x=c("logn",EnvtDatColumns), pop="pop", E=E, tau=1,scaling="none", predictmethod="loo")
  fiti$outsampfitstats[1]
  
  result=matrix(nrow=15,ncol=2)
  for(i in 2002:(2013+3*(Season==1))){
    predi=c(i,predict(fiti,newdata=species[species[,"yr"]%in%(i:(i-E)),])$outsampfitstats[2])
    result[i-2001,]=predi}
  return(result)
}
#1=Fall
#0=Spring
answer<-GiveRMSE(0, 131, 6)
answer
write.csv(round(t(answer),2),"Butterfish Spring.csv")

getwd() #saved here

