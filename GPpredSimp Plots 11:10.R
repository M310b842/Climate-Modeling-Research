species=myDataFiltered[myDataFiltered[,"spp"]==131 & myDataFiltered[,"yr"]%in%(1977:2008),]
E=6; 
fiti=fitGP(data =species ,y = "logn", x =c("logn", EnvtDatColumns), pop = "pop", E=E, tau=1, scaling = "none", predictmethod = "loo")
#Gives you R2 
fiti$outsampfitstats[1]

#Plots and saves it to your computer under myfishplot
plotGPpredSimp(fiti, "fishplot Butterfish")
