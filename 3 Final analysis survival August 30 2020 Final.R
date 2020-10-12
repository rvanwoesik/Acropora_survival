#Final analysis for Acropora cervicornis survival

library(spBayesSurv)
library(survival)
library(spBayesSurv)
library(coda)

setwd("C:/RobsR/survival/Florida 2019_2020")

#AllData3=read.csv("AlldataFL3_Year_revised.csv")
AllData3=read.csv("Florida_Final_RBB.csv")
colnames(AllData3)
AllData3

unique(AllData3$Time)
unique(AllData3$Outplant_Year)

#2013 is causing problems because there is not enough data across the habitats and regions
library(dplyr)
AllData3= filter(AllData3, !Outplant_Year %in% c(2013))
unique(AllData3$Outplant_Year)

#Check for negative numbers
#AllData3 <- subset(AllData3, Time > 0)

# Make Methods a factor
AllData3$Method=as.factor(AllData3$Method)
AllData3$Year=as.factor(AllData3$Outplant_Year)
AllData3$Agency=as.factor(AllData3$Agency)
AllData3$Region=as.factor(AllData3$Region)
AllData3$Zone=as.factor(AllData3$Zone)
AllData3$Size=as.factor(AllData3$Size)
AllData3$Reef=as.factor(AllData3$Reef)
#plot(AllData3$Agency)

#hist(AllData3$Outplant_Year)

# MCMC parameters
nburn=5000; nsave=5000; nskip=2;
#nburn=500; nsave=300; nskip=0;
set.seed(1)
mcmc=list(nburn=nburn, nsave=nsave, nskip=nskip, ndisplay=1000);
prior = list(M=10, r0=1);
#mcmc <- list(nburn = 5000, nsave = 2000, nskip = 4, ndisplay = 1000)


Geno2 = indeptCoxph(formula = Surv(Time, Censor)~ Size+Zone+Region+Year, data=AllData3,   prior=NULL, mcmc=mcmc)
sfit2=summary(Geno2) 
sfit2
tgrid <- seq(0.1, 800)
library(spatstat)



# Figure 3 Colony size
par(mar=c(5,10,3,5))
xpred = data.frame(Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), 
                   Region=c("Lower Keys", "Lower Keys", "Lower Keys"), 
                   row.names = c("1-15 cm", "16-50 cm", "51-160 cm"), 
                   Size=c("S","M","L"), Year=c("2016", "2016", "2016"))
#par(mar=c(7,9,2,2))
plot(Geno2, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Probability of Survival")
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab="Probability of survival")



# Figure 4 Habitats
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Region=c("Lower Keys", "Lower Keys", "Lower Keys", "Lower Keys", "Lower Keys", "Lower Keys"),
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon (Patch)", "Reef Crest", "Unknown"), 
                   Size=c("M","M","M","M", "M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Probability of survival")
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon (Patch)", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")


par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab="Probability of survival")



#Figure 5 Regions medium colonies in back reefs 
xpred = data.frame(Zone=c("Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef"), 
                   Region=c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   row.names = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   Size=c("M","M","M","M", "M","M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Probability of survival")
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta", "yellow"), legend = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")


par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')



#Figure 6 Latitude


AllData3$Lat=as.factor(AllData3$Latitude)
unique(AllData3$Lat)

nburn=2000; nsave=100; nskip=2;
mcmc=list(nburn=nburn, nsave=nsave, nskip=nskip, ndisplay=1000);
prior = list(M=10, r0=1);

max(AllData3$Latitude)
min(AllData3$Latitude)

AllData3$Lat1<-cut(AllData3$Latitude, c(24,24.5,25,25.5,26, 26.5), labels=c(24.5,25,25.5,26,26.5))
unique(AllData3$Lat1)
AllData3$Lat1=as.factor(AllData3$Lat1)
summary(AllData3$Lat1)


Lat3 = indeptCoxph(formula = Surv(Time, Censor)~ Size + Latitude + Year, data=AllData3,   prior=NULL, mcmc=mcmc)

sfit2=summary(Lat3) 
sfit2

summary(Lat3)
#xpred = data.frame(Latitude=c(24.5,25,25.5,26,26.5), row.names=c("Lat.24.5°","Lat.25°", "Lat.25.5°", "Lat.26°", "Lat.26.5°" ))

xpred = data.frame(row.names=c("Lat.24.5°","Lat.25°", "Lat.25.5°", "Lat.26°", "Lat.26.5°"),
                   Latitude=c(24.5,25,25.5,26,26.5),
                   Size=c("M","M","M","M","M"), Year=c("2016", "2016","2016", "2016","2016"))

plot(Lat3, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Probability of Survival")

legend("bottomleft", col = c("black","red", "green", "blue", "cyan"), legend = c("24.5°N","25°N","25.5°N","26°N","26.5°N"),lty = c(1,2,3,4,5,lwd = 2)) 


par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')





#Table 2
#summary(Geno2) # has some convergence issues, need to run a huge number of iterations because zones are not distributed through subregions
burn=5000; nsave=2000; nskip=2;
set.seed(1)
mcmc=list(nburn=nburn, nsave=nsave, nskip=nskip, ndisplay=1000);
prior = list(M=10, r0=1);

#pool size
#Geno3 = indeptCoxph(formula = Surv(Time, Censor)~ Zone+Region+Year, data=AllData3,   prior=NULL, mcmc=mcmc)
#sfit2=summary(Geno3) 
#summary(Geno3) # no convergence issues

#pool Zone, because zone has a low effect compared with other covariates
#Geno4 = indeptCoxph(formula = Surv(Time, Censor)~ Size+Region+Year, data=AllData3,   prior=NULL, mcmc=mcmc)
#summary(Geno4)
#traceplot(mcmc(Geno4$beta[1,])) # no convergence issues
