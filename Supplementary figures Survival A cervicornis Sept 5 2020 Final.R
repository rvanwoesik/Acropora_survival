#Supplementary Data
install.packages("spBayesSurv")
install.packages("coda")

library(spBayesSurv)
library(survival)
library(spBayesSurv)
library(coda)


#setwd("C:/Users/rbanister2019/Desktop/Survivor A.pal/R Scripts")
setwd("C:/Users/Raymond Banister/Desktop/FIT/RVW/Survivor A.pal/R Scripts")


#####
AllData3=read.csv("Florida_Final_RBB.csv")
colnames(AllData3)

unique(AllData3$Time)
unique(AllData3$Outplant_Year)
#Noticed some negative numbers
AllData3 <- subset(AllData3, Time > 0)

# MCMC parameters
nburn=500; nsave=300; nskip=0;
set.seed(1)
# Note larger nburn, nsave and nskip should be used in practice.
mcmc=list(nburn=nburn, nsave=nsave, nskip=nskip, ndisplay=1000);
prior = list(M=10, r0=1);
#mcmc <- list(nburn = 5000, nsave = 2000, nskip = 4, ndisplay = 1000)

# Make Methods a factor
AllData3$Method=as.factor(AllData3$Method)
AllData3$Year=as.factor(AllData3$Outplant_Year)
AllData3$Agency=as.factor(AllData3$Agency)
AllData3$Region=as.factor(AllData3$Region)
AllData3$Zone=as.factor(AllData3$Zone)
AllData3$Size=as.factor(AllData3$Size)
#plot(AllData3$Agency)
#####


#For Supplementary figures 1 and 3.
#########
#Creating histograms
Agency = AllData3$Agency
summary(Agency)
plot(Agency, ylab = "# of Outplants", ylim = c(0,18000))
box("plot","solid")


Habitat = AllData3$Zone
summary(Habitat)
plot(Habitat, ylab = "# of Outplants", ylim = c(0,10000), cex.axis=0.8)
box("plot","solid")

Region = AllData3$Region
summary(Region)
plot(Region, ylab = "# of Outplants", ylim = c(0,10000), cex.axis=0.8)
box("plot","solid")

Region1 = factor(AllData3$Region, levels=c("Broward-Miami", "Biscayne", "Upper Keys", "Middle Keys", "Lower Keys","Marquesas", "Dry Tortugas"))
#Region = AllData3$Region
summary(Region1)
plot(Region1, ylab = "# of Outplants", ylim = c(0,10000),cex.names=1.4,cex.axis=1.1) #cex.names changes x-axis size only and cex.axis changes y -axis size only
box("plot","solid")

#cex.lab=0.8, cex.axis=0.8, cex.main=0.8, cex.sub=0.8, cex.names=0.8

#Histogram of S2 was done on excel --> Florida_Final_Month_Year

#Different Sizes with same habitat for each region 
#####
Region = indeptCoxph(formula = Surv(Time, Censor)~ Size + Zone +Region +Year, data=AllData3,   prior=NULL, mcmc=mcmc)
sfit2=summary(Region) 
sfit2


par(mfrow = c(1,1))
tgrid <- seq(0.1, 800)
#####

#Figure 6 Supplementary
#Different Sizes with same habitat for each region
#Broward Miami
#####
xpred = data.frame(Size=c("S","M","L"), row.names = c("S", "M", "L"), Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), Region=c("Broward-Miami", "Broward-Miami", "Broward-Miami"), Year=c("2016", "2016", "2016"))
plot(Region, xnewdata=xpred, tgrid=tgrid,legend=NULL)
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3),lwd = 2) # xlab="Time (days)", ylab="Survival")

install.packages("spatstat")
library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 7 Supplementary
#Different Sizes with same habitat for each region 
#Biscayne
#####
xpred = data.frame(Size=c("S","M","L"), row.names = c("S", "M", "L"), Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), Region=c("Biscayne", "Biscayne", "Biscayne"), Year=c("2016", "2016", "2016"))
plot(Region, xnewdata=xpred, tgrid=tgrid,legend=NULL)
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 8 Supplementary
#Different Sizes with same habitat for each region 
#Upper Keys
#####
xpred = data.frame(Size=c("S","M","L"), row.names = c("S", "M", "L"), Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), Region=c("Upper Keys", "Upper Keys", "Upper Keys"), Year=c("2016", "2016", "2016"))
plot(Region, xnewdata=xpred, tgrid=tgrid,legend=NULL)
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 9 Supplementary
#Different Sizes with same habitat for each region 
#Middle Keys
#####
xpred = data.frame(Size=c("S","M","L"), row.names = c("S", "M", "L"), Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), Region=c("Middle Keys", "Middle Keys", "Middle Keys"), Year=c("2016", "2016", "2016"))
plot(Region, xnewdata=xpred, tgrid=tgrid,legend=NULL)
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 10 Supplementary
#Different Sizes with same habitat for each region 
#Marquesas
#####
xpred = data.frame(Size=c("S","M","L"), row.names = c("S", "M", "L"), Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), Region=c("Marquesas", "Marquesas", "Marquesas"), Year=c("2016", "2016", "2016"))
plot(Region, xnewdata=xpred, tgrid=tgrid,legend=NULL)
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 11 Supplementary
#Different Sizes with same habitat for each region 
#Dry Tortugas
#####
xpred = data.frame(Size=c("S","M","L"), row.names = c("S", "M", "L"), Zone=c("Fore Reef", "Fore Reef", "Fore Reef"), Region=c("Dry Tortugas", "Dry Tortugas", "Dry Tortugas"), Year=c("2016", "2016", "2016"))
plot(Region, xnewdata=xpred, tgrid=tgrid,legend=NULL)
legend("bottomleft", col = c("black","red", "green"), legend = c("1-15 cm TLE", "16-50 cm TLE", "51-160 cm TLE"),lty = c(1,2,3),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.


#Habitats

Habitat = indeptCoxph(formula = Surv(Time, Censor)~ Size + Zone +Region +Year, data=AllData3,   prior=NULL, mcmc=mcmc)

sfit2=summary(Habitat)
sfit2

traceplot(mcmc(Habitat$beta[1,]), main="beta1")
traceplot(mcmc(Habitat$h.scaled[2,]), main="h")

par(mfrow = c(1,1))
tgrid <- seq(0.1, 800)


#Supplementary figure 12: Zone in different regions for small colonies
#Broward Miami/small
#####
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Size=c("S","S","S","S","S","S"), 
                   Region=c("Broward-Miami", "Broward-Miami", "Broward-Miami", "Broward-Miami","Broward-Miami","Broward-Miami"), 
                   Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Habitat, xnewdata=xpred, tgrid=tgrid,legend=NULL) 
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Supplementary figure 13: Zone in different regions for small colonies
#Biscayne/small
#####
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Size=c("S","S","S","S","S","S"),                    
                   Region=c("Biscayne", "Biscayne", "Biscayne","Biscayne","Biscayne","Biscayne"),
                   Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Habitat, xnewdata=xpred, tgrid=tgrid,legend = NULL) 
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Supplementary figure 14: Zone in different regions for small colonies
#Upper Keys/small
#####
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Size=c("S","S","S","S","S","S"), 
                   Region=c("Upper Keys", "Upper Keys", "Upper Keys","Upper Keys","Upper Keys","Upper Keys"),
                   Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Habitat, xnewdata=xpred, tgrid=tgrid,legend=NULL) 
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Supplementary figure 15: Zone in different regions for small colonies
#Middle Keys/small
#####
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Size=c("S","S","S","S","S","S"), 
                   Region=c("Middle Keys", "Middle Keys", "Middle Keys","Middle Keys","Middle Keys","Middle Keys"),
                   Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Habitat, xnewdata=xpred, tgrid=tgrid,legend=NULL) 
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Supplementary figure 16: Zone in different regions for small colonies
#Marquesas/small
#####
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Size=c("S","S","S","S","S","S"), 
                   Region=c("Marquesas", "Marquesas","Marquesas","Marquesas","Marquesas","Marquesas"),
                   Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Habitat, xnewdata=xpred, tgrid=tgrid,legend=NULL) 
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####


#Supplementary figure 17: Zone in different regions for small colonies
#Dry Tortugas/small
#####
xpred = data.frame(Zone=c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   row.names = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"), 
                   Size=c("S","S","S","S","S","S"), 
                   Region=c("Dry Tortugas", "Dry Tortugas", "Dry Tortugas","Dry Tortugas","Dry Tortugas","Dry Tortugas"),
                   Year=c("2016", "2016", "2016", "2016", "2016", "2016"))
plot(Habitat, xnewdata=xpred, tgrid=tgrid,legend=NULL) 
legend("bottomleft", col = c("black","red", "green", "blue", "cyan", "magenta"), legend = c("Back Reef", "Bank/Shelf", "Fore Reef", "Lagoon", "Reef Crest", "Unknown"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####
























#######################OLD CODE BELOW#############


#NEW supplementary figures (07_22_2020) 
#Based off of Figure 5: Medium - All Subregions - 2016 - Back Reef

Geno2 = indeptCoxph(formula = Surv(Time, Censor)~ Size + Zone +Region +Year, data=AllData3,   prior=NULL, mcmc=mcmc)
sfit2=summary(Geno2) 
sfit2
tgrid <- seq(0.1, 800)
par(mfrow=c(1,1))

#Supplementary figure 18 #S18_Regions_M_2016_BankShelf
xpred = data.frame(Zone=c("Bank/Shelf", "Bank/Shelf", "Bank/Shelf", "Bank/Shelf", "Bank/Shelf", "Bank/Shelf", "Bank/Shelf"), 
                   Region=c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   row.names = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   Size=c("M","M","M","M", "M","M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta","yellow"), legend = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"),lty = c(1,2,3,4,5,6,7),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)

par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')

#Supplementary figure 19 # S19_Regions_M_2016_ForeReef
xpred = data.frame(Zone=c("Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef", "Fore Reef"), 
                   Region=c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   row.names = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   Size=c("M","M","M","M", "M","M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta","yellow"), legend = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"),lty = c(1,2,3,4,5,6,7),lwd = 2) # xlab="Time (days)", ylab="Survival")

par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')

#Supplementary figure 20 # S20_Regions_M_2016_Lagoon
xpred = data.frame(Zone=c("Lagoon", "Lagoon", "Lagoon", "Lagoon", "Lagoon", "Lagoon", "Lagoon"), 
                   Region=c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   row.names = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   Size=c("M","M","M","M", "M","M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta","yellow"), legend = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"),lty = c(1,2,3,4,5,6,7),lwd = 2) # xlab="Time (days)", ylab="Survival")

par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')

#Supplementary figure 21 # S21_Regions_M_2016_Reef Crest
xpred = data.frame(Zone=c("Reef Crest", "Reef Crest", "Reef Crest", "Reef Crest", "Reef Crest", "Reef Crest", "Reef Crest"), 
                   Region=c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   row.names = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   Size=c("M","M","M","M", "M","M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta","yellow"), legend = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"),lty = c(1,2,3,4,5,6,7),lwd = 2) # xlab="Time (days)", ylab="Survival")

par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')

#Supplementary figure 22 # S22_Regions_M_2016_Unknown
xpred = data.frame(Zone=c("Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown", "Unknown"), 
                   Region=c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   row.names = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"), 
                   Size=c("M","M","M","M", "M","M","M"), Year=c("2016", "2016", "2016", "2016", "2016", "2016", "2016"))
plot(Geno2, xnewdata=xpred, tgrid=tgrid, legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta","yellow"), legend = c("Upper Keys", "Middle Keys", "Lower Keys", "Dry Tortugas", "Marquesas", "Broward-Miami", "Biscayne"),lty = c(1,2,3,4,5,6,7),lwd = 2) # xlab="Time (days)", ylab="Survival")

par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Probability of survival')













#supplementary figures removed after reviewal below.
########
#YEAR SUPPLEMENTARY DATA
#Data for Year - Size\habitat\region
#####
SZRY = indeptCoxph(formula = Surv(Time, Censor)~ Size+Zone+Region+Year, data=AllData3,   prior=NULL, mcmc=mcmc)
#All1 = survregbayes(formula = Surv(Time, Censor)~ Size+Zone+Region+Year, data=AllData3,   prior=NULL, mcmc=mcmc, selection=TRUE)
sfit2=summary(SZRY)
sfit2

traceplot(mcmc(SZRY$beta[1,]), main="beta1")
traceplot(mcmc(SZRY$h.scaled[2,]), main="h")

par(mfrow = c(1,1))
tgrid <- seq(0.1, 800)
#####

#Figure 16 Supplementary: 
#Years_Back Reef_Broward-Miami_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Broward-Miami","Broward-Miami","Broward-Miami","Broward-Miami", "Broward-Miami", "Broward-Miami"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")


library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 17 Supplementary: 
#Years_Back Reef_Biscayne_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Biscayne","Biscayne","Biscayne","Biscayne","Biscayne","Biscayne"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 18 Supplementary: 
#Years_Back Reef_Upper Keys_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Upper Keys","Upper Keys","Upper Keys","Upper Keys", "Upper Keys", "Upper Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 19 Supplementary: 
#Years_Back Reef_Middle Keys_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Middle Keys","Middle Keys","Middle Keys","Middle Keys", "Middle Keys", "Middle Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 20 Supplementary: 
#Years_Back Reef_Lower Keys_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Lower Keys","Lower Keys","Lower Keys","Lower Keys", "Lower Keys", "Lower Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 21 Supplementary: 
#Years_Back Reef_Marquesas_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Marquesas","Marquesas","Marquesas","Marquesas", "Marquesas", "Marquesas"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 22 Supplementary: 
#Years_Back Reef_Dry Tortugas_Medium
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("M","M","M","M", "M","M"),
                   Region=c("Dry Tortugas","Dry Tortugas","Dry Tortugas","Dry Tortugas", "Dry Tortugas", "Dry Tortugas"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####


#Large

#Figure 23 Supplementary: 
#Years_Back Reef_Broward-Miami_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Broward-Miami","Broward-Miami","Broward-Miami","Broward-Miami", "Broward-Miami", "Broward-Miami"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####


#Figure 24 Supplementary: 
#Years_Back Reef_Biscayne_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Biscayne","Biscayne","Biscayne","Biscayne","Biscayne","Biscayne"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 25 Supplementary: 
#Years_Back Reef_Upper Keys_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Upper Keys","Upper Keys","Upper Keys","Upper Keys", "Upper Keys", "Upper Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####


#Figure 26 Supplementary: 
#Years_Back Reef_Middle Keys_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Middle Keys","Middle Keys","Middle Keys","Middle Keys", "Middle Keys", "Middle Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####


#Figure 27 Supplementary: 
#Years_Back Reef_Lower Keys_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Lower Keys","Lower Keys","Lower Keys","Lower Keys", "Lower Keys", "Lower Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####


#Figure 28 Supplementary: 
#Years_Back Reef_Marquesas_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Marquesas","Marquesas","Marquesas","Marquesas", "Marquesas", "Marquesas"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 29 Supplementary: 
#Years_Back Reef_Dry Tortugas_Large
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("L","L","L","L", "L","L"),
                   Region=c("Dry Tortugas","Dry Tortugas","Dry Tortugas","Dry Tortugas", "Dry Tortugas", "Dry Tortugas"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####



######SMALL

#Figure 30 Supplementary:
#Years_Back Reef_Broward-Miami_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Broward-Miami","Broward-Miami","Broward-Miami","Broward-Miami", "Broward-Miami", "Broward-Miami"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 31 Supplementary:
#Years_Back Reef_Biscayne_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Biscayne","Biscayne","Biscayne","Biscayne","Biscayne","Biscayne"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 32 Supplementary:
#Years_Back Reef_Upper Keys_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Upper Keys","Upper Keys","Upper Keys","Upper Keys", "Upper Keys", "Upper Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 33 Supplementary:
#Years_Back Reef_Middle Keys_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Middle Keys","Middle Keys","Middle Keys","Middle Keys", "Middle Keys", "Middle Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 34 Supplementary:
#Years_Back Reef_Lower Keys_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Lower Keys","Lower Keys","Lower Keys","Lower Keys", "Lower Keys", "Lower Keys"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 35 Supplementary:
#Years_Back Reef_Marquesas_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Marquesas","Marquesas","Marquesas","Marquesas", "Marquesas", "Marquesas"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####

#Figure 36 Supplementary:
#Years_Back Reef_Dry Tortugas_Small
#####
xpred = data.frame(Year=c("2012", "2014","2015", "2016", "2017", "2018"),
                   row.names=c("2012", "2014","2015", "2016", "2017", "2018"),
                   Zone=c("Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef", "Back Reef"),
                   Size=c("S","S","S","S", "S","S"),
                   Region=c("Dry Tortugas","Dry Tortugas","Dry Tortugas","Dry Tortugas", "Dry Tortugas", "Dry Tortugas"))
plot(SZRY, xnewdata=xpred, tgrid=tgrid,legend=NULL) # xlab="Time (days)", ylab="Survival")
legend("bottomleft", col = c("black","red", "green","blue","cyan","magenta"), legend = c("2012", "2014", "2015","2016","2017","2018"),lty = c(1,2,3,4,5,6),lwd = 2) # xlab="Time (days)", ylab="Survival")

library(spatstat)
par(xpd=NA)  #make it so you can plot outside of the plot area
whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(xlab='Time (days)') #label the axis 'Days'

whiteout<-clickpoly(add=T,np=1,col='white') #draw a white polygon around 'Time'
plot(whiteout,add=T,col='white',border='white') #plot the polygon covering the words
title(ylab='Survival')
# whiteout2<-polygon(x=c(240.3147,647.6728,627.2025,330.3838),y=c(-0.2452871,-0.2709160,-0.4022639,-0.4022639),col='white',border='white') #no need to plot polygon after.
#####