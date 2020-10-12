#Florida Outplant survival 

# Code E Brown, R van Woesik, L Roth, J Roth

#r set up projections, maps, and shapefiles}
library(rstudioapi)
library(survival)
library(rgdal)
library(raster)
library(rgeos)
library(coxme)
library(pracma)


setwd("C:/RobsR/survival/Florida 2019_2020")



TNC<-read.csv("Data/Take2TNC.csv", stringsAsFactors=FALSE)
dim(TNC)
sum(TNC$TotalOutplants) #2376
head(TNC)

TNC1<-TNC[1:134,]
TNC2<-TNC[135:150,]
TNC3<-TNC[151:158,]

#for 1-134
TNCAlive1<-TNC1[rep(seq(nrow(TNC1)), as.numeric(TNC1$Alive1)),] #make rows for each alive individual
TNCAlive12<-cbind(EventDate=TNCAlive1$MonDate1, Censor=rep(as.numeric(0), nrow(TNCAlive1)),TNCAlive1 ) 

TNCDead1<-TNC1[rep(seq(nrow(TNC1)),as.numeric(TNC1$Dead1)),] #make rows for each dead one
TNCDead12<-cbind(EventDate=TNCDead1$MonDate1, Censor=rep(as.numeric(1), nrow(TNCDead1)),TNCDead1 )

#for 135-150
TNCAlive2<-TNC2[rep(seq(nrow(TNC2)), as.numeric(TNC2$Alive2)),] #make rows for each alive individual
TNCAlive22<-cbind(EventDate=TNCAlive2$MonDate2, Censor=rep(as.numeric(0), nrow(TNCAlive2)),TNCAlive2 ) 

TNCDead21<-TNC2[rep(seq(nrow(TNC2)),as.numeric(TNC2$Dead1)),]
TNCDead211<-cbind(EventDate=TNCDead21$MonDate1, Censor=rep(as.numeric(1), nrow(TNCDead21)),TNCDead21)

TNCDead22<-TNC2[rep(seq(nrow(TNC2)),as.numeric(TNC2$Dead2)),]
TNCDead222<-cbind(EventDate=TNCDead22$MonDate2, Censor=rep(as.numeric(1), nrow(TNCDead22)),TNCDead22)

#for 151-158
TNCAlive3<-TNC3[rep(seq(nrow(TNC3)), as.numeric(TNC3$Alive3)),] #make rows for each alive individual
TNCAlive32<-cbind(EventDate=TNCAlive3$MonDate3, Censor=rep(as.numeric(0), nrow(TNCAlive3)),TNCAlive3 ) 

TNCDead31<-TNC3[rep(seq(nrow(TNC3)),as.numeric(TNC3$Dead1)),]
TNCDead311<-cbind(EventDate=TNCDead31$MonDate1, Censor=rep(as.numeric(1), nrow(TNCDead31)),TNCDead31)

TNCDead32<-TNC3[rep(seq(nrow(TNC3)),as.numeric(TNC3$Dead2)),]
TNCDead322<-cbind(EventDate=TNCDead32$MonDate2, Censor=rep(as.numeric(1), nrow(TNCDead32)),TNCDead32)

TNCDead33<-TNC3[rep(seq(nrow(TNC3)),as.numeric(TNC3$Dead3)),]
TNCDead333<-cbind(EventDate=as.numeric(TNCDead33$MonDate3), Censor=rep(as.numeric(1), nrow(TNCDead33)),TNCDead33)
str(TNCAll)

TNCAll<-data.frame(stringsAsFactors = FALSE)
TNCAll<-rbind(TNCAlive12,TNCDead12, TNCAlive22,TNCDead211, TNCDead222, TNCAlive32, TNCDead311, TNCDead322, TNCDead333)
TNCAll$Time<-TNCAll$EventDate-TNCAll$DateOutplanted
TNCAll[which(is.na(TNCAll$Time)),]

TNCfit <- survfit(Surv(as.numeric(Time), Censor) ~ Size, data = TNCAll)
plot(TNCfit)

N <- length(unique(TNCAll$Size))

#make plot of Survival by size, can make any plot of TNC data
plot(TNCfit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "TNC Survival Time by Size",col=2:3)
legend( "bottomleft", legend=sort(unique(TNCAll$Size)),
        col=2:3,  lty=1)


#Mote
Mote<-read.csv("Data/Take2Mote.csv", na.strings=c(""," ","NA", "<NA>", "."),stringsAsFactors=FALSE)
head(Mote)
sum(Mote$TotalOutplants) #14000
str(Mote)

Mote5<-Mote[1:50,]
Mote4<-Mote[51:420,]
Mote3<-Mote[421:700,]
Mote2<-Mote[701:2300,]
Mote1<-Mote[2301:2800,]

hist(Mote1$Alive)

#Mote1
Mote1$Alive<-Mote1$TotalOutplants-Mote1$NewDead1
Mote1Alive1<-Mote1[rep(seq(nrow(Mote1)), as.numeric(Mote1$Alive)),] #make rows for each alive individual
Mote1Alive<-cbind(Time=Mote1Alive1$Time1, Censor=rep(as.numeric(0), nrow(Mote1Alive1)),Mote1Alive1 ) 

Mote1Dead1<-Mote1[rep(seq(nrow(Mote1)),as.numeric(Mote1$NewDead1)),] #make rows for each dead one
Mote1Dead<-cbind(Time=Mote1Dead1$Time1, Censor=rep(as.numeric(1), nrow(Mote1Dead1)),Mote1Dead1 )

head(Mote2)
#Mote2
Mote2$Alive<-Mote2$TotalOutplants-Mote2$NewDead2
Mote2Alive1<-Mote2[rep(seq(nrow(Mote2)), as.numeric(Mote2$Alive)),] #make rows for each alive individual
Mote2Alive<-cbind(Time=Mote2Alive1$Time2, Censor=rep(as.numeric(0), nrow(Mote2Alive1)),Mote2Alive1 ) 

Mote2Dead1<-Mote2[rep(seq(nrow(Mote2)),as.numeric(Mote2$NewDead1)),] #make rows for each dead one
Mote2Dead1<-cbind(Time=Mote2Dead1$Time1, Censor=rep(as.numeric(1), nrow(Mote2Dead1)),Mote2Dead1 )
head(Mote2Dead1)

Mote2Dead2<-Mote2[rep(seq(nrow(Mote2)),as.numeric(Mote2$NewDead2)),] #make rows for each dead one
Mote2Dead2<-cbind(Time=Mote2Dead2$Time2, Censor=rep(as.numeric(1), nrow(Mote2Dead2)),Mote2Dead2 )


#Mote3
Mote3$Alive<-Mote3$TotalOutplants-Mote3$AllDead3
Mote3$Alive[Mote3$Alive<0]<-0
Mote3Alive1<-Mote3[rep(seq(nrow(Mote3)), as.numeric(Mote3$Alive)),] #make rows for each alive individual
Mote3Alive<-cbind(Time=Mote3Alive1$Time3, Censor=rep(as.numeric(0), nrow(Mote3Alive1)),Mote3Alive1 ) 
head(Mote3Alive1)

Mote3Dead1<-Mote3[rep(seq(nrow(Mote3)),as.numeric(Mote3$NewDead1)),] #make rows for each dead one
Mote3Dead1<-cbind(Time=Mote3Dead1$Time1, Censor=rep(as.numeric(1), nrow(Mote3Dead1)),Mote3Dead1 )
dim(Mote3Dead1)

Mote3Dead2<-Mote3[rep(seq(nrow(Mote3)),as.numeric(Mote3$NewDead2)),] #make rows for each dead one
Mote3Dead2<-cbind(Time=Mote3Dead2$Time2, Censor=rep(as.numeric(1), nrow(Mote3Dead2)),Mote3Dead2 )

Mote3Dead3<-Mote3[rep(seq(nrow(Mote3)),as.numeric(Mote3$NewDead3)),] #make rows for each dead one
Mote3Dead3<-cbind(Time=Mote3Dead3$Time3, Censor=rep(as.numeric(1), nrow(Mote3Dead3)),Mote3Dead3 )

#Mote4
Mote4$Alive<-Mote4$TotalOutplants-Mote4$AllDead4
Mote4Alive1<-Mote4[rep(seq(nrow(Mote4)), as.numeric(Mote4$Alive)),] #make rows for each alive individual
Mote4Alive<-cbind(Time=Mote4Alive1$Time4, Censor=rep(as.numeric(0), nrow(Mote4Alive1)),Mote4Alive1 ) 
head(Mote4Alive1)

Mote4Dead1<-Mote4[rep(seq(nrow(Mote4)),as.numeric(Mote4$NewDead1)),] #make rows for each dead one
Mote4Dead1<-cbind(Time=Mote4Dead1$Time1, Censor=rep(as.numeric(1), nrow(Mote4Dead1)),Mote4Dead1 )
dim(Mote4Dead1)

Mote4Dead2<-Mote4[rep(seq(nrow(Mote4)),as.numeric(Mote4$NewDead2)),] #make rows for each dead one
Mote4Dead2<-cbind(Time=Mote4Dead2$Time2, Censor=rep(as.numeric(1), nrow(Mote4Dead2)),Mote4Dead2 )

Mote4Dead3<-Mote4[rep(seq(nrow(Mote4)),as.numeric(Mote4$NewDead3)),] #make rows for each dead one
Mote4Dead3<-cbind(Time=Mote4Dead3$Time3, Censor=rep(as.numeric(1), nrow(Mote4Dead3)),Mote4Dead3 )

Mote4Dead4<-Mote4[rep(seq(nrow(Mote4)),as.numeric(Mote4$NewDead4)),] #make rows for each dead one
Mote4Dead4<-cbind(Time=Mote4Dead4$Time4, Censor=rep(as.numeric(1), nrow(Mote4Dead4)),Mote4Dead4 )

#Mote5
Mote5$Alive<-Mote5$TotalOutplants-Mote5$AllDead5
Mote5Alive1<-Mote5[rep(seq(nrow(Mote5)), as.numeric(Mote5$Alive)),] #make rows for each alive individual
Mote5Alive<-cbind(Time=Mote5Alive1$Time5, Censor=rep(as.numeric(0), nrow(Mote5Alive1)),Mote5Alive1 ) 
head(Mote5Alive1)

Mote5Dead1<-Mote5[rep(seq(nrow(Mote5)),as.numeric(Mote5$NewDead1)),] #make rows for each dead one
Mote5Dead1<-cbind(Time=Mote5Dead1$Time1, Censor=rep(as.numeric(1), nrow(Mote5Dead1)),Mote5Dead1 )
dim(Mote5Dead1)

Mote5Dead2<-Mote5[rep(seq(nrow(Mote5)),as.numeric(Mote5$NewDead2)),] #make rows for each dead one
Mote5Dead2<-cbind(Time=Mote5Dead2$Time2, Censor=rep(as.numeric(1), nrow(Mote5Dead2)),Mote5Dead2 )
dim(Mote5Dead22)

Mote5Dead3<-Mote5[rep(seq(nrow(Mote5)),as.numeric(Mote5$NewDead3)),] #make rows for each dead one
Mote5Dead3<-cbind(Time=Mote5Dead3$Time3, Censor=rep(as.numeric(1), nrow(Mote5Dead3)),Mote5Dead3 )

Mote5Dead4<-Mote5[rep(seq(nrow(Mote5)),as.numeric(Mote5$NewDead4)),] #make rows for each dead one
Mote5Dead4<-cbind(Time=Mote5Dead4$Time4, Censor=rep(as.numeric(1), nrow(Mote5Dead4)),Mote5Dead4 )

Mote5Dead5<-Mote5[rep(seq(nrow(Mote5)),as.numeric(Mote5$NewDead5)),] #make rows for each dead one
Mote5Dead5<-cbind(Time=Mote5Dead5$Time5, Censor=rep(as.numeric(1), nrow(Mote5Dead5)),Mote5Dead5 )


MoteAll<-rbind(Mote1Alive, Mote2Alive, Mote3Alive, Mote4Alive, Mote5Alive, Mote1Dead, Mote2Dead1, Mote2Dead2, Mote3Dead1, Mote3Dead2, Mote3Dead3, Mote4Dead1, Mote4Dead2, Mote4Dead3, Mote4Dead4, Mote5Dead1, Mote5Dead2, Mote5Dead3, Mote5Dead4, Mote5Dead5)

head(Mote1Alive)

hist(MoteAll$Time)

#Plot of Mote survival by size.

Motefit <- survfit(Surv(as.numeric(Time), Censor) ~ Size, data = MoteAll)
N <- length(unique(MoteAll$Size))
plot(Motefit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "Mote Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(MoteAll$Size)),
        col=1:N,  lty=1)

MoteAll[which(MoteAll$Time>1000),]
sum(Mote$TotalOutplants[which(Mote$Size=="L")])
sum(Mote$AllDead5, na.rm = TRUE)

# FWC 1

FWC<-read.csv("Data/Take2FWC1.csv", na.strings=c(""," ","NA", "#NULL!"), stringsAsFactors=FALSE)
str(FWC)
dim(FWC)
FWCindividuals<-unique(FWC$tag_number)
length(FWCindividuals)
dim(FWC1Subset)
FWCAllData<-data.frame(stringsAsFactors=FALSE)

for(i in 1:length(unique(FWC$tag_number))){
  thisTag<-FWCindividuals[i]
  thisTagfirstRow <- min(which(FWC$tag_number == thisTag, arr.ind=TRUE))
  thisTagObs<-FWC$DaysAfterOutplanting[which(FWC$tag_number==thisTag)]
  lastObs<-max(FWC$DaysAfterOutplanting[which(FWC$tag_number==thisTag)])
  
  if(FWC$Status[which(FWC$DaysAfterOutplanting==lastObs & FWC$tag_number==thisTag)]=="live"){
    FWCrow<-c(EventDate=lastObs, Censor = "0", FWC[thisTagfirstRow,])
  } else{
    firstDead<-min(FWC$DaysAfterOutplanting[which(FWC$tag_number==thisTag & FWC$Status!="live" )])
    FWCrow<-c(EventDate=firstDead, Censor = "1", FWC[thisTagfirstRow,])
    
  }
  
  FWCAllData<-rbind(FWCAllData, FWCrow, stringsAsFactors=FALSE)
  
}
str(FWCAllData)
unique(FWCAllData$Size)
dim(FWC2AllData)
192+780
FWC1fit <- survfit(Surv(as.numeric(FWCAllData$EventDate), as.numeric(Censor)) ~ Size, data = FWCAllData)

#plot of FWC data by Size
N <- length(unique(FWCAllData$Size))-1
plot(FWC1fit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "FWC Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(FWCAllData$Size)),
        col=1:N,  lty=1)

#FWC 2
#This is a second dataset also provided by FWC
FWC2<-read.csv("Data/Take2FWC2.csv", na.strings=c(""," ","NA", "#NULL!"), stringsAsFactors=FALSE)

FWC2individuals<-unique(FWC2$Tag.ID)
length(FWC2individuals)
FWC2AllData<-data.frame(stringsAsFactors=FALSE)

thisTag<-"L25"
for(i in 1:length(unique(FWC2$Tag.ID))){
  thisTag<-FWC2individuals[i]
  thisTagfirstRow <- min(which(FWC2$Tag.ID == thisTag, arr.ind=TRUE))
  thisTagObs<-FWC2$Time[which(FWC2$Tag.ID==thisTag)]
  lastObs<-max(FWC2$Time[which(FWC2$Tag.ID==thisTag)])
  thisSize<-FWC2$Size[thisTagfirstRow]
  
  if(FWC2$Status[which(FWC2$Time==lastObs & FWC2$Tag.ID==thisTag)]=="alive"){
    FWC2row<-c(EventDate=lastObs, Censor = "0", FWC2[thisTagfirstRow,], Size2=thisSize)
  } else{
    firstDead<-min(FWC2$Time[which(FWC2$Tag.ID==thisTag & FWC2$Status!="alive" )])
    FWC2row<-c(EventDate=firstDead, Censor = "1", FWC2[thisTagfirstRow,],Size2=thisSize)
    
  }
  
  FWC2AllData<-rbind(FWC2AllData, FWC2row, stringsAsFactors=FALSE)
  
}
dim(FWC2AllData)
tail(FWC2AllData)
unique(FWC2AllData$Censor)
unique(FWC2AllData$Size[which(FWC2AllData$Censor==1)])

FWC2fit <- survfit(Surv(as.numeric(FWC2AllData$EventDate), as.numeric(Censor)) ~ Size2, data = FWC2AllData)

#plot of FWC2 data by size 

N <- length(unique(FWC2AllData$Size2))-1
plot(FWC2fit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "FWC2 Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(FWC2AllData$Size2)),
        col=1:N,  lty=1)


#sizeRegression}

#must convert FWC size class ranges to averages
FWCsize<-FWC
FWCsize$size_class_initial..TLE.cm.[which(FWCsize$size_class_initial..TLE.cm.=="5-10")]<-7.5
FWCsize$size_class_initial..TLE.cm.[which(FWCsize$size_class_initial..TLE.cm.=="10-20")]<-15
FWCsize$size_class_initial..TLE.cm.[which(FWCsize$size_class_initial..TLE.cm.=="20-50")]<-35
FWCsize$size_class_initial..TLE.cm.[which(FWCsize$size_class_initial..TLE.cm.=="50-100")]<-75
FWCsize$size_class_initial..TLE.cm.<-as.numeric(FWCsize$size_class_initial..TLE.cm.)

FWCsize$madWidth<-pmax(FWCsize$max_ht_initial..cm., FWCsize$max_width_initial..cm., na.rm=T)
unique(FWCsize$madWidth)

#Can get a linear fit of FWC TLE from max width
TLEfit <- lm(FWCsize$size_class_initial..TLE.cm.~FWCsize$madWidth)
summary(TLEfit)
x=seq(1,9360,1)
y=predict(TLEfit,newdata=list(x=seq(1,9360,1),  interval="confidence"))
plot(x,y,lwd=2)
length(FWCsize$size_class_initial..TLE.cm.)
plot( FWCsize$madWidth ,FWCsize$size_class_initial..TLE.cm.)
abline(-9.16687, 2.95931)
unique(FWCsize$madWidth)

plot(TLEfit)


#CRF

CRF3<-read.csv("Data/Take3CRF.csv",stringsAsFactors=FALSE)
CRF1<-CRF3[1:1215,]
CRF2<-CRF3[1216:1233,]
CRFAllData<-data.frame(stringsAsFactors=FALSE)

for(i in 1:length(CRF1$RowID)){
  CRFrow<-cbind(Time = CRF1$Time[i], Censor= CRF1$Censor[i], CRF1[i,])
  CRFAllData<-rbind(CRFAllData, CRFrow, stringsAsFactors=FALSE)
}
names(CRFAllData)
CRFindividuals2<-unique(CRF2$CoralID)
for(i in 1:length(CRFindividuals2)){
  thisTag<-CRFindividuals2[i]
  thisTagfirstRow <- min(which(CRF2$CoralID == thisTag, arr.ind=TRUE))
  thisTagObs<-CRF2$Time[which(CRF2$CoralID==thisTag)]
  lastObs<-max(CRF2$Time[which(CRF2$CoralID==thisTag)])
  
  
  if(CRF2$Censor[which(CRF2$Time==lastObs & CRF2$CoralID==thisTag)]==0){
    CRF2row<-c(Time=lastObs, Censor = 0, CRF2[thisTagfirstRow,])
  } else{
    firstDead<-min(CRF2$Time[which(CRF2$CoralID==thisTag & CRF2$Censor!=0 )])
    CRF2row<-c(Time=firstDead, Censor = 1, CRF2[thisTagfirstRow,])
    
  }
  
  CRFAllData<-rbind(CRFAllData, CRF2row, stringsAsFactors=FALSE)
  
}

hist(CRFAllData$Time)

CRFfit <- survfit(Surv(as.numeric(Time), as.numeric(Censor)) ~ Size, data = CRFAllData)

N <- length(unique(CRFAllData$Size))
plot(CRFfit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "CRF Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(CRFAllData$Size)),
        col=1:N,  lty=1)


CRF<-read.csv("Data/Take2CRF.csv", na.strings=c(""," ","NA", "<NA>", ".", "#N/A", "Not recorded", "not recorded", "Unknown", "Not in outplanting database"), stringsAsFactors=FALSE )
str(CRF)
CRF$NewDead<-0
head(CRF)
length(unique(CRF$ClusterID))


CRF1<-subset(CRF, CRF$MultipleRecords=="N")
CRF2<-subset(CRF, CRF$MultipleRecords=="Y")

#CRF1
CRF1$Obs<-1
CRF1Alive<-CRF1[rep(seq(nrow(CRF1)), as.numeric(CRF1$Alive)),]
CRF1Alive2<-cbind(EventDate=CRF1Alive$Date.Monitored, Censor=rep(as.numeric(0), nrow(CRF1Alive)),CRF1Alive)

CRF1Dead<-CRF1[rep(seq(nrow(CRF1)), CRF1$TotalDead),]
CRF1Dead2<-cbind(EventDate=CRF1Dead$Date.Monitored, Censor=rep(as.numeric(1), nrow(CRF1Dead)),CRF1Dead)

CRFAll<-rbind(CRF1Alive2,CRF1Dead2)

#CRF2
CRF2<-cbind(CRF2, Obs=ave(as.numeric(CRF2$ClusterID), CRF2$ClusterID, FUN=seq_along))

library(data.table)
clusters<-unique(CRF2$ClusterID)


for(i in 1:length(CRF2$ClusterID)){
  thisCluster<-CRF2[which(CRF2$ClusterID==clusters[i]),]
  
  CRF2Alive<-thisCluster[rep(seq(nrow(thisCluster)), as.numeric(thisCluster$Alive[which.max(thisCluster$Obs)])),]
  CRF2Alive2<-cbind(EventDate=CRF2Alive$Date.Monitored, Censor=rep(as.numeric(0), nrow(CRF2Alive)),CRF2Alive)
  
  for(j in 1:max(thisCluster$Obs)){
    if(j==1){
      thisCluster$NewDead[j]<-thisCluster$TotalDead[j]
    } else {
      thisCluster$NewDead[j]<-thisCluster$TotalDead[j]-thisCluster$TotalDead[j-1]
    }
  }
  
  CRF2Dead<-thisCluster[rep(seq(nrow(thisCluster)), thisCluster$TotalDead[j]),]
  CRF2Dead2<-cbind(EventDate=CRF2Dead$Date.Monitored, Censor=rep(as.numeric(1), nrow(CRF2Dead)),CRF2Dead) 
  CRFAll<- rbind(CRFAll, CRF2Alive2, CRF2Dead2)
}

CRFAll$Time<-CRFAll$EventDate-CRFAll$Date.Outplanted

dim(CRFAll)

CRFfit <- survfit(Surv(as.numeric(Time), Censor) ~ Size, data = CRFAll)

#plot CRF survival by size

N <- length(unique(CRFAll$Size))-1
plot(CRFfit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "CRF Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(CRFAll$Size)),
        col=1:N,  lty=1)

sum(Mote$TotalOutplants[which(Mote$Size=="L")])
sum(Mote$TotalOutplants[which(Mote$Size=="M")])


#University of Miami

UM<-read.csv("Data/UMOutplanting.csv", na.strings = c("UNK"), stringsAsFactors=FALSE)
UM$Size[UM$X100E..c50.<16]<-"S"
UM$Size[UM$X100E..c50.>15&UM$X100E..c50<51]<-"M"
UM$Size[UM$X100E..c50.>50]<-"L"


#for 1-248
head(UM$Genotype)
UMindividuals<-unique(UM$CoralID)
length(UMindividuals)
UM$CoralID[which(duplicated(UM$CoralID))]
UMAllData<-data.frame(stringsAsFactors=FALSE)

for(i in 1:248){
  UM$LE..cm.1[is.na(UM$LE..cm.1[i])]<- -2
  UM$LE..cm.2[is.na(UM$LE..cm.2[i])]<- -2
  UM$LE..cm.3[is.na(UM$LE..cm.3[i])]<- -2
  
  if(UM$LE..cm.4[i] != -1){
    thisCensor <- 0
    thisTime<-UM$Date.of.monitoring4[i]-UM$Outplanting.Date[i]
  } else if (UM$LE..cm.1[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring1[i]-UM$Outplanting.Date[i]
    
  }else if (UM$LE..cm.2[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring2[i]-UM$Outplanting.Date[i]
    
  }else if (UM$LE..cm.3[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring3[i]-UM$Outplanting.Date[i]
    
  }else if (UM$LE..cm.4[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring4[i]-UM$Outplanting.Date[i]
    
  }
  
  UMrow<- c(Time=thisTime, Censor = thisCensor, UM[i,])
  UMAllData<-rbind(UMAllData, UMrow, stringsAsFactors=FALSE)
}

for(i in 250:847){
  UM$LE..cm.1[is.na(UM$LE..cm.1[i])]<- -2
  UM$LE..cm.2[is.na(UM$LE..cm.2[i])]<- -2
  if(UM$LE..cm.3[i] != -1){
    thisCensor <- 0
    thisTime<-UM$Date.of.monitoring3[i]-UM$Outplanting.Date[i]
  } else if (UM$LE..cm.1[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring1[i]-UM$Outplanting.Date[i]
    
  }else if (UM$LE..cm.2[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring2[i]-UM$Outplanting.Date[i]
    
  }else if (UM$LE..cm.3[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring3[i]-UM$Outplanting.Date[i]
    
  }
  
  UMrow<- c(Time=thisTime, Censor = thisCensor, UM[i,])
  UMAllData<-rbind(UMAllData, UMrow, stringsAsFactors=FALSE)
}


for(i in 848:1741){
  UM$LE..cm.1[is.na(UM$LE..cm.1[i])]<- -2
  if(UM$LE..cm.2[i] != -1){
    thisCensor <- 0
    thisTime<-UM$Date.of.monitoring2[i]-UM$Outplanting.Date[i]
  } else if (UM$LE..cm.1[i]== -1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring1[i]-UM$Outplanting.Date[i]
    
  }else if (UM$LE..cm.2[i]== -1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring2[i]-UM$Outplanting.Date[i]
    
  }
  
  UMrow<- c(Time=thisTime, Censor = thisCensor, UM[i,])
  UMAllData<-rbind(UMAllData, UMrow, stringsAsFactors=FALSE)
}

for(i in 1742:1822){
  if(UM$LE..cm.1[i] != -1){
    thisCensor <- 0
    thisTime<-UM$Date.of.monitoring1[i]-UM$Outplanting.Date[i]
  } else if (UM$LE..cm.1[i]==-1){
    thisCensor <- 1
    thisTime<-UM$Date.of.monitoring1[i]-UM$Outplanting.Date[i]
    
  }
  
  UMrow<- c(Time=thisTime, Censor = thisCensor, UM[i,])
  UMAllData<-rbind(UMAllData, UMrow, stringsAsFactors=FALSE)
}
tail(UMAllData)

UMfit <- survfit(Surv(as.numeric(Time), Censor) ~ Size, data = UMAllData)

#plot of NSE survival by size
N <- length(unique(UMAllData$Size))
plot(UMfit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "U Miami Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(UMAllData$Size)),
        col=1:N,  lty=1)

dim(UMAllData)
UMAllData[which(UMAllData$Time>300&UMAllData$Size=="L"),]


# Nova South Eastern

NSE<-read.csv("Data/Take2NSE.csv", na.strings=c(""," ","NA", "#NULL!"), stringsAsFactors=FALSE)
NSE$Censor[NSE$Outcome =="Alive"]<- 0
NSE$Censor[NSE$Outcome !="Alive"]<- 1
head(NSE)

NSEfit <- survfit(Surv(as.numeric(Survival.days), as.numeric(Censor)) ~ Size, data = NSE)

#plot of NSE survival by size
N <- length(unique(NSE$Size))
plot(NSEfit, conf.int = T, xlab="Time (days)",ylab="Survival Probability", mark.time = F, main = "NSE Survival Time by Size",col=1:N)
legend( "bottomleft", legend=sort(unique(NSE$Size)),
        col=1:N,  lty=1)




# Combine all survival datasets

CRFSubset<-subset(CRFAllData, select=c("NameReef", "Censor", "Time", "Latitude", "Longitude", "Size", "Genotype"))
CRFSubset$Method<-"e"
CRFSubset$Agency<-"CRF"
CRFSubset<-CRFSubset[!is.na(CRFSubset$Latitude),]

TNCSubset<-subset(TNCAll, select=c("Site", "Censor", "Time", "Latitude", "Longitude", "Size", "Geno"))
TNCSubset$Site<-as.factor(TNCSubset$Site)
TNCSubset$Method<-"n"
TNCSubset$Agency <- "TNC"
TNCSubset$Geno<-as.character(TNCSubset$Geno)

MoteSubset<-subset(MoteAll, select=c("Site", "Censor", "Time", "Latitude", "Longitude", "Size", "Geno"))
MoteSubset$Geno<-as.character(MoteSubset$Geno)
MoteSubset$Method<-"n"
MoteSubset$Agency <- "Mote"


FWC1Subset<-subset(FWCAllData, select=c("outplant_site", "Censor", Time="EventDate", "Outplant_lat","Outplant_lon", "Size", "Genet"))
FWC1Subset$Method<-"n"
FWC1Subset$Agency<-"FWC"

FWC2Subset<-subset(FWC2AllData, select=c("Treatment", "Censor", Time="EventDate", "Latitude","Longitude", "Size", "Genet"))
FWC2Subset$Longitude<-FWC2Subset$Longitude*-1
FWC2Subset$Method<-"n"
FWC2Subset$Agency<-"FWC"



NSESubset<-subset(NSE, select=c("Site", "Censor", "Survival.days", "Latitude", "Longitude", "Size", "Genotype", "Treatment"))
NSESubset$Agency<-"NSE"

UMSubset<-subset(UMAllData, select=c("Site", "Censor", "Time", "Lat", "Long", "Size", "Genotype"))
UMSubset$Method<- "n"
UMSubset$Agency<-"UM"

dim(UMSubset)

names(CRFSubset)<-names(TNCSubset)<-names(MoteSubset) <- names(FWC1Subset)<-  names(FWC2Subset)<-names(NSESubset)<-names(UMSubset)<-c("Site", "Censor", "Time", "Latitude", "Longitude","Size",  "Genotype", "Method","Agency")

AllData<-data.frame()
AllData<-rbind(CRFSubset, TNCSubset, MoteSubset, FWC1Subset, FWC2Subset, NSESubset, UMSubset)


#Make a csv file for all the data

write.csv(AllData, file = "AlldataFL.csv")

#There was an error at one site, with no lat and long
AllData=read.csv("AlldataFL.csv")


newpoints<-SpatialPoints(cbind(AllData$Longitude, AllData$Latitude), CRS(longlat))
newRegions<- over(newpoints, regionslonglat[,"Region"])
newHabitat<- over(newpoints, reefTractlonglat[,"Zone"])

AllData2<- cbind(AllData,newRegions, newHabitat)
AllData3<-AllData2[!is.na(AllData2$Region) & !is.na(AllData2$Zone),]

AllData3$Zone[which(AllData3$Zone == "Bank/Shelf Escarpment")]<-"Bank/Shelf"
AllData3$Censor<-as.numeric(AllData3$Censor)
AllData3$Time<-as.numeric(AllData3$Time)
AllData3$Site<-as.factor(AllData3$Site)
AllData3$Size<-as.factor(AllData3$Size)
AllData3$Genotype<-as.factor(AllData3$Genotype)
AllData3$Agency<-as.factor(AllData3$Agency)
AllData3$Method<-as.factor(AllData3$Method)

#calculate the number of unique genotypes at each outplant site
library(plyr)
NumGenotypesvec<-ddply(AllData3,~Site,summarise,NumGenotypes=length(unique(Genotype)))
AllData3$NumGenotypes <- NumGenotypesvec$NumGenotypes[match(AllData3$Site, NumGenotypesvec$Site)]
AllData3$NumGenotypes2[AllData3$NumGenotypes>60]<-"VeryHigh>60"
AllData3$NumGenotypes2[AllData3$NumGenotypes<60 &AllData3$NumGenotypes>20]<-"High21-60"
AllData3$NumGenotypes2[AllData3$NumGenotypes<21 &AllData3$NumGenotypes>10]<-"Medium11-20"
AllData3$NumGenotypes2[AllData3$NumGenotypes<11&AllData3$NumGenotypes>5]<-"Low6-10"
AllData3$NumGenotypes2[AllData3$NumGenotypes<6]<-"VeryLow<6"
dev.off()      

#See number of genotypes by site in a histogram
hist(AllData3$NumGenotypes)
head(AllData3)
(AllData3[which(AllData3$NumGenotypes>60),])

dim(AllData3)
length(unique(AllData3$Site))

cdata <- ddply(AllData3, c("Agency", "NumGenotypes2"), summarise,
               N    = max(NumGenotypes),
               mean = mean(Time),
               sd   = sd(Time),
               se   = sd / sqrt(N)
)
cdata
max(AllData3$NumGenotypes[which(AllData3$Agency=="FWC1"|AllData3$Agency=="FWC2")])

head(AllData3)
#AllData3[which(AllData3$Site==X)]
#max(AllData3$Time[which(AllData3$Site>1000)])/


write.csv(AllData3, file = "AlldataFL3.csv")
