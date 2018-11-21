  #设定工作目录
setwd("C:/Users/Administrator/Desktop")
#加载相应的r包
library(quantmod)
library(xts)
library(urca)
library(TTR)
library("WindR", lib.loc="D:/R-3.4.1/library")
library(grDevices)
#读入数据
future<-read.csv("data.csv",header=TRUE)
class(future)
future<-xts(future[-1],order.by=as.Date(future[,1]))
head(future,2)
tail(future)

LLDPE<-future[,"LLDPE"]
PP<-future[,"PP"]

formStart<-"2014-07-01"
formEnd<-"2017-07-07"
formPeriod<-paste(formStart,"::",formEnd,sep ="")

LLDPE<-LLDPE[formPeriod]
PP<-PP[formPeriod]
length(LLDPE)
#####################################################
SSD<-function(x,y){ 
  retx<-ROC(x,type="discrete")[-1] 
  rety<-ROC(y,type="discrete")[-1]   
  standardX<-cumprod(1+retx) 
  standardY<-cumprod(1+rety) 
  SSD<-sum((standardX-standardY)^2) 
  return(SSD) 
}
PSdistance<-SSD(LLDPE,PP)
PSdistance


#######################################
LLDPElog<-log(LLDPE)

plogdf<- ur.df(coredata(LLDPElog),type='none',lags=1)
summary(plogdf)

retP<-diff(LLDPElog)[-1]
retpdf<- ur.df(coredata(retP),type='none',lags=1)
summary(retpdf)

PPlog<-log(PP)
Slogdf<- ur.df(coredata(PPlog),type='none',lags=1)
summary(Slogdf)

retS<-diff(PPlog)[-1]
retSdf<- ur.df(coredata(retS),type='none',lags=1)
summary(retSdf)

plot(PPlog,ylim=c(8,10),type="l",main="对数价格时序图")
points(LLDPElog,col="red",pch=".",lty=3)
legend("topright",legend=c("LLDPE","PP"),
       col=c("red","black"),pch=c(20,NA_integer_),lty=c(0,1))

plot.zoo(cbind(retP,retS),col=c("red","black"),lty=c(2,1),
         main="对数价格收益率")
legend("topright",legend=c("LLDPE","PP"),
       col=c("red","black"),lty=c(2,1),cex=0.7)

regPS<-lm(LLDPElog~PPlog)
regPS
summary(regPS)

alpha<-coef(regPS)[1]
alpha
beta<-coef(regPS)[2]
beta

spread <- LLDPElog-beta*PPlog-alpha
names(spread)<-"regression spread"
head(spread)
plot(spread,type="l",main="价差序列")

UnitRoot<-ur.df(spread,type="none")
summary(UnitRoot)


tradStart<-"2014-07-01"
tradEnd<-"2017-07-07"
tradPeriod<-paste(tradStart,"::",tradEnd,sep ="")
LLDPE<-LLDPE[tradPeriod]
PP<-PP[tradPeriod]


spreadCal<-function(x,y){
  retx<-ROC(x,type="discrete")[-1] 
  rety<-ROC(y,type="discrete")[-1] 
  standardX<-cumprod(1+retx)
  standardY<-cumprod(1+rety)
  spread<-standardX-standardY
  return(spread)
}

TradSpread<-spreadCal(LLDPE,PP)
summary(TradSpread)


#############################################################
mu<- mean(spread) 
sd<-sd(spread)

CoSpreadT<-log(LLDPE)-beta*log(PP)-alpha
names(CoSpreadT)<-"CoSpreadT"
summary(CoSpreadT)

plot(CoSpreadT,ylim=c(-0.25,0.25) ,
     main = "交易期价差序列(基于协整理论的跨品种期货套利策略)") 
abline(h = mu, col = "red", lwd =1) 
abline(h = mu + 0.9* sd, col = "blue", lty=6,lwd=2) 
abline(h = mu - 0.9* sd, col = "blue", lty=6,lwd=2)
abline(h = mu + 2* sd, col = "red", lty=1,lwd=1) 
abline(h = mu - 2* sd, col = "red", lty=1,lwd=1)


############################################################
library("PairTrading")

LLDPE<-future[,"LLDPE"]["2014::2017"] 
PP<-future[,"PP"]["2014::2017"]
prcdata<-merge(LLDPE,PP)

PSreg<-EstimateParameters(prcdata, method = lm)
PSreg
str(PSreg)  
plot(PSreg$spread,main="套利价差序列图")

BTreg<-EstimateParametersHistorically(prcdata,period=100, method = lm) 
str(BTreg)
head(na.omit(BTreg$hedge.ratio))
IsStationary(PSreg$spread, 0.05)
spread<-na.omit(BTreg$spread)
IsStationary(spread, 0.05) 

signal<-Simple(spread, spread.entry=0.01)
head(signal,300)
class(signal)

barplot(signal,space = 0, border = "red",
        xaxt="n",yaxt="n",xlab="",ylab="",main = "期货正反向套利区间分析", font.main = 4)
par(new=TRUE) 
plot(spread,main="")

pairReturn<-Return(prcdata,lag(signal,1),lag(na.omit(BTreg$hedge.ratio),1))
names(pairReturn)<-"pairReturn" 
head(pairReturn) 
library(PerformanceAnalytics)
charts.PerformanceSummary(pairReturn,main="LLDPE与PP期货统计套利交易绩效",geometric=FALSE) 
##################################################################################

library(urca) 
library(quantmod)
library(PerformanceAnalytics)

PP<-future[,"PP"]
LLDPE<-future[,"LLDPE"]

formStart<-"2014-10-01"
formEnd<-"2017-07-07"
formPeriod<-paste(formStart,"::",formEnd,sep ="")
PP<-PP[formPeriod]
LLDPE<-LLDPE[formPeriod]

log_LLDPE<-log(LLDPE)
summary(ur.df(log_LLDPE))
summary(ur.df(diff(log_LLDPE)[-1]))
log_PP<-log(PP)
summary(ur.df(log_PP))
summary(ur.df(diff(log_PP)[-1]))


regrePS<-lm(log_LLDPE~log_PP)
summary(regrePS)
alpha<-coef(regrePS)[1]
beta<-coef(regrePS)[2]
spreadf<-log_LLDPE-beta*log_PP-alpha
UnitRootf<-ur.df(spreadf,type="none")
summary(UnitRootf)
mu<- mean(spreadf) 
sd<-sd(spreadf)


LLDPE<-LLDPE["2014-10-01/2017-07-07"]
PP<-PP["2014-10-01/2017-07-07"]
CoSpreadT<-log(LLDPE)-beta*log(PP)-alpha
names(CoSpreadT)<-"CoSpreadT"
summary(CoSpreadT)


plot(CoSpreadT,ylim=c(-0.20,0.20) ,main = "交易期价差序列(协整套利)")
abline(h = mu,col="black",lwd =1)
abline(h = c(mu+0.2*sd,mu-0.2*sd),col = "blue",lty=6,lwd =2)
abline(h = c(mu+0.9*sd,mu-0.9*sd), col = "green",lty=2,lwd =2.5)
abline(h = c(mu+3.5*sd,mu-3.5*sd),col = "red",lty=3,lwd =3)
##############################################
level<-c(mu-3.5*sd,mu-0.8*sd,mu-0.25*sd,
         mu+0.25*sd, mu+0.8*sd,mu+3.5*sd)

interval<-function(x,level){
  prcLevel<-cut(x,breaks=c(-Inf,level,Inf))
  prcLevel<-as.numeric(prcLevel)-4
}

prcLevel<-interval(CoSpreadT,level)
prcLevel
head(prcLevel)

TradeSig<-function(prcLevel){
  n<-length(prcLevel)
  signal<-rep(0,n)
  for (i in (2:n)){
    if(prcLevel[i-1]==1 & prcLevel[i]==2 )
      signal[i]<-(-2)
    
    if(prcLevel[i-1]==1 & prcLevel[i]== 0)
      signal[i]<-2
    
    if(prcLevel[i-1]==2 & prcLevel[i]==3)
      signal[i]<-3
    
    if(prcLevel[i-1]==(-1)& prcLevel[i]==(-2))
      signal[i]<-1
    
    if(prcLevel[i-1]==(-1)& prcLevel[i]==0)
      signal[i]<-(-1)
    
    if(prcLevel[i-1]==(-2)& prcLevel[i]==(-3))
      signal[i]<-(-3)
  }
  return(signal)
}

signal<-TradeSig(prcLevel)
position<-c()
position[1]<-signal[1]
ns<-length(signal)
for (i in 2:ns){
  position[i]<-position[i-1]
  if (signal[i]==1)
    position[i]=1
  if (signal[i]==(-2))
    position[i]=(-1)
  if (position[i-1]==1 &signal[i]==(-1))
    position[i]=0
  if (position[i-1]==(-1) &signal[i]==(2))
    position[i]=0
  if (signal[i]==3) break
  if (signal[i]==-3) break
}

position<-xts(position,order.by=index(CoSpreadT))
position<- -position
tail(position)

#####

TradeSim<-function(PriceA,PriceB,Position){
  n<-length(Position)
  priceA<-as.numeric(PriceA)
  priceB<-as.numeric(PriceB)
  position<-as.numeric(Position)
  size<-1000
  shareA<-size*position
  shareB<-c()
  shareB[1]<-(-beta)*shareA[1]*priceA[1]/priceB[1]
  cash<-c()
  cash[1]<-2000
  for (i in 2:n){
    shareB[i]<-shareB[i-1]
    cash[i]<- cash[i-1]
    if(position[i-1]==0 & position[i]==1){
      shareB[i]<-(-beta)*shareA[i]*priceA[i]/priceB[i]
      cash[i]<-cash[i-1]-(shareA[i]*priceA[i]+shareB[i]*priceB[i])
    }
    if(position[i-1]==0&position[i]==(-1)){
      shareB[i]<-(-beta)*shareA[i]*priceA[i]/priceB[i]
      cash[i]<-cash[i-1]-(shareA[i]*priceA[i]+shareB[i]*priceB[i])
    }
    if(position[i-1]==1& position[i]==0){
      shareB[i]<-0
      cash[i]<-cash[i-1]+(shareA[i-1]*priceA[i]+shareB[i-1]*priceB[i])
    }
    if((position[i-1]==(-1))&(position[i]==0)){
      shareB[i]<-0
      cash[i]<-cash[i-1]+(shareA[i-1]*priceA[i]+shareB[i-1]*priceB[i])
    }
    
  }
  cash<-xts(cash,order.by=index(Position))
  shareA<-xts(shareA,order.by=index(Position))
  shareB<-xts(shareB,order.by=index(Position))
  asset<-cash+shareA*PriceA+shareB*PriceB
  account<-merge(Position,shareA,shareB,cash,asset)
  colnames(account)<-c("Position","shareA","shareB","cash","asset")
  return(account)
}

account<-TradeSim(LLDPE,PP,position)
tail(account)

plot.zoo(account[,c(1,4,5)],col=c("black","blue","red"), main="套利交易账户")



