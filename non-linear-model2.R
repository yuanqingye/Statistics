# link address
# https://datascienceplus.com/second-step-with-non-linear-regression-adding-predictors/#load libraries
library(nlme)
#first try effect of treatment on logistic growth
Ks <- c(100,200,150) 
n0 <- c(5,5,6)
r <- c(0.15,0.2,0.15)
time <- 1:50
#this function returns population dynamics following
#a logistic curves
logF <- function(time,K,n0,r){
  d <- K * n0 * exp(r*time) / (K + n0 * (exp(r*time) - 1))
  return(d)
}
#simulate some data
dat <- data.frame(Treatment=character(),Time=numeric(),
                  Abundance=numeric())
for(i in 1:3){
  Ab <- logF(time = time,K=Ks[i],n0=n0[i],r=r[i])
  tmp <- data.frame(Treatment=paste0("T",i),Time=time,
                    Abundance=Ab+rnorm(time,0,5))
  #note that random deviates were added to the simulated 
  #population density values
  dat <-rbind(dat,tmp)
}
#the formula for the models
lF<-formula(Abundance~K*n0*exp(r*Time)/(K+n0*(exp(r*Time)-1)) | Treatment)

#fit the model
(m <- nlsList(lF,data=dat,start=list(K=150,n0=10,r=0.5)))

library(ggplot2)
#derive the predicted lines
dat$Pred <-predict(m)
#plot
ggplot(dat,aes(x=Time,y=Abundance,color=Treatment))+
  geom_point()+geom_line(aes(y=Pred))



#load libraries
library(bbmle)
library(reshape2)
#parameter for simulation
K <- 200
n0 <- 5
#the gradient in temperature
Temp <- ceiling(seq(0,20,length=10))


#simulate some data
mm <- sapply(Temp,function(x){ 
  rpois(50,logF(time = 1:50,K = K,n0=n0,r = 0.05 + 0.01 * x))})
#sample data from a poisson distribution with lambda parameter equal
#to the expected value, note that we do not provide one value for the
#parameter r but a vector of values representing the effect of temperature
#on r
#some reshaping
datT <- melt(mm)
names(datT) <- c("Time","Temp","Abund")
datT$Temp <- Temp[datT$Temp]

#fit the model
(mll <- mle2(Abund~dpois(K*n0*exp(r*Time)/(K+n0*(exp(r*Time)-1))),
             data=datT,parameters = list(r~Temp),
             start=list(K=100,n0=10,r=0.05)))

datT$pred <- predict(mll)
ggplot(datT,aes(x=Time,y=Abund,color=factor(Temp)))+
  geom_point()+geom_line(aes(y=pred))

#simulate some data
mm <- sapply(Temp,function(x){
  rpois(50,logF(time = 1:50,K = 100+20*x-x**2,n0=n0,r = 0.05 + 0.01 * x))})
#note that this time both K and r are given as vectors to represent their
#relation with temperature

#some reshaping
datT <- melt(mm)
names(datT) <- c("Time","Temp","Abund")
datT$Temp <- Temp[datT$Temp]

#the negative log-likelihood function
LL <- function(k0,k1,k2,n0,r0,r1){
  K <- k0 + k1*Temp + k2*Temp**2
  r <- r0 + r1*Temp
  lbd <- K*n0*exp(r*Time)/(K+n0*(exp(r*Time)-1))
  -sum(dpois(Abund,lbd,log=TRUE))
}

(mll2 <- mle2(LL,data=datT,start=list(k0=100,k1=1,k2=-0.1,n0=10,r0=0.1,r1=0.1)))

cc <- coef(mll2)
#sorry for the awful coding, to get the model predicted values we need
#to code the relation by hand which is rather clumsy ...
datT$pred <- melt(sapply(Temp,function(x) {logF(time=1:50,K=cc[1]+cc[2]*x+cc[3]*x**2,n0=cc[4],r=cc[5]+cc[6]*x)}))[,3]
ggplot(datT,aes(x=Time,y=Abund,color=factor(Temp)))+
  geom_point()+geom_line(aes(y=pred))
