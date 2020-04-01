set.seed(1000)  
x<-seq(-5,5,length.out=1000)  
y<-dt(x,1,0)  

plot(x,y,col="red",xlim=c(-5,5),ylim=c(0,0.5),type='l',  
     xaxs="i", yaxs="i",ylab='density',xlab='',  
     main="The T Density Distribution")  

lines(x,dt(x,5,0),col="green")  
lines(x,dt(x,5,2),col="blue")  
lines(x,dt(x,50,4),col="orange")  

legend("topleft",legend=paste("df=",c(1,5,5,50)," ncp=", c(0,0,2,4)), lwd=1, col=c("red", "green","blue","orange"))  