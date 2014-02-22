require(MASS)
x<-mvrnorm(10,c(5, 5),matrix(c(1,0,0,2),2,2))
y<-mvrnorm(10,c(10, 10),matrix(c(1,0,0,3),2,2))
z<-mvrnorm(10,c(15, 15),matrix(c(2,0,0,2),2,2))


for (i in 1:100 ) {
  x_rand <- rbind(x[sample(1:nrow(x),size = 2,)])
  y_rand <- rbind(y[sample(1:nrow(x),size = 2,)])
  z_rand <- rbind(z[sample(1:nrow(x),size = 2,)])
  x1[i]<-rnorm(1,x_rand[,1],1)
  x2[i]<-rnorm(1,x_rand[,2],2)
  y1[i]<-rnorm(1,y_rand[,1],1)
  y2[i]<-rnorm(1,y_rand[,2],3)
  z1[i]<-rnorm(1,z_rand[,1],2)
  z2[i]<-rnorm(1,z_rand[,2],2)
}

mygrid <- expand.grid(X1=seq(0,20, by=0.1),X2=seq(0,20, by=0.1))

BC <- function()
{ classifier<<-rep(0,nrow(mygrid))
  for (i in c(1:nrow(mygrid))) {
    a<-(exp(-(mygrid$X1[i]-x_rand[,1])^2/2 - (mygrid$X2[i]-x_rand[,2])^2/8))
    b<-(exp(-(mygrid$X1[i]-y_rand[,1])^2/2 - (mygrid$X2[i]-y_rand[,2])^2/18))
    c<-(exp(-(mygrid$X1[i]-z_rand[,1])^2/8 - (mygrid$X2[i]-z_rand[,2])^2/8))
    classifier[i]<-order(c(a,b,c))[3]
  }
  classifier<<-classifier
}

myplotBC <- function () 
{
  plot(c(0,20),c(0,20),type="n",xlab="X1",ylab="X2")
  points(mygrid$X1[classifier==1],mygrid$X2[classifier==1], col='paleturquoise1')
  points(mygrid$X1[classifier==2],mygrid$X2[classifier==2], col='rosybrown1')
  points(mygrid$X1[classifier==3],mygrid$X2[classifier==3], col='palegreen')
  points(z1[1:100],z2[1:100],col="black",pch=20)
  points(y1[1:100],y2[1:100],col="violetred3",pch=20)
  points(x1[1:100],x2[1:100],col="mediumblue",pch=20)
}
BC()
myplotBC()