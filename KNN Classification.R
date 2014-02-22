x1<-c(rnorm(100,5,2.5),rnorm(100,10,2),rnorm(100,15,3))
x2<-c(rnorm(100,5,2),rnorm(100,10,2),rnorm(100,5,2))
mygrid <- expand.grid(X1=seq(0,20, by=0.15),X2=seq(0,15, by=0.15))

NN <- function(){
  Neighbors<<-rep(0,nrow(mygrid))
  for (i in c(1:nrow(mygrid))) {
    distances<-(mygrid$X1[i]-x1)^2+(mygrid$X2[i]-x2)^2
    sort.distances<-sort.int(distances,index.return=TRUE)
    sort.indexes <- sort.distances$ix[1:15]
    sort.indexes[sort.indexes<=100]=0
    sort.indexes[sort.indexes<=200 &sort.indexes>100]=1
    sort.indexes[sort.indexes<=300 &sort.indexes>200]=2
    Neighbors[i] <- names(sort((table(sort.indexes)), decreasing=TRUE)[1])
  }
  Neighbors<<-Neighbors
}

myplotNN <- function () 
{
  plot(c(0,20),c(0,15),type="n",xlab="X1",ylab="X2")
  points(mygrid$X1[Neighbors==0],mygrid$X2[Neighbors==0],col="paleturquoise1")
  points(mygrid$X1[Neighbors==1],mygrid$X2[Neighbors==1],col="rosybrown1")
  points(mygrid$X1[Neighbors==2],mygrid$X2[Neighbors==2],col="palegreen")
  points(x1[201:300],x2[201:300],col="black",pch=20)
  points(x1[101:200],x2[101:200],col="violetred3",pch=20)
  points(x1[1:100],x2[1:100],col="mediumblue",pch=20)
}
NN()

myplotNN()
