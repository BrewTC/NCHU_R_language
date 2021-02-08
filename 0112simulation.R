#Ex1
u=runif(100)
x=1+1*(u>0.2)+1*(u>0.35)+1*(u>0.6);x
table(x)

get.u=function(n){
  u=runif(n)
  x=1+1*(u>0.2)+1*(u>0.35)+1*(u>0.6);x
  out=table(x)
  return (out/n*100)
}
get.u(1000)


pp.check=function(nsim){
  nsim=100
  pp=matrix(NA,nrow=nsim,ncol=4)
  for(i in 1:nsim){
    u=runif(100)
    # x=1+1*(u>0.2)+1*(u>0.35)+1*(u>0.6)
    x=ifelse(u<0.4,4,ifelse(u<0.65,3,ifelse(u<0.85,1,2)));table(x)
    pp[i,]=as.numeric(table(x));pp
  }
  out=colMeans(pp)
  names(out)=c('p1','p2','p3','p4')
  return (out)
}
pp.check(nsim=100)


#Ex3
x=runif(100)
n=100
#permute 10 times
n.perm=10
perm=matrix(NA,nrow=n,ncol=n.perm)
for (i in 1:n.perm){  #每次perm
  x
  pos.perm=sample(1:n,size=n,replace=F);pos.perm
  x[pos.perm]
  perm[,i]=x[pos.perm]
  
}
perm   #100*10
colSums(perm)


#Ex4 指數分布inverse
inv.trans=function(n,lamda){
  u=runif(n)
  x=-log(1-u)/lamda
  return(x)
}
x=inv.trans(n=1000,lamda=1);x
hist(x,main='simulated x',breaks=20)
y=rexp(n=1000,rate=1) #check
hist(y,breaks=20)


#Ex5
get.inv.df=function(nsim,lamda,theda){
  u=runif(nsim)
  y=-log(1-u)/lamda
  x=y/theda
  return (x)
}
z=get.inv.df(1000,1,1)
hist(z)


#Ex8 exp
get.rej.sim=function(nsim){
  x.rej=c();y.rej=c()
  for(i in i:nsim){
    t=-1
    while (t<0){  #1筆
      x=rexp(n=1,rate=1);x
      y=runif(n=1,min=0,max=exp(-x));y
      diff=(x^2*exp(-x))-y;diff
      if(x>1|diff<0) t=-1 else t=y
    }
    x.rej[i]=x;y.rej[i]=y
  }
  return (cbind(x.rej,y.rej))
}

x=get.rej.sim(1000)
hist(x[,1], main= 'Histogram of simulated data')
plot(x[,1],x[,2], main= 'Scatter of the simulated distribution')


get.rej.sim2=function(nsim){
  x.rej=c();y.rej=c()
  for(i in i:nsim){
    t=-1
    while (t<0){  #1筆
      x=runif(1);x
      y=runif(n=1,min=0,max=0.4);y
      diff=(x^2*exp(-x))-y;diff
      if(x>1|diff<0) t=-1 else t=y
    }
    x.rej[i]=x;y.rej[i]=y
  }
  return (cbind(x.rej,y.rej))
}

x=get.rej.sim2(10000)

hist(x[,1], main= 'Histogram of simulated data')
plot(x[,1],x[,2], main= 'Scatter of the simulated distribution')

#Exercise1
inv.trans=function(nsim){
  nsim=1000
  u=runif(nsim);u
  x=u^(1/nsim);x
  return(x)
}
x=inv.trans(nsim=1000);x
hist(x,breaks=20)
y=rexp(n=1000,rate=1) 
hist(y)


#Exercise2
get.inv.df=function(nsim,theda){
  u=runif(nsim)
  y1=log(1-u)/theda
  x=nsim*y1
  return(x)
}
x=get.inv.df(nsim=1000,theda=1)
hist(x)

qnorm(0.2,mean=0,sd=1)
