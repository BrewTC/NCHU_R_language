#簡單隨機抽樣
x = sample(1:6, size=100, replace=T, prob=c(0.1,0.2,0.3,0.3,0.05,0.05));x
x = factor(x, levels=1:6); table(x)/sum(table(x))
table(x)

y=sample(x,size=10,replace=F);table(y);table(y)/sum(table(y))

#add bootstraping
get.sam.by.SRS=function(x,n.bt,n=10){
  y=c()
  for(i in 1:n.bt){
    y.bt=sample(x,size=n,replace=F)
    y=c(y,y.bt);y
  }
  y=factor(y,levels=levels(x))
  pp=table(y)/sum(table(y))
  return(list(y,pp))
}

get.sam.by.SRS(x,n.bt=1000,n=10)


#分層比例抽樣
x = sample(1:6, size=100, replace=T, prob=c(0.1,0.2,0.3,0.3,0.05,0.05));x
x = factor(x, levels=1:6); table(x)/sum(table(x))
levels(x)
length(x)
sum(table(x))
table(x)/sum(table(x))
#抽
ni=as.numeric(table(x));ni;pi=ni/100;pi
y=sample(levels(x),size=10,replace=T,prob=pi)
y=factor(y,levels=levels(x));table(y);table(y)/sum(table(y))

#BT抽
get.sam.by.SPS=function(x,n.bt,n=10){
  ni=as.numeric(table(x)/length(x));ni
  y=c()
  for(i in 1:n.bt){
    y.bt=sample(levels(x),size=10,replace=T,prob=ni)
    y=c(y,y.bt);y
  }
  y=factor(y,levels=levels(x))
  pp=table(y)/sum(table(y))
  return(pp)
}
get.sam.by.SPS(x,n.bt=10000,n=10)



#分層隨機抽樣

x = sample(1:6, size=100, replace=T, prob=c(0.1,0.2,0.3,0.3,0.05,0.05));x
x = factor(x, levels=1:6); table(x)/sum(table(x))
length(x)
#抽
y=c();samp=sample(c(2,2,2,2,1,1),size=6,replace=F)
#i是第幾層
for (i in 1:length(x)){
  y.sam=sample(levels(x)[i],size=samp[1],replace=T)
  y=c(y,y.sam)
}
y=factor(y,levels=levels(x));table(y);table(y)/sum(table(y))

#BT抽
get.sam.by.SSR=function(x,n.bt,n=10){
  y=c()
  for(i in 1:n.bt){
    samp=sample(c(2,2,2,2,1,1),size=6,replace=F)
    for (i in 1:6){
      y.bt=sample(levels(x)[i],size=samp[i],replace=T)
      y=c(y,y.bt)
    }
  }
  y=factor(y,levels=levels(x))
  pp=table(y)/sum(table(y))
  return(pp)
}
get.sam.by.SSR(x,n.bt=1000,n=10)
