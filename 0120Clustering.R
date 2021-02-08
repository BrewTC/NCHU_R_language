# Clustering
#實作1 水稻
datdir = "D:/新尖兵/學生講義_pdf檔/ch27_BTA_實作I/"
datfile = paste(datdir, 'project1_rice.dry.csv', sep='');datfile
dat= read.table(datfile, header = T,sep=','); dim(dat); dat[1:3,]
x=dat[,-1];dim(x)
plot(x)

#step1 search for clusters by MLE approach[unsupervised]
library(mclust)
fit=Mclust(x) #幹要跑多久
summary(fit)
names(fit)
plot(fit) 
pg=fit$G;pg #分幾群
cluster.MLE=fit$classification ; table(cluster.MLE)
cluster.MLE#跑出來是1跟2的東c(類別)

#step2 clustering by using kmeans[supervised]
fit1=kmeans(x,pg)
names(fit1)
cluster.kmeans=fit1$cluster;table(cluster.kmeans)
cluster.kmeans
fit1$betweenss
out=data.frame(dat,cluster.MLE,cluster.kmeans); out[1:3,];dim(out)
table(cluster.MLE,cluster.kmeans) 

#plot
par(mfcol=c(2,2))

# bivariate cluster plot
library(cluster)
clusplot(x,cluster.kmeans,color=T,shade=T,lines=0,main='clustering by k-means')
clusplot(x,cluster.MLE,color=T,shade=T,lines=0,main='clustering by MLE')

# discriminant projection plot
library(fpc)
plotcluster(x,cluster.kmeans,main='clustering by k-means',xlab='Discriminant coordinate 1',ylab='Discriminant coordinate 2')
plotcluster(x,cluster.MLE,main='clustering by MLE',xlab='Discriminant coordinate 1',ylab='Discriminant coordinate 2')


#實作2 毛豆

datdir = "D:/新尖兵/學生講義_pdf檔/ch28_BTA_實作II/"
datfile = paste(datdir, 'project2_impuData.csv', sep='');datfile
dat= read.table(datfile, header = T,sep=','); dim(dat); dat[1:3,]
x=dat[,-1];dim(x);x[1:5,]
# con=cbind(x[,1],x[,2],x[,3],x[,7],x[,8],x[,13],x[,14],x[,17]);con[1:5,]
# cat=cbind(x[,4],x[,5],x[,6],x[,9],x[,10],x[,11],x[,12],x[,15],x[,16],x[,18],x[,19]) ;cat[1:5,]
plot(x)

loc.con=c(1:3,7:8,13,14,17)
loc.cat=c(4:6,9:12,15:16,18:19)
con=x[,loc.con];con[1:5,]
cat=x[,loc.cat];cat[1:5,]

library(kamila)
fit2=wkmeans(conData=con,catData =cat, conWeight=0.5, nclust=5)
names(fit2)
cluster.wkmeans=fit2$cluster
table(fit2$cluster)
fit2$betweenss/fit2$totss #解釋

par(mfcol=c(1,2))
clusplot(x,cluster.wkmeans,color=T,shade=T,lines=0,main='clustering by weighted k-means')
plotcluster(x,cluster.wkmeans,main='clustering by weighted k-means',xlab='Discriminant coordinate 1',ylab='Discriminant coordinate 2')

s=1 ; k=c();R2=c();H=c();Nei=c();out=c()
for (i in 2:12){  #每個歧異度  #從2群開始
  # s=1;i=3
  fit=wkmeans(conData=con,catData =cat, conWeight=0.5, nclust=i)
  
  pp=table(fit$cluster)/nrow(x);pp
  k[s]=length(pp) ;k
  
  R2[s]=round((fit$betweenss/fit$totss)*100,2) ;R2
  H[s]= -sum(pp*log(pp))/log(k[s]);H
  Nei[s]=1-sum(pp^2);Nei
  out=data.frame(k,R2,H,Nei);out
  s=s+1
}
out

