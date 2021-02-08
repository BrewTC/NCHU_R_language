#PCA

datdir = "D:/新尖兵/學生講義_pdf檔/ch27_BTA_實作I/"
datfile = paste(datdir, 'project1_rice.dry.csv', sep='');datfile
dat= read.table(datfile, header = T,sep=','); dim(dat); dat[1:3,]

#step1 check & test correlation among 19 variables/phenotypes
x=dat[,-1];dim(x)
plot(x)
corr=matrix(1,19,19) #自己跟自己相關係數是1

for (i in 1:18){
  for (j in (i+1):19){
    corr[i,j]=cor.test(x[,i],x[,j])$p.value  #右上三角
    corr[j,i]=cor.test(x[,i],x[,j])$est   #corr coef
  }
}
corr

#step2 normal test & outliers exam (沒做)
p.normal=c()
outliers=c()
for (i in 1:19){
  p.normal[i]=shapiro.test(x[,i])$p.value  #很多沒常態
  z=(x[,i]-mean(x[,i]))/sd(x[,i])
  outliers[i]=sum(1*(abs(z)>3.0)) #outliers個數
}
outliers
loc=which(p.normal<0.05)


#step3 PCA
pca = prcomp(x, scale=T)
summary(pca) # top k PCs account for 80% of the variance
pca$rotation # PC loadings
pca$x # scores
ev = pca$sdev^2; ev # eigenvalues
ev1 = as.numeric(ev);
ev2 = ev1[ev1>1]


par(mfcol=c(2,2))

#plot1 scree plot
plot(pca,ylim=c(0,5), type="lines",main='') # scree plot
points(ev2, col="red", pch=19)
abline(h=1, col="gray60", lty=3)
title('Scree plot',cex.main=1,font.main=1) #預設明明不錯看

#plot2 variance explained plot
var.explain=round(ev1/sum(ev1),3);var.explain ; sum(var.explain)
pp=rep(1:19,var.explain*1000);pp  #times
pp=factor(pp,labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
colour=c(rep('red',7),rep('grey60',12))

plot(pp,ylim=c(0,250),main='')#variance explained plot
for(i in 1:19){
  text((i+((i-1)/5)),summary(pp)[[i]]+12,paste(var.explain[i]*100,'%'),cex=0.8,col=colour[i])
  #真會調  #12是跟頂端的距離
title(paste('TOP',length(ev2),'PCs explained',sum(var.explain[1:7]*100),'% variance'),cex.main=1,font.main=1)
}


#plot3 PCA score plot
plot(pca$x , pch='.', main='PCA score plot',cex=2)

#plot4 biplot
biplot(pca,cex=0.3,main='biplot')#,var.axes=F 箭頭
abline(h=0, col="gray60", lty=3) # only select PCs having λ>1
abline(v=0, col="gray60", lty=3)

