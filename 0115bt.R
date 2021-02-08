#Bootstrap
#Ex4
get.CI.pp.bt = function(p, n, B, alpha) {
  #---- generate the original sample ----
  n=100 ;p=0.52;B=1000;alpha=0.05
  dat = rep(c(0,1),c(n-p*n,p*n)); table(dat)  #1重複52次
  
  #---- bootstrapping ----
  pp.bt = c()
  for (i in 1:B) {
    pp.bt[i] = sum(sample(dat, size=n, replace=T))/n
    pp.bt
  }
  #---- get bootstrap distribution of pp, SE, CI ----
  m.bt = mean(pp.bt); m.bt
  se.bt = sd(pp.bt); se.bt
  a = qnorm(alpha/2, lower.tail=F); a  #Z
  ci.bt = c(p-a*se.bt, p+a*se.bt); ci.bt
  return(list(p, pp.bt, m.bt, se.bt, alpha, ci.bt))
}

out=get.CI.pp.bt(0.52,100,B=500,alpha=0.05)
p.true = unlist(out[1]); pp.true
pp.bt = unlist(out[2]); pp.bt
m.bt = unlist(out[3]); m.bt
se.bt = unlist(out[4]); se.bt
alpha = unlist(out[5]); alpha
ci.bt = unlist(out[6]); ci.bt

paste((1-alpha)*100, '% CI for proportion is between ', 
      round(ci.bt[1],2), ' and ', round(ci.bt[2],2), sep='')

hist(pp.bt, breaks=100)


#EX5
get.CI.ppDiff.bt = function(p1, p2, n, B, alpha) {
  #---- generate the original sample ----
  dat1 = rep(c(0,1),c(n-p1*n,p1*n)); table(dat1)
  dat2 = rep(c(0,1),c(n-p2*n,p2*n)); table(dat2)
  #---- bootstrapping ----
  pp.bt1 = c(); pp.bt2 = c()
  for (i in 1:B) {
    pp.bt1[i] = sum(sample(dat1, size=n, replace=T))/n
    pp.bt2[i] = sum(sample(dat2, size=n, replace=T))/n
  }
  #---- get bootstrap distribution of pp, SE, CI ----
  m.bt1 = mean(pp.bt1); m.bt2 = mean(pp.bt2); m.bt1; m.bt2
  ppDiff.bt = m.bt1-m.bt2; ppDiff.bt
  se.bt = sqrt(m.bt1*(1-m.bt1)/n + m.bt2*(1-m.bt2)/n); se.bt #辣個公式
  a = qnorm(alpha/2, lower.tail=F); a
  ci.bt = c((p1-p2)-a*se.bt, (p1-p2)+a*se.bt); ci.bt
  return(list(p1-p2, pp.bt1, pp.bt2, ppDiff.bt, se.bt, alpha, ci.bt))
}
out=get.CI.ppDiff.bt(p1=0.79,p2=0.38,n=1000,B=500,alpha=0.05);out
ppDiff.true=unlist(out[1]);ppDiff.true
pp.bt1=unlist(out[2]);pp.bt1
pp.bt2=unlist(out[3]);pp.bt2
ppDiff.bt=unlist(out[4])
se.bt=unlist(out[5])
alpha=unlist(out[6])
ci.bt=unlist(out[7])

paste((1-alpha)*100,'%CI for difference in proportions is between ',round(ci.bt[1],2),' and ',round(ci.bt[2],2),sep='')
hist(pp.bt1-pp.bt2,breaks=100)


#Exercise1

get.CI.pp.bt = function(p, n, B, alpha) {
  #---- generate the original sample ----
  # n=100 ;p=1/6;B=1000;alpha=0.05
  dat = rep(c(0,1),c(n-p*n,p*n)); table(dat)  #1是紅髮
  
  #---- bootstrapping ----
  pp.bt = c()
  for (i in 1:B) {
    pp.bt[i] = sum(sample(dat, size=n, replace=T))/n
  }
  #---- get bootstrap distribution of pp, SE, CI ----
  m.bt = mean(pp.bt); m.bt
  se.bt = sd(pp.bt); se.bt
  a = qnorm(alpha/2, lower.tail=F); a  #Z
  ci.bt = c(max(0,(p-a*se.bt)), p+a*se.bt); ci.bt  #避免CI下界是負的
  return(list(p, pp.bt, m.bt, se.bt, alpha, ci.bt))
}

out=get.CI.pp.bt(p=1/6,n=100,B=500,alpha=0.05)
pp.true = unlist(out[1]); pp.true
pp.bt = unlist(out[2]); pp.bt
m.bt = unlist(out[3]); m.bt
se.bt = unlist(out[4]); se.bt
alpha = unlist(out[5]); alpha
ci.bt = unlist(out[6]); ci.bt

paste((1-alpha)*100, '% CI for proportion is between ', 
      round(ci.bt[1],2), ' and ', round(ci.bt[2],2), sep='')

hist(pp.bt, breaks=100)

#Exercise3
#x+-95%CI = x+-1.96*SD 

#Exercise5 應該4這樣8
x=c(22, 32, 18, 54, 19, 43, 8, 28, 33)
get.CI.mean.bt=function(x,n=length(x),B,alpha){
  xbar=mean(x)
  xbar.bt=c();sd.bt=c()
  for(i in 1:B){
    x.bt=sample(x,size=n,replace=T)
    xbar.bt[i]=mean(x.bt)
    sd.bt[i]=sd(x.bt)}
  
  m.bt = mean(xbar.bt); m.bt
  se.bt = sd(sd.bt); se.bt
  a = qnorm(alpha/2, lower.tail=F); a  #Z
  ci.bt = c(max(0,(xbar-a*se.bt)), xbar+a*se.bt); ci.bt
  
  
  return(list(xbar,xbar.bt,sd.bt,m.bt,se.bt,alpha,ci.bt))
}

out=get.CI.mean.bt(x,B=500,alpha=0.05)
xbar = unlist(out[1]); 
xbar.bt = unlist(out[2]);
sd.bt = unlist(out[3]); sd.bt
m.bt = unlist(out[4]); m.bt
se.bt = unlist(out[5]); se.bt
alpha = unlist(out[6]); alpha
ci.bt = unlist(out[7]); ci.bt
paste((1-alpha)*100, '% CI for proportion is between ', 
      round(ci.bt[1],2), ' and ', round(ci.bt[2],2), sep='')

hist(xbar.bt)


#Ex6
datdir = "D:/新尖兵/學生講義_pdf檔/dat/"
datfile = paste(datdir, 'mathmark.dat', sep='')
dat= read.table(datfile, header = T); dim(dat); dat[1:3,]
mech = dat[,1]; vect = dat[,2]
alg = as.numeric(as.matrix(dat[,3])); anal = dat[,4]; stat = as.numeric(as.matrix(dat[,5]))

get.error.rate=function(y,x,method,p.train=NULL,k=NULL){
  n = length(y);res=c();error.rate=c()
  if (method=='LOOCV'){
    for (i in 1:n){
      fit=lm(y[-i]~x[-i])
      res[i]=y[i]-(fit$coef[[1]]+fit$coef[[2]]*x[i])}
    error.rate=sum(res^2)/n
 }
  if (method=='holdout'){
    ii=sample(1:n,size=floor(n*p.train),replace=F)
    y.train=y[ii];y.test=y[-ii];x.train=x[ii];x.test=x[-ii]
    fit=lm(y.train~x.train)
    res=y.test-(fit$coef[[1]]+fit$coef[[2]]*x.test)
    error.rate=sum(res^2)/n
}
  if (method=='kfold'){
    # iix=split(x,c(1:k));iiy=split(y,c(1:k))
    # for (i in 1:k){
    #   x.train[i]=iix[-i];y.train[i]=iiy[-i];y.test[i]=iiy[i];x.test[i]=iix[i]
    #   fit=lm(y.train~x.train)
    #   res[i]=y.test-(fit$coef[[1]]+fit$coef[[2]]*x.test)
    
    loc=sample(1:n,size=n,replace=F) #隨機
    s=1;t=0
    for (i in 1:k){
      if (i==k) ii=loc[s:n] else ii = loc[s:(t+floor(n/k))] #ii是前22個
      jj=setdiff(1:n,ii)
      y.train=y[jj];y.test=y[ii];x.train=x[jj];x.test=x[ii]
      fit=lm(y.train~x.train)
      res=y.test-(fit$coef[[1]]+fit$coef[[2]]*x.test)
      error.rate[i]=sum(res^2)/length(y.test)
      s=s+floor(n/k);t=t+floor(n/k)}#s=23 t=22
      error.rate=(sum(error.rate)/k)
  # return(sum(error.rate)/k)
    
  }

  return (error.rate)
  
}
get.error.rate(y=stat,x=alg,method='LOOCV')
get.error.rate(y=stat,x=alg,method='holdout',p.train=0.5)
get.error.rate(y=stat,x=alg,method='kfold',k=4)

# k=4
# o=c(1:103) ;o
# loc=split(o,c(1:k));loc
# for (i in 1:k){
#   ii[]
# }
# ii=loc[[1]];ii
# o[loc[[(k)]]]

