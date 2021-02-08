#The MADe method
x=c(5,6,7,13,43,45,46,55,56,60,61,62,65,66,66,67,90,100,104,132)
get.MADe=function(x,k){
  # x=c(5,6,7,13,43,45,46,55,56,60,61,62,65,66,66,67,90,100,104,132);L=0;U=0;k=2
  MAD=median(abs(x-median(x)))
  MADe=1.483*MAD;MADe
  L=median(x)-k*MADe
  U=median(x)+k*MADe
  loc=which(x<L|x>U);loc
  if(length(loc)>0){
    temp=c()
    for (i in 1:length(loc)){
      temp=paste(temp,x[loc[i]])
    }
    out=paste('using',k,'MADe','outliers are',temp)}
  else out=paste('no outliers')
  return(out)
}

get.MADe(x,2)
# m=median(x);m
# M=abs(x-m);M
# MAD=median(M);MAD
# MADe=1.483*MAD;MADe
# 
# m-2*MADe;m+2*MADe
# m-3*MADe;m+3*MADe

shapiro.test(x)
z = (x-mean(x))/sd(x); round(z,2)

#Ex6
y=c(199.31,199.53,200.19,200.82,201.92,201.95,202.18,245.57)
y=c(5,6,7,13,43,45,46,55,56,60,61,62,65,66,67,90,100,104,132)
Grubbs=function(y){
  s=2;temp1=c();temp2=c()
  
  while(s==2){
    Gmax=(max(y)-mean(y))/sd(y);Gmax
    n=length(y);n
    cutoff=(n-1)/sqrt(n)*sqrt((qt(0.05/n,n-2))^2/(n-2+qt(0.05/n,n-2)^2));cutoff
    if(Gmax>cutoff){
      temp1=paste(temp1,y[n])
      y=y[-n];y}
    else s=1}
  
  while(s==1){
    Gmin=(mean(y)-min(y))/sd(y)
    n=length(y);n
    cutoff=(n-1)/sqrt(n)*sqrt((qt(0.05/n,n-2))^2/(n-2+qt(0.05/n,n-2)^2));cutoff
    if(Gmin>cutoff){
      temp2=paste(temp2,y[1])
      y=y[-1]}
    else s=0}
  return(paste('outliers are',temp1,temp2))
}

Grubbs(y)

