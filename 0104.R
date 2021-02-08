#讀資料
# datdir = "D:/新尖兵/學生講義_pdf檔/dat/"
# datfile = paste(datdir, 'data1.txt', sep='')
# dat=read.table(datfile,head=T,sep='\t');dat

#標準差
# get.sd=function(x){
#   n=length(x)
#   x.bar=mean(x)
#   x.var=(sum(x^2)-n*x.bar^2)/(n-1)
#   x.sd=sqrt(x.var)
#   out=c(n,x.bar,x.var,x.sd)
#   names(out)=c('N','mean','var','sd')       #取名字
#   return (out)
# }
# 
# 
# dat.qc=function(x,cutpoints){
#   q=quantile(x,cutpoints)
#   x.new=x[x>q[[1]]&x<q[[2]]]   #取中間
#   out=get.sd(x=x.new)
#   return(out)
# }
# 
# x=1:100
# dat.qc(x,c(0.02,0.98))

# 圖
# weight=rnorm(100,55,5)
# height=rnorm(100,165,5)
# plot(weight,height,xlab="體重",ylab="身高")

#多變數
# sd.mv = function(dat){
#   n = nrow(dat)
#   x2 = dat^2
#   x2.sum = colSums(x2) # xi 平方和
#   x.bar = colMeans(dat) # xi 平均數 (同 colSums(dat)/n)
#   sd.dat = sqrt((x2.sum-n*x.bar^2)/(n-1))
#   return(list(ncol(dat), n, x.bar, sd.dat))
#   ## list(#vbles, #obs, mean, sd)
# }
# y = matrix(rnorm(12,10,2), nrow=4, ncol=3); y
# sd.mv(dat=y)






b = c(2,2);b

 
