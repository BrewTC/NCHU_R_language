#矩陣解
A = matrix(1:4,nrow=2,ncol=2);A
b=c(2,2);b

get.beta=function(A,b){
  xx=t(A)%*%A;xx
  b.hat=solve(xx)%*%t(A)%*%b   #不需要知道怎麼推導的公式
  return (b.hat)
}
get.beta(A,b)

#level
x = c('Yes', 'No', 'No', 'Yes', 'Yes'); x
x1=as.factor(x); x1; mode(x1)   #以alphabetic排序
x2 = factor(x, levels=c('Yes', 'No')); x2; mode(x2) #先定義順序
levels(x2) = c('是', '否'); x2  #改level

#table
counts2 = table(x2); counts2
counts2[2]

#NSE
x1 = c('N', 'N', 'S', 'S')
x2 = c('S', 'E', 'S', 'S')
x3 = c('E', 'E', 'E', 'N')
x = data.frame(x1, x2, x3); x;
out=matrix(NA,ncol=3,nrow=3)
for (i in 1:3) { 
  for(j in 1:3){   #j也代表NSE
    y = factor(x[,i], levels=c('N','S','E'))
    out[i,j] = table(y)[[j]]    #存在第i列j行 #[[]]只取數字  
  }
}
out
rownames(out) = c('x1', 'x2', 'x3')
colnames(out) = c('N', 'S', 'E')
out


#ordered 排序大小
x = c('B','F','A','C','A','C','B','A','F','D'); x; mode(x)
x1 = factor(x); x1
x2 = ordered(x, levels=c('F','E','D','C','B','A')); x2
ii = (x2 >= 'B'); ii   #True or False
jj = which(x2>='B'); jj  #which找位置

#missing data
x = c(0, 1, 2, NA, 4, 0, NA); x
ii = is.na(x); ii
x[ii] = -9
x[is.na(x)] = -9
x

x = c(0, 1, 2, NA, 4, NA, NA)
ii = 1*(is.na(x)); ii  #回傳1&0
x[ii==1] = mean(x,na.rm=T)  #改-9再挑>=0的 or na.rm就好
x

#LOCF
x = c(0, 1, 2, NA, 4, NA, NA)
for (i in 1:length(x)) {
  if (is.na(x[i]) == T) x[i] = x[i-1] #後面是執行的東西
  #把missing換成它前面的值
}
x

#避免loop 欸這要跑兩次?
x = c(0, 1, 2, NA, 4, NA, NA)
x[is.na(x)] = -9; x
loc = which(x==-9); loc
x[loc] = x[loc-1]
x
#鬼之setdiff 
loc.rep=setdiff(loc-1,loc)
loc.rep1=max(setdiff(1:6,setdiff(loc-1,loc.rep)))  #找出連續missing的地方 1~6排除6 再找最大的(離7最近)
loc.LOCF=c(loc.rep,loc.rep1)
x[loc]=x[loc.LOCF]
x

#linear regression
x = runif(100, max=10, min=0)
y = runif(100, max=1, min=0)
loc.na = sample(1:100, 10, replace=F)
y[loc.na] = NA
dat = data.frame(x,y); dat; dim(dat)

temp = dat[-loc.na,]; dim(temp)  #砍掉missing
x.new = as.numeric(as.matrix(temp[,1]))  #萬用法則加就對了==
y.new = as.numeric(as.matrix(temp[,2]))
fit = lm(y.new~x.new); summary(fit)  #迴歸分析語法 X配飾Y?? 
names(fit)
fit$coe  #可偷懶
b0 = fit$coe[[1]]; b0
b1 = fit$coe[[2]]; b1
y[loc.na] = b0 + b1*x[loc.na] # y.new = b0 + b1*x.new
dat.impu = data.frame(x, y); dat.impu

#list
x1 = matrix(1:6, nrow=2, ncol=3)
x2 = 1:10
x3 = 1000
out = list(x1, x2, x3); out
out[[1]]
out[[1]][2,3]  #取矩陣

#dataframe
weight = c(150, 135, 210, 140)
height = c(65, 61, 70, 65)
gender = c('F', 'F', 'M', 'F')
dat = data.frame(weight, height, gender); d
data.frame(w=weight, h=height, g=gender)

#12鬼月
x = rnorm(300, mean=0, sd=1); x
y = matrix(x, nrow=100, ncol=3); y
z = ts(y, start=c(1961,1), frequency=12); z;dim(z)
plot(z)
#差分
z0 = z[-nrow(z),];z0;dim(z0) #砍掉最後row
z1 = z[-1,];z1;dim(z1)      #砍掉第一row
ds = z1-z0; ds[1:3,]; dim(ds) # 差分數列
plot(ds[,2]); abline(h=0, col=2)


x = 1:10; y = 5:20
ii = x %in% y; ii; x[ii] # x裡有沒有y
intersect(x, y)  #交集
unique(c(x,y))  #聯集

#which
x = c(-9,1:9, 99, 9:1); y = 5:20
loc1 = which(x %in% y); loc1; x[loc1]
loc2 = which(y %in% x); loc2; y[loc2]
loc3 = which(x==max(x)); loc3; x[loc3]

#table
x = c(-9,1:9, 99, 9:1); y = c(-99, 0, 5:20, 0, 10)
table(x,y)

#單變數
impu.uv = function(x) {
  #loc=which(is.na(x)==T)
  #x[loc] = mean(x[x>=0])
  x[is.na(x)] = mean(x,na.rm=T)   #這樣也可以吧
  # ii = 1*(is.na(x)); ii 
  # x[ii==1] = mean(x,na.rm=T)
  x.impu = x
  return(x.impu)
}
x = sample(c(0:10,NA), size=10, replace=T, prob=c(0.1, 0.05,0.1, 0.1, 0.1,0.1,0.1,0.1,0.05,0.05,0.05,0.3)); x
impu.uv(x)



#多變數
impu.mv = function(dat){
  vble.mean = colMeans(dat, na.rm=T) # 求各變數的平均值
  out = dat # 宣告新變數 (size 和 dat 相同)
  for (i in 1:ncol(dat)){ # loop 開始
    xi = dat[,i] # 讀取第 i 個變數
    xi[is.na(xi)] = -9; xi # 將遺漏值取代為 -9
    loc = which(xi==-9); loc # 找出遺漏值的位置
    out[loc,i] = round(vble.mean[i],2) # 遺漏值取代為平均值
  } 
  return(out)
}
x = sample(c(0:3,NA), size=30, replace=T,
           prob=c(0.1, 0.2, 0.2, 0.2, 0.3)); x
dat = matrix(x, nrow=10, ncol=3); dat
impu.mv(dat)


#剪刀石頭布
won.or.lost = function(x) {
  ssc = c('scissors', 'stone', 'cloth')
  while (length(intersect(x,ssc))==0){    #有交集長度1沒有0
    x=scan(file='',what='character',quiet=T,nlines=1)}  #自己輸入字串  #偵錯
  
  y = sample(ssc, 1); y
  if (x=='scissors' & y=='scissors') {out = 'equal!!'}
  if (x=='scissors' & y=='stone') {out = 'you lost!!'}
  if (x=='scissors' & y=='cloth') {out = 'you won!!'}
  if (x=='stone' & y=='scissors') {out = 'won!!'}
  if (x=='stone' & y=='stone') {out = 'equal!!'}
  if (x=='stone' & y=='cloth') {out = 'you lost!!'}
  if (x=='cloth' & y=='scissors') {out = 'you lost!!'}
  if (x=='cloth' & y=='stone') {out = 'you won!!'}
  if (x=='cloth' & y=='cloth') {out = 'equal!!'}  #484可以不用paste
  return(out)
  }
won.or.lost('scissdors')


#猜數字不想貼ㄌ

