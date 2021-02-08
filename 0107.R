x1 = 1:10
x2 = 11:20
x3 = 21:30
## x 不是 data frame 格式 → 無法堆疊
x = cbind(x1,x2,x3); x; mode(x)
x.new = stack(x); x.new # 堆疊

## x 為 data frame 格式 → 可以堆疊
x = data.frame(x1,x2,x3); x
x.new = stack(x); x.new # 堆疊
x.ori = unstack(x.new); x.ori # 反堆疊

#merge
x1 = 1:10
x2 = sample(0:3, size=10, replace=T)
x3 = 21:30
x4 = c(rep(0, times=7), 1,2,8)
x5 = rnorm(10, mean=0, sd=1)

x = data.frame(x1,x2,x3);
y = data.frame(x4,x5);
names(x) = c('id','pid', 'score'); x
names(y) = c('id', 'si'); y

out1 = merge(x, y, by.x='id', by.y='id', all=T); out1; dim(out1) #依照id
out2 = merge(x, y, by.x='pid', by.y='id', all=T); out2; dim(out2)


#???
x = matrix(1:50, nrow=10, ncol=5)
x[,1] = sample(LETTERS[1:10], 10, replace=F)
y = matrix(runif(30), nrow=10, ncol=3)
y[,1] = sample(LETTERS[1:10], 10, replace=T, prob=c(0.1,0.1,0.1,0.1,0.1, 0.1,0.1,0.1,0.199,0.001))
## 資料整併
x = data.frame(x)
y = data.frame(y)
names(x) = c('id', 's1', 's2', 's3', 's4'); x
names(y) = c('id', 's5', 'si'); y
## data merge (by ‘id’)
out1 = merge(x, y, by.x='id', by.y='id', all=T); out1; dim(out1)
out2 = merge(x, y, by.x='id', by.y='id', all.x=T); out2; dim(out2)
out3 = merge(x, y, by.x='id', by.y='id', all.y=T); out3; dim(out3)
#找redundant
out3 = merge(x, y, by.x='id', by.y='id', all.y=T); out3; dim(out3)
id.new = as.character(as.matrix(out3[,1]))
tab = table(id.new); tab
id.red = names(tab)[which(tab>1)]; id.red #2個以上的
loc = which(id.new %in% id.red); loc
out3[loc,]


#smoke
scores = sample(15:90, size=50, replace=T); scores
gender = sample(c(1,2), size=50, replace=T); gender
smoke = sample(c(0,1), size=50, replace=T); smoke
x = data.frame(scores, gender, smoke); dim(x)
x
#split(scores,gender)$"1"
failed = x[scores<60,]; failed
pass = x[scores>=60,]; pass
## 及格 與 不及格 學生的姓別與抽煙關係聯合次數與百分比
tab = table(failed$gender, failed$smoke); tab; tab/sum(tab)
round(tab/sum(tab),2)
tab = table(pass$gender, pass$smoke); tab; tab/sum(tab)



#CH5 Loop


par(mfrow=c(2,2))
## 畫 4個 t 分配 (df分別為1, 2, 3, 4) 的直方圖
df = seq(from=1, to=4); df
for (i in df) {
  hist(rt(100, i), xlab='',main=paste('Student t (df=', i,')', sep=''))
  #從t分布隨機產生 df是自由度
}


#上三角形
x=matrix(0,nrow=5,ncol=5)
for (i in 1:nrow(x)){
  for (j in i:5){
    x[i,j]=i+j
  }
}
x
#函數化
get.tri.matrix=function(n,type){
  x=matrix(0,nrow=n,ncol=n)
  for (i in 1:nrow(x)){
    for (j in i:n){
      if(type=='up'){x[i,j]=i+j}
      else {x[j,i]=i+j}}}
  x
  }
get.tri.matrix(6,'up')

#上三角R下三角P
x1 = rnorm(100); x2 = rnorm(100, 5, 2); x3 = rt(100, 2)
x4 = rbeta(100, 2, 5); x5 = sample(1:100, 100, replace=T)
x = data.frame(x1,x2,x3,x4,x5); dim(x);x
out = matrix(NA, nrow=5, ncol=5); out
for (i in 1:5) {
  for (j in i:5) {
    out[i,j] = cor(x)[i,j] #cor是相關係數
    cor(x)#每個彼此的
   cor.test(x[,1], x[,4])
    out[j,i] = cor.test(x[,i], x[,j])$p.value  #幹這啥小
    out
  }
}
round(out, 3)


#CH6 Functions
# zzzz
#Ex8-2
group = c('g1','g1','g1','g2','g2','g2','g3','g3','g3','g3','g4','g4','g5')
score = c(60,70,88, 54,78,79, 62,68,90,81, 77,88, 92)
gid = unique(group); gid
s.mean = c(); s.max = c(); s.min = c()
j = 1
for (i in gid) {  #i是g1,g2...
  loc = which(group==i); loc
  s = score[loc]; s
  s.mean[j] = round(mean(s), 2)
  s.max[j] = max(s)
  s.min[j] = min(s)
  j = j + 1
}
out = data.frame(gid, s.mean, s.max, s.min); out

#Ex8-4 
get.N = function(n, f1, ...) {x = rnorm(n, 0, 1); return(f1(x, ...))}
get.t = function(n , f1, ...) { x = rt(n, 3); return(f1(x, ...)) }
get.chisq = function(n , f1, ...) {x=rchisq(n,2); return(f1(x, ...))}
get.F = function(n , f1, ...) { x = rf(n, 1, 4); return(f1(x, ...)) }

myfunc = function(f0, ...) {
  f0(...)
}

par(mfcol=c(2,2))
myfunc(f0=get.chisq, n=10000, f1=plot, col='red')
myfunc(get.chisq, n=10000, hist, breaks=100, main='N(0,1)')
myfunc(get.chisq, n=10000, boxplot)
myfunc(get.chisq, n=10000, qqnorm)
myfunc(get.N, n=10000, stem)


# CH7 R graphics
par(mai = c(0.1,0.1,0.1,0.1), mfrow = c(4,4)) #mai設定邊界
Color = c('coral2','deeppink1','lawngreen')
par(mfrow = c(3,3))
for (i in 1:9) {
  m = runif(1, max=5, min=0); m
  s = runif(1, max=3, min=0); s
  x = rnorm(200, mean=m, sd=s);x
  #main是title
  hist(x, col=Color[(i %% 3)+1], main=paste('Histogram of x', i, sep=''))
}


#符號
alpha = 0.5
x = rnorm(100); x
y = alpha*sqrt(abs(x)); y
hist(y, main=expression(alpha*sqrt(abs(x)))) #炫炮

#legend
p = runif(1000, max=1, min=0); p
s = -log(p, base=10); s
hist(s, xlab= "score", ylab= "安安", main=expression(-log[10](P)))
legend(locator(1), "GWAS p-value")  #自己點要放哪

#連續型畫散布圖
x=sample(1:100, 100, replace=T)
y=sample(1:100, 100, replace=T)
plot(x,y)

xy=data.frame(x,y)
xy1 = cbind(x,y)
par(mfcol=c(1,2))
plot(xy); plot(xy1)

#x為factor會畫長條圖
x=sample(1:4, 100, replace=T)
x = factor(x, labels=c(1,2,3,4)); x
plot(x)

#x1是factor會畫盒狀圖
x1 = factor(x, labels=c(1,2,3,4)); x1
y = sample(40:80, 100, replace=T)
par(mfcol=c(1,2))
plot(x, y); plot(x1,y)

#多變數
x = sample(1:4, 100, replace=T)
y = sample(40:80, 100, replace=T)
z = sample(1:99, 100, replace=T)
xyz = data.frame(x, y, z)
plot(xyz)
plot(~x+y+z)#跟樓上一樣


datdir = "D:/新尖兵/學生講義_pdf檔/dat/"
datfile = paste(datdir, 'mathmark.dat', sep='')
dat= read.table(datfile, header = T); dim(dat); dat[1:3,]
mech = dat[,1]; vect = dat[,2]
alg = dat[,3]; anal = dat[,4]; stat = dat[,5]
# paired 散佈圖
plot(~alg+stat+mech+vect+anal)

par(mfrow=c(2,2))
plot(stat~alg+mech+vect+anal) # ~前面作y

# 畫 statistics vs algebra 散佈圖
plot(alg, stat, main='Scatterplot', xlab='Algebra',ylab='Statistics')
abline(lm(stat~alg), col='red')  #回傳一整條線??
fit=lm(stat~alg);summary(fit)
fit$coe
abline(h=mean(stat),col='green')
abline(v=mean(alg), col='blue')

#隨機產生模擬線性xy
x = runif(100, max=1, min=-1)
par(mfcol=c(1,2))
#---- 高度相關 (群) ----
for (i in 1:length(x)) {
  if (x[i] <= -0.3) y[i] = runif(1, min=-0.9, max=-0.8)
  if (x[i]>-0.3 & x[i]<=0.3) y[i] = runif(1, min=-0.2, max=0.2)
  if (x[i]>0.3) y[i] = runif(1, min=0.8, max=0.9) }
plot(x, y, main='Scatterplot'); abline(lm(y~x), col='red') 
#更美ㄉ
for (i in 1:length(x)) {
  y[i] = runif(1, min=x[i]-0.1, max=x[i]+0.1) }
plot(x, y, main='Scatterplot'); abline(lm(y~x), col='red')


#條件圖
math = sample(40:80, size=100, replace=T); math
stat = sample(55:99, size=100, replace=T); stat
smk = sample(c('ns', 'es', 'cs'), size=100, replace=T); smk
drink = sample(c('never', 'wine', 'beer', 'vodka'), size=100,replace=T); drink
coplot(math~stat|smk, rows=1)


#qq系列
math = sample(40:80, size=100, replace=T); math
stat = sample(55:99, size=100, replace=T); stat
qqnorm(math); qqline(math)


