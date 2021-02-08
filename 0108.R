#長條圖
x = sample(c('AA', 'Aa', 'aa'), size=100, replace=T,prob=c(0.35,0.55,0.10)); table(x)
y = sample(c('CA', 'CN'), size=100, replace=T,prob=c(0.4,0.6)); table(y)
counts = table(y, x); counts

par(mfcol=c(1,2))
barplot(counts, main="Genotypes distribution by MS",
        xlab="Genotypes", col=c("blue","red"),
        legend=rownames(counts))
barplot(counts, beside=T,
        main="Genotypes distribution by MS",
        xlab="Genotypes", col=c("blue","red"),
        legend=rownames(counts)) 

#盒鬚圖
datdir = "D:/新尖兵/學生講義_pdf檔/dat/"
datfile = paste(datdir, 'mathmark.dat', sep='')
dat= read.table(datfile, header = T); dim(dat); dat[1:3,]
mech = dat[,1]; vect = dat[,2]
alg = dat[,3]; anal = dat[,4]; stat = dat[,5]
x = data.frame(mech, vect, alg, anal, stat); dim(x)
## 畫 所有變數 的盒狀圖
boxplot(x, col="yellow", main="Uni scores")
summary(x) 


#線型圖
datfile = paste(datdir, 'dep.dat', sep='')
dep = read.table(datfile);dep
M = max(dep); N = ncol(dep)
plot(c(1,12), c(-1,M), type='n', xlab='Month', ylab='Frequency')
for (i in 1:N){  #每個column是一條線
  lines(dep[,i], lwd=0.5, col=i)  #是color
}
legend(locator(1), lty=1, lwd=2, ncol=5, cex=1,
       c('D1', 'D2', 'D3', 'D4', 'D5', 'D6', 'D7', 'D8', 'D9', 'D10'),
       col=c(1,2,3,4,5,6,7,8,9,10)) 

#圓餅圖
slices = c(13, 12, 1, 17, 8)
lbls = c('US', 'UK', 'Australia', 'Germany', 'France')
pp = round(slices/sum(slices)*100)
lbls = paste(lbls, ' ', pp, '%', sep='') # add % to labels
## 畫圓餅圖
pie(slices, labels=lbls, col=rainbow(length(lbls))) # #col=(1:length(lbls)))

#Ex9-1
datfile = paste(datdir, 'mathmark.dat', sep='')
dat= read.table(datfile, header = T); dim(dat); dat[1:3,]
alg = dat[,3]
## 畫 algebra 直方圖
h = hist(alg, breaks=60, xlim=c(0,100), col="grey")
cutpts = quantile(alg, prob=c(0.1, 0.5, 0.9)); cutpts
abline(v=cutpts, col=c("pink","green","pink"),lty=c(3,1,3), lwd=2)
xfit = seq(from=min(alg), to=max(alg), length=40)
yfit = dnorm(xfit, mean=mean(alg), sd=sd(alg)) #密度
yfit = yfit*diff(h$mids[1:2])*length(alg)
lines(xfit, yfit, col="blue", lwd=2)


#CH8
#NA
a= c(1, 2, NA, 4, 5); b = c(0, NA, 10, NA, -1)
x = data.frame(a, b); x
na.pass(x)
na.fail(x) # 傳回 error 訊息
na.omit(x)
na.exclude(x)#這兩個一樣

#apply
x=matrix(1:12, nrow=3, ncol=4); x
apply(x, 1, sum) # 傳回每個 row 的 sum
apply(x, 2, sum) 
rowSums(x);colSums(x)
apply(x, 1, mean) # 傳回每個 row 的 mean


d= c(1,2,1,4,5); e = c(0,1,10,0,0)
y = t(data.frame(d, e)); y
apply(y, 1, table) 

#sapply?
x = list(a=1:10, beta=exp(-3:3), logic=c(T,F,F,T)); x



#grep 在 x 元素中 尋找特定字串
x = c('arm', 'foot', 'lefroo', 'bafoobar')
loc = grep('foo', x); loc
out = x[loc]; out

#春秋作
id = c('96S001', '98F003', '96F002', '97A001', '97A002',
       '99F001', '99A001', '99F003', '99S002', '99S001',
       '95A001', '95A002', '100S003', '101S002', '101F001')
loc1 = grep('S', id); loc1 # 春作
id[loc1]
loc2 = grep('F', id); loc2 # 秋作
id[loc2]
loc = grep('A', id); loc
s=strsplit(id[loc], 'A');s
year=c();year
for(i in 1:length(s)){   #忘記1開始
  year[i]=s[[i]][1];year
  out=unique(year);out
}
out

#星座
zodiac = c('鼠', '牛', '虎', '兔', '龍', '蛇', '馬', '羊', '猴', '雞', '狗', '猪')
star = c('白羊','金牛','雙子','巨蟹','獅子','處女','天秤','天蠍','射手','魔羯','水瓶','雙魚')
start = c('0321', '0420', '0521', '0622', '0723', '0823', '0923','1024', '1122', '1221', '0121', '0220')
end = c('0419', '0520', '0621', '0722', '0822', '0922', '1023','1121', '1220', '0120', '0219', '0320')
get.star = function(Bday) {
  
  year = substr(Bday, start=1, stop=4); year
  month = substr(Bday, start=5, stop=6); month
  date = substr(Bday, start=7, stop=8); date
  out = matrix(NA, nrow=length(Bday), ncol=1) ;out
  for (i in 1:length(Bday)) {
    md = substr(Bday[i], start=5, stop=8); md
    zod = (as.numeric(year[i])-1900) %% 12 + 1; zod
    loc.start = grep(month[i], substr(start,1,2)); loc.start
    loc.end = grep(month[i], substr(end,1,2)); loc.end
    loc = c(loc.start, loc.end); loc # 月份可能落在的星座
    ii = (as.numeric(md)-as.numeric(start[loc]))*
      (as.numeric(end[loc])-as.numeric(md)); ii
    if (ii[1]<0 & ii[2]<0) {
      if (prod(ii)<0) jj = loc[ii>0] else jj = loc[ii==min(ii)]; jj } else {
        if (prod(ii)<0) jj = loc[ii>0] else jj = loc[ii==min(abs(ii))]; jj }
    out[i,1] = paste(year[i], '年', month[i], '月', date[i], '日出生: 屬',
                     zodiac[zod], ' '
                     , star[jj], '座', sep='')
  }
  return(out)
}
get.star(Bday=c('19830408', '19940101', '20141221' , '20141231','19980821'))


#基因
datfile = paste(datdir,'glist-hg18.txt', sep=''); datfile
dat = read.table(datfile, header=F, sep=''); dat[1:6,]; dim(dat)
chr = as.character(as.matrix(dat[,1]))
gene = as.character(as.matrix(dat[,4]))
#---- 篩選出 HIST1H 基因家族資料
loc = grep('HIST1H', gene); loc
out1 = dat[loc,]; out1; dim(out1)

#---- 篩選出位於chr=16 (28396100-29725000) 的基因資料
loc = which(chr== '16'); loc
temp = dat[loc,]; temp[1:6,]; dim(temp)
bp0 = as.numeric(as.matrix(temp[,2]))
bp1 = as.numeric(as.matrix(temp[,3]))
start = 28396100; end = 29725000
ii = (bp0-start)*(bp0-end); ii # gene in the region if ii<=0
jj = (bp1-start)*(bp1-end); jj # gene in the region if jj<=0
kk = 1*(bp0<start)*(bp1>end); kk # gene covers the region if kk=1
loc.ijk = which(ii<=0 | jj<=0 | kk==1); loc.ijk
out2 = temp[loc.ijk,]; out2; dim(out2)
#排除法
ii=(bp1<start & bp1<end)
jj=(bp0>start & bp0>end)
loc.ij=which(ii==1|jj==1);loc.ij
out3=temp[-loc.ij,];out3;dim(out3)


CH9
#羅吉斯
cn =c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt = c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group = gl(n=2, k=10, length=20, labels=c('cn','trt')); group
weight = c(cn, trt)

fit1 = lm(weight ~ group); fit1 # weight = α + β*group + ε
summary(fit1)
fit2 = lm(weight ~ group - 1); fit2 # weight = β*group + ε #沒截距ㄉ
summary(fit2)
names(fit2)

fit = lm(Sepal.Length ~ Petal.Width, data=iris) #什麼花的資料
summary(fit)
par(mfcol=c(2,2))
plot(fit)
#一頭霧水==

#殘差
fit1 = lm(Sepal.Length ~ Petal.Width, data=iris)
resid = fit1$residuals
shapiro.test(resid)




