#CH3 Reading & Storing Data
#都用read.table就好
datdir = "D:/新尖兵/學生講義_pdf檔/dat/"

datfile = paste(datdir, 'data1.txt', sep='')
dat1 = read.table(datfile, header=T, sep='\t'); dat1; dim(dat1)

datfile = paste(datdir, 'data1.csv', sep='')
dat2 = read.table(datfile, header=T, sep=','); dat2; dim(dat2)

datfile = paste(datdir, 'data1.prn', sep='')
dat3 = read.table(datfile, header=T, sep=''); dat3; dim(dat3)

#讀SPSS
library(foreign)
datfile = paste(datdir, 'form 4.sav', sep='')
dat5 = read.spss(datfile, use.value.labels=F, to.data.frame=T)
dat5[1:3,1:10]; dim(dat5)
#讀dat
datfile = paste(datdir, 'cases.dat', sep='')
dat6 = read.table(datfile, header=F, sep=' ') # wrong
dat6 = read.table(datfile, header=F, sep='\t')
dat6; dim(dat6)
#讀GWAS
datfile = paste(datdir,'gwasQC.tfam', sep='')
dat7 = read.table(datfile, header=F, sep='\t')
dat7[1:4,]; dim(dat7)

datfile = paste(datdir, 'table1.map', sep='')
dat8 = read.table(datfile, header=F, sep='\t')  #沒表頭
dat8; dim(dat8)
#整理
chr = dat8[,1]; chr
loc = which(chr %in% c(1,10,13)); loc  #只挑1 10 13
# loc = which(chr %in% (4))  #會覆寫
out = dat8[loc,]; out; dim(out) #整理出來
colnames(out) = c('chr', 'SNP', 'dist', 'BP'); out
#把BP/1000 & 改表頭
BP=out[,4]/1000;BP
out[,4]=BP;out
colnames(out) = c('chr', 'SNP', 'dist', 'KBP'); out

#存csv檔
outfile = paste(datdir, 'out1.csv', sep=''); outfile
write.table(out, file=outfile, sep=',', quote=F,row.names=F, col.names=T)

#存txt
chr = dat8[,1]; chr
loc = which(chr %in% c(1, 4, 8, 10)); loc
out = dat8[loc,]; out; dim(out)
colnames(out) = c('chr', 'SNP', 'dist', 'BP'); out

outfile = paste(datdir, 'out2.txt', sep='')
write.table(out, file=outfile, sep='\t', quote=F,row.names=F, col.names=T)

#Ex5-1
x = matrix(1:100, nrow=20, ncol=5); x
loc.row = seq(1,20, by=2); loc.row  #先弄出要砍的地方
loc.col = seq(2,5, by=2); loc.col
out = x[-loc.row,-loc.col]; out #砍

outfile = paste(datdir, 'outMatrix.csv', sep='')
write.table(out, file=outfile, sep=',', quote=F,row.names=F, col.names=T)

#Ex5-2
x=matrix(1:108,nrow=18,ncol=6);x
loc.row=seq(1,18,by=2);loc.row
loc.col=seq(2,6,by=2);loc.col
out=x[loc.row,loc.col] ;out
outfile = paste(datdir, 'out2.csv2', sep='')
write.table(out, file=outfile, sep=';', quote=F,row.names=F, col.names=T)
read.csv2(outfile, header=T)


#CH4 Data recoding
#recode 分類
x=sample(1:99,20,replace=T)
1*(x<25)
2*(x>=25&x<50)
3*(x>=50&x<75)
4*(x>=75)
y=1*(x<25)+2*(x>=25&x<50)+3*(x>=50&x<75)+4*(x>=75); y #向量相加

table(y)

w=ifelse(x<25,1,ifelse(x>=25 & x<50,2,ifelse(x>=50 & x<75,3,4))) #直接給值
w
x = c('貓', '狗', '狼')
y = c('家庭寵物', '野生動物')
x.ind = 1*(x %in% c('貓', '狗')) + 2*(x %in% '狼'); x.ind
w = y[x.ind]; w


x=sample(c('a','b','c'),10,replace=T);x
table(x)
y=1*(x %in% c('a','c'))+2*(x %in% ('b')) ;y

#sort
x = c(2.2, NA, 3.5, 1.4, 4.9, NA, 8.6, 3.0)
sort(x, na.last=T)   #把NA放最後

#將xy依x由小到大排列
#法1
x = c(2.2, 2.9, 3.5, 1.4, 4.9, 1.9, 8.6, 3.0)
y = 1:8
xy = data.frame(x,y); xy
loc = c()
o = sort(x);o
for (i in 1:length(x)) {
  loc[i] = which(x==o[i]); loc     #一直把x跟o比對 x在哪個位置==o[i]
}
xy[loc,]  #依照loc列順序排列
xy[c(1,4,5),]

#法3
loc=c()
o=order(x);o  #回傳的是位置
xy[o,]


#rank不要用這個==
x = c(2.2, 2.9, 3.5, 1.4, 4.9, 1.9, 8.6, 3.0) ;x
ii = rank(x); ii
y=c()
w=c()
y[ii] = x ; y
w[3]=x[1];w

#法2 rank
loc = c()
test=c()
o = rank(x);o
loc[o] = rownames(xy) ;loc
test[c(3,4)]=rownames(xy)[c(1,2)];test
xy[loc,]



