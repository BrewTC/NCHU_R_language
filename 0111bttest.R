x=sample(1:100,100);x
x.bt=sample(x,100,replace=T) ;x.bt
x1=unique(x.bt);x1
length(x1)/length(x.bt)

get.bt=function(n){
  x=sample(1:n,n)
  x.bt=sample(x,n,replace=T)
  x1=unique(x.bt)
  out=1-(length(x1)/length(x.bt))
  return (out*100)
  
}
get.bt(10000000)
