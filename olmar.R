install.packages("R.matlab")
library("R.matlab")
path<-('E:\Lecture notes\FE5209 financial econometrics\project\OLPS-master\Data')
matdata<-file.path(path,"djia.mat")
data1<-as.vector(readMat(matdata))
datamatrix1<-as.matrix(as.data.frame(data1))
class(datamatrix1)
n<-dim(datamatrix1)[1]
m<-dim(datamatrix1)[2]

cum_ret = 1
cumpro_ret = NULL
daily_ret = NULL
epsilon=10
alpha=0.5
tc=0
sumreturn=1
day_weight = as.matrix(rep(1/m,m))
day_weight_o = as.matrix(rep(0,m))
daily_portfolio = as.vector(rep(NULL,m))
phi=t(as.matrix(rep(1,m)))

simplex_projection <-function(v,b){
  if (b<0)
  {print('error')}
  #print(v)
  v = ( v > 0) * v
  u = sort (v, decreasing = TRUE)
  sv = cumsum(u)
  rho = tail(which(u>(sv-b)/c(1:length(u))),n=1)
  #print(rho)
  #print((sv[rho]-b)/rho)
  theta=max((sv[rho]-b)/rho,0)
  temp=(v - theta)
  temp[temp< 0]=0
  w=temp
  return(w)
}

for(i in seq(from=1, to=n))
{data<-t(as.matrix(datamatrix1[i,]))
if(i>=2){
  phi=alpha+(1-alpha)*phi/datamatrix1[i-1,]
  ell=max(0,epsilon-phi%*%day_weight)
  xbar=mean(phi)
  denominator=(phi-xbar)%*%t(phi-xbar)
if(denominator!=0){
  lambda=ell/denominator
}else{
  lambda=0
}
  day_weight<-day_weight+as.numeric(lambda)*(t(phi)-xbar)
  day_weight<-simplex_projection(day_weight,1)
}
day_weight<-day_weight/sum(day_weight)
if(i==1)
{
  daily_portfolio=day_weight
}else{
  daily_portfolio=cbind(daily_portfolio,day_weight)
}
daily_ret=cbind(daily_ret,(data%*%day_weight)*(1-tc/2*sum(abs(day_weight-day_weight_o))))
cum_ret=cum_ret*daily_ret[i]
cumpro_ret=cbind(cumpro_ret,cum_ret)
day_weight_o = day_weight*t(data)/daily_ret[i]
}
plot(seq(from=1, to=n),cumpro_ret)
result[[1]]=as.numeric(cum_ret)
result[[2]]=as.numeric(cumpro_ret)
result[[3]]=as.numeric(daily_ret)
source("ra_result_analyze.R")
ra_result_analyze(data_matrix,result[[1]],result[[2]],result[[3]])

