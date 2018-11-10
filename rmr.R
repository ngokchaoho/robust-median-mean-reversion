
rmr <- function(fid, data, varagins){
  epsilon = varagins[1]
  W = varagins[2]
  tc = varagins[3]
  output = rmr_run(fid, data,epsilon,tc, W)
  return(output)
}

rmr_run <- function(fid,data,epsilon,tc,W) {
  T = dim(data)[1]
  N = dim(data)[2]
  run_ret = NULL
  total_ret = cbind(rep(1,T))
  day_ret = cbind(rep(1,T))
  day_weight = cbind(rep(1,N)/N)
  day_weight_o = cbind(rep(0,N))
  day_weight_n = cbind(rep(0,N))
  turno = 0
  data_close = matrix(1,T,N)
  for (i in 2:T) {
    data_close[i,] = data_close[i - 1,]*data[i,]
  }
  print(paste("Parameters tc:", tc))
  k = 1
  run_ret[k] = 1
  for (t in 1:T) {
    day_ret[t] = (data[t,] %*% day_weight) * (1 - tc/2  * sum(abs(day_weight - day_weight_o))) 
    run_ret[k] = run_ret[k] %*% day_ret[t]
    total_ret[t] = run_ret[k] 
    day_weight_o = (day_weight * cbind(data[t,]))/day_ret[t] 
    if (t < T) {
      day_weight_n = rmr_day_weight(data_close,data,t + 1,day_weight,W,epsilon)
      turno = turno + sum(abs(day_weight_n - day_weight))
      day_weight = day_weight_n
    }
  }
  output = list(run_ret,total_ret,day_ret,turno)
  return(output)
}

rmr_day_weight <- function(data_close, data, t1, day_weight,w,epsilon){
  if (t1 < w + 2) {
    x_t1 = data[t1 - 1,]
  } else {
    x_t1 = l1median_VaZh_z(data_close[(t1 - w):(t1 - 1),])/data_close[t1 - 1,]
  }
  
  if (norm(as.matrix(x_t1 - mean(x_t1))^2,type = '2') == 0) {
    tao = 0
  } else {
    tao = min(0,((x_t1) %*% day_weight - epsilon)/(norm(as.matrix(x_t1 - mean(x_t1)),type = '2')^2))
  }
  day_weight = day_weight - tao * cbind(x_t1 - mean(x_t1))
  day_weight = simplex_projection(day_weight,1)
  return(day_weight)
}

l1median_VaZh_z <- function(X) {
  medIn = apply(X,2,median)
  zerotol = 1e-15
  maxiter = 200
  rn = dim(X)[1]
  vn = dim(X)[2]
  iterdis = 1
  iter = 0
  y = medIn
  tol = 1e-9
  while (iterdis > 0 && iter < maxiter) {
    Tnum = rep(0,vn)
    R = rep(0,vn)
    Tden = 0 
    yita = 0 
    for (i in 1:rn) {
      dist = norm(as.matrix(X[i,] - y),type = '2')
      if (dist >= zerotol) {
       Tnum = Tnum + X[i,]/dist
       Tden = Tden + 1/dist
       R = R + (X[i,] - y)/dist
      } else {
        yita = 1
      }
    }
    if (Tden == 0) {
      T = 0
    } else {
      T = Tnum/Tden
    }
    if (norm(as.matrix(R),type = '2') == 0) {
      r = 0
    } else {
      r = min(1,yita/norm(as.matrix(R),type = '2'))
    }
    Ty = (1 - r) * T + r*y
    iterdis = norm(as.matrix(Ty - y),type = '1') - tol * norm(as.matrix(y),type = '1')
    iter = iter + 1
    y = Ty
  }
  return(y)
}

simplex_projection <- function(v,b){
  if (b < 0)
  {print('error')}
  v = (v > 0) * v
  u = sort(v, decreasing = TRUE)
  sv = cumsum(u)
  rho = tail(which(u > (sv - b)/c(1:length(u))),n = 1)
  #print(rho)
  #print((sv[rho]-b)/rho)
  theta = max(0,(sv[rho] - b)/rho)
  #print("theta")
  #print(theta)
  temp = v - theta
  temp[temp < 0] = 0
  w = temp
  return(w)
}



#install.packages('R.matlab')
library("R.matlab")
#install.packages("readxl")
#install.packages("stats")
#library(stats)
#library(readxl)
path <- ('Data')
#input
pathname <- file.path(path,'sp500.mat')
data_1 <- as.vector(readMat(pathname))
#data_matrix <- read_excel(pathname, sheet = "P4", skip=4, col_names = FALSE)
#data_matrix <- data.matrix(data_matrix[,2:ncol(data_matrix)])
#data_matrix <- data_matrix[complete.cases(data_matrix),]
#data_matrix <- read.csv(pathname,sep=',',stringsAsFactors = FALSE,skip=3,header=TRUE)
#class(data_1)
#print(data_1)
data_matrix <- as.matrix(as.data.frame(data_1))
#class(data_matrix)
fid = "rmr.txt"
#implementation
parameters = c(5, 5, 0)
result = rmr(fid,data_matrix,parameters)
write.csv(file = "rmr.csv",result)
source("ra_result_analyze.R")
ra_result_analyze(paste(pathname,"rmr.csv",sep = '_'),data_matrix,result[[1]],result[[2]],result[[3]])
