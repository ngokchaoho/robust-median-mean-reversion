
pamr_run <- function(fid, data, tc)
 {
  data_matrix=data
  t = nrow(data_matrix)
  m = ncol(data_matrix)
  
  cum_ret = 1
  daily_ret = NULL
  cumpro_ret = NULL
  
    
  e = 0.5  
  tc = 0
  SumReturn = 1
  day_weight = t(as.matrix(rep(1/m,times = m)))
  day_weight_o = t(as.matrix(rep(0,times = m)))
  daily_portfolio = as.vector(rep(NULL,times = m))
  for (i in seq(from = 1, to = t))
  {
    data <- t(as.matrix(data_matrix[i,]))
    if (i >= 2)
    {
      data1 <- t(as.matrix(data_matrix[i - 1,]))
      day_weight2 <- day_weight - eta*(data1 - sum(data1)/m)
      day_weight = simplex_projection(day_weight2,1)
    }
  
    day_weight <- day_weight/sum(day_weight)
    if (i == 1)
    {
      daily_portfolio = day_weight
    }
    else
    {
      daily_portfolio = rbind(daily_portfolio,day_weight)
    }
    # daily_portfolio[i,] is the day_weight of the i-th period.
    #data=# the closing prices of m assets in i th period From the dataset.
    daily_ret = cbind(daily_ret,((data) %*% t(day_weight) %*% (1 - tc/2*sum(abs(day_weight - day_weight_o)))))#every element is the the return of every day.
    cum_ret = cum_ret*daily_ret[i]
    cumpro_ret = cbind(cumpro_ret,cum_ret)
    day_weight_o = day_weight*data/daily_ret[i]
    denominator = (data - 1/m*sum(data)) %*% t(data - 1/m*sum(data))# 1*30
    if (denominator != 0)
      eta = (daily_ret[i] - e)/denominator
    eta = max(0,eta)
  #  eta
  }
  return(list(cum_ret,cumpro_ret,daily_ret))
}

 simplex_projection <- function(v,b)
   {
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
 pathname <- file.path(path,'tse.mat')
 data_1 <- as.vector(readMat(pathname))
 #data_matrix <- read_excel(pathname, sheet = "P4", skip=4, col_names = FALSE)
 #data_matrix <- data.matrix(data_matrix[,2:ncol(data_matrix)])
 #data_matrix <- data_matrix[complete.cases(data_matrix),]
 #data_matrix <- read.csv(pathname,sep=',',stringsAsFactors = FALSE,skip=3,header=TRUE)
 #class(data_1)
 #print(data_1)
 data_matrix <- as.matrix(as.data.frame(data_1))
 #class(data_matrix)
 fid = "pamr.txt"
 tc = 0
 result = pamr_run(fid,data_matrix,tc)
 write.csv(file = "pamr.csv",result)
 source("ra_result_analyze.R")
 ra_result_analyze(paste(pathname,"pamr.csv",sep = '_'),data_matrix,as.numeric(result[[1]]),as.numeric(result[[2]]),as.numeric(result[[3]]))