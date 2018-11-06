

market <- function(fid, data, varagins){
  
  t = nrow(data_matrix)
  m = ncol(data_matrix)
  
  cum_ret = 1
  daily_ret = NULL
  total_ret = NULL
  
  
  
  SumReturn = 1
  day_weight = t(as.matrix(rep(1/m,times = m)))
  day_weight_o = t(as.matrix(rep(0,times = m)))
  
  tc = varagins[1]
  e = varagins[2]
  for (i in seq(from = 1, to = t))
  {
    data <- t(as.matrix(data_matrix[i,]))
    nrow(data)
    ncol(data)
    daily_ret = cbind(daily_ret,(data) %*% t(day_weight))*(1 - tc/2*sum(abs(day_weight - day_weight_o)))
    cum_ret = cum_ret*daily_ret[i]
    total_ret = cbind(total_ret,cum_ret)
    day_weight_o = day_weight*data/daily_ret[i]
    day_weight = day_weight_o
    day_weight = day_weight/sum(day_weight)
  }
  output = list(as.numeric(cum_ret),as.numeric(total_ret),as.numeric(daily_ret))
  return(output)
}

# library("R.matlab")
# path <- ('Data/')
# #input
# pathname <- file.path(path,'djia.mat')
# data_1 <- as.vector(readMat(pathname))
# #class(data_1)
# #print(data_1)
# data_matrix <- as.matrix(as.data.frame(data_1))
# #class(data_matrix)
# 
# 
# fid = 'market.txt'
# result = market(fid,data_matrix,c(0,0.5))
# source("ra_result_analyze.R")
# ra_result_analyze(data_matrix,result[[1]],result[[2]],result[[3]])
