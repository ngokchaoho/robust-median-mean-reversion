best_run<-function(fid, data, tc)
{
  output <- as.data.frame(data)
  size_data<- dim(output)
  n = size_data[1]
  m = size_data[2]
  data <- as.matrix(output)
  
  # Variables for return, start with uniform weight
  cum_ret = 1
  cumprod_ret = matrix(1,c(n, 1))
  daily_ret = matrix(1,c(n, 1))
  ret_m = matrix(1,c(n, 2)) #cumprod_ret + daily_ret
  day_weight = matrix(1,c(m, 1))/m
  day_weight_o = matrix(0,c(m, 1))
  daily_portfolio = matrix(0,nrow=n,ncol=m)
  
  # print file head
  write('------------------', file = fid, append = FALSE )
  ln <- paste('Parameters [tc:', tc, ']')
  write(ln, file = fid, append = TRUE )
  write('day\t Daily Return\t Total return', file = fid, append = TRUE )
  
  # Calculate wealth return for each stock
  tmp_daily_ret = matrix(1,c(m, 1))
  tmp_cumprod_ret = matrix(1,c(m, 1))
  
  for (p in 1:n)
  {
    tmp_daily_ret = data[p, ]
	  tmp_cumprod_ret = tmp_cumprod_ret*tmp_daily_ret
  }
  
  # Find the maximum and its index
  best_ind = which.max(tmp_cumprod_ret)
  
  day_weight = matrix(0,c(m, 1))
  day_weight[best_ind] <- 1
  
  for (p in 1:n)
  {
    # Normalize the constraint, always useless
    day_weight = day_weight/sum(day_weight)
    daily_portfolio[p, ] = t(day_weight)
    
    # Cal t's return and total return
    daily_ret[p, ] = (data[p,]%*%day_weight)*(1-tc/2*sum(abs(day_weight-day_weight_o)))
    cum_ret = cum_ret %*% daily_ret[p,]
    cumprod_ret[p, ] = cum_ret;
    
    # Adjust weight[p,] for the transaction cost issue
    day_weight_o = day_weight*(data[p,])/(daily_ret[p, 1])

    lns <- paste(p,'\t',daily_ret[p, 1],'\t',cumprod_ret[p, 1])
    write(lns,file=fid,append=TRUE)
  }
  
  ln1 <- paste('BEST(tc=', tc,' ,Final return: ', cum_ret)
  write(ln1, file=fid, append=TRUE)
  write('------------------', file=fid, append=TRUE)
  return(list(cum_ret,cumprod_ret,daily_ret))
}
library("R.matlab")
path <- ('Data/')
#input
pathname <- file.path(path,'djia.mat')
data_1 <- as.vector(readMat(pathname))
#class(data_1)
#print(data_1)
data_matrix <- as.matrix(as.data.frame(data_1))

tc=0
result = best_run(fid,data_matrix,tc)
write.csv(file = "BEST.csv",result)
source("ra_result_analyze.R")
ra_result_analyze(data_matrix,result[[1]],result[[2]],result[[3]])

