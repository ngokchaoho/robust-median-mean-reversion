#install.packages("pracma")
library(pracma)
ra_dd <- function(ret_data) {
  nDays = length(ret_data)
  ddval = (max(ret_data) - ret_data[nDays])/max(ret_data)
  return(ddval)
}
ra_mdd <- function(data) {
  nDays = length(data)
  mddvec = cbind(rep(0,nDays))
  for (i in 1:nDays) {
    mddvec[i] = ra_dd(data[1:i])
  }
  mddval = max(mddvec)
  return(mddval)
}
ra_result_analyze <- function(fid,data, cum_ret, cumsum_ret, daily_ret) {
  RET_RF = 1.000156
  NUM_TRADE = 252
  n = dim(data)[1]
  m = dim(data)[2]
  market_daily_ret = cbind(rep(1,n))
  day_weight = cbind(rep(1, m))/m
  for (t in 1:n) {
    day_weight = day_weight/sum(day_weight)
    market_daily_ret[t] = data[t,] %*% day_weight
    day_weight = (day_weight * cbind(data[t,]))/market_daily_ret[t]
  }
  win_ratio = sum(daily_ret >= market_daily_ret)/n
  # Statistical t-Tests
  market_mu = colMeans(market_daily_ret) - 1
  strategy_mu = mean(daily_ret) - 1
  x = cbind(rep(1,n),market_daily_ret - RET_RF)
  y = daily_ret - RET_RF
  a = mldivide(x, y)
  alpha = a[1]
  beta = a[2]
  
  strategy_se = sd(y - x %*% a)/sqrt(n)
  t_stat = alpha / strategy_se
  p_value = 1 - pt(t_stat,n-1)
  # Risk adjusted return: APY, standard deviation, and annualized Sharpe Ratio
  apy = (cum_ret)^(1/round(n/NUM_TRADE)) - 1
  stdev = sd(daily_ret * sqrt(NUM_TRADE))
  sr = (apy - 0.04)/stdev
  # Risk adjusted return: DD, MDD and CR
  ddval = ra_dd(cumsum_ret)
  mddval = ra_mdd(cumsum_ret)
  cr = apy/mddval
  #Store all analyzed value
  ra_ret = NULL
  # Results for statistical t-test
  ra_ret[1] = n
  ra_ret[2] = strategy_mu
  ra_ret[3] = market_mu
  ra_ret[4] = win_ratio
  ra_ret[5] = alpha
  ra_ret[6] = beta
  ra_ret[7] = t_stat
  ra_ret[8] = p_value
  # results for standard deviation of return and sharpe ratio.
  ra_ret[9] = apy
  ra_ret[10] = stdev
  ra_ret[11] = sr
  # Results for maximum drawdown analysis and calmar ratio.
  ra_ret[12] = ddval
  ra_ret[13] = mddval
  ra_ret[14] = cr
  names(ra_ret) = c("n","strategy_mu","market_mu","win_ratio","alpha","beta","t_stat","p_value","apy","stdev","sr","ddval","mddval","cr")
  write.table(file = fid,ra_ret,col.names = FALSE)
}