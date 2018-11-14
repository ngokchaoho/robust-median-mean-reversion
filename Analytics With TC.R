#install.packages('R.matlab')
#library("R.matlab")
#install.packages("readxl")
#install.packages("stats")
#library(stats)
library(readxl)
path <- ('Data(Haolin)')
#input
pathname <- file.path(path,'BE500.xlsx')
#data_1 <- as.vector(readMat(pathname))
data_matrix <- read_excel(pathname, sheet = "P3", skip = 4, col_names = FALSE)
data_matrix <- data.matrix(data_matrix[,2:ncol(data_matrix)])
#data_matrix <- data_matrix[complete.cases(data_matrix),]
#class(data_1)
#print(data_1)
#data_matrix <- as.matrix(as.data.frame(data_1))

source("ra_result_analyze.R")
source("plot/rmr.R")
source("plot/PAMR.r")
source("plot/olmar.R")
source("plot/MARKET.R")
source("plot/BEST.R")

t = nrow(data_matrix)
m = ncol(data_matrix)

tc = 0.0002
  varagins = c(5,5,tc)
  fid = "rmr-djia.csv"
  rmr_result = rmr(fid,data_matrix,varagins)

  ra_result_analyze(paste(pathname,"rmr_TC.csv",sep = '_'),data_matrix,rmr_result[[1]],rmr_result[[2]],rmr_result[[3]])

# tc=0
# fid="rmr-arma-djia.csv"
# rmr_arma_result = rmr_arma(fid,data_matrix,varagins)
# write.csv(file = "rmr_arma.csv",rmr_arma_result)
# lines(seq(from = 1, to = t),rmr_arma_result[[2]],col="orange",type="l")

#tc=

  fid = "BEST-djia.csv"
  best_result = best_run(fid,data_matrix,tc)
  ra_result_analyze(paste(pathname,"BEST_TC.csv",sep = '_'),data_matrix,best_result[[1]],best_result[[2]],best_result[[3]])

  fid = "pamr-djia.csv"
  pamr_result = pamr_run(fid,data_matrix,tc)
  ra_result_analyze(paste(pathname,"pamr_TC.csv",sep = '_'),data_matrix,as.numeric(pamr_result[[1]]),as.numeric(pamr_result[[2]]),as.numeric(pamr_result[[3]]))

  fid = "olmar-djia.csv"
  olmar_result = olmar_run(fid,data_matrix,tc)
  ra_result_analyze(paste(pathname,"olmar_TC.csv",sep = '_'),data_matrix,as.numeric(olmar_result[[1]]),as.numeric(olmar_result[[2]]),as.numeric(olmar_result[[3]]))

# vargins2 = c(0,5)
# fid = "Market-djia.csv"
# market_result = market(fid,data_matrix,vargins2)
# write.csv(file = "olmar.csv",market_result)
# lines(seq(from = 1, to = t),t(market_result[[2]]),col = "yellow",type = "l")




title("Total Wealth of Different Strategies in BE500 2007-2010 with Transaction cost",lwd = 3)
legend("topright",cex = 0.8,c("rmr","best","pamr","olmar"),col = c("black","red","blue","yellow"),lty = 1:4)




#source("ra_result_analyze.R")
#ra_result_analyze(data_matrix,best_result[[1]],best_result[[2]],best_result[[3]])
