install.packages("pracma")
install.packages("R.matlab")
setwd("E:\\nus-study\\FEconometrics-LN\\rmr-master")

source("ra_result_analyze.R")
source("rmr.R")
source("PAMR.r")
source("olmar.R")
source("MARKET.R")
source("BEST.R")
source("ARMA.R")

library("forecast")

library("R.matlab")
path <- ('E:\\rmr-master\\Data')
#input
pathname <- file.path(path,'nyse-n.mat')
data_1 <- as.vector(readMat(pathname))
#class(data_1)
#print(data_1)
data_matrix <- as.matrix(as.data.frame(data_1))
t = nrow(data_matrix)
m = ncol(data_matrix)

varagins=c(5,5,0)

fid="rmr-nyse-n.csv"
rmr_result = rmr(fid,data_matrix,varagins)
write.csv(file = "nyse-n.csv",rmr_result)
plot(seq(from = 1, to = t),t(rmr_result[[2]]),col="black",type="l",ylim=c(0,4.5),xlab="TIME",ylab="TOTAL WEALTH in NYSE-N")


tc=0
fid="rmr-arma-nyse-n.csv"
rmr_arma_result = rmr_arma(fid,data_matrix,varagins)
write.csv(file = "rmr_arma.csv",rmr_arma_result)
lines(seq(from = 1, to = t),rmr_arma_result[[2]],col="orange",type="l")

tc=0
fid="BEST-nyse-n.csv"
best_result = best_run(fid,data_matrix,tc)
write.csv(file = "BEST.csv",best_result)
lines(seq(from = 1, to = t),best_result[[2]],col="red",type="l")



tc=0
fid="pamr-nyse-n.csv"
pamr_result = pamr_run(fid,data_matrix,tc)
write.csv(file = "pamr.csv",pamr_result)
lines(seq(from = 1, to = t),t(pamr_result[[2]]),col="blue",type="l")


tc=0
fid="olmar-nyse-n.csv"
olmar_result = olmar_run(fid,data_matrix,tc)
write.csv(file = "olmar.csv",olmar_result)
lines(seq(from = 1, to = t),t(olmar_result[[2]]),col="green",type="l")

vargins2=c(0,5)
fid="Market-nyse-n.csv"
market_result = market(fid,data_matrix,vargins2)
write.csv(file = "olmar.csv",market_result)
lines(seq(from = 1, to = t),t(market_result[[2]]),col="yellow",type="l")




title("Total Wealth of Different Strategies in nyse-n",lwd=3)
legend("topright",cex=0.8,c("rmr","rmr-arma","best","pamr","olmar","market"),col=c("black","orange","red","blue","yellow","green"),lty=1:6)


