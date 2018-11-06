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

library(stats)
library(readxl)
path <- ('E:\\nus-study\\FEconometrics-LN\\rmr-master\\DATA(haolin)')

#input
pathname <- file.path(path,'BE500.xlsx')
#data_1 <- as.vector(readMat(pathname))
data_matrix <- read_excel(pathname, sheet = "P2")
data_matrix <- data.matrix(data_matrix[5:nrow(data_matrix),2:ncol(data_matrix)])
#data_matrix <- data_matrix[complete.cases(data_matrix),]
dim(data_matrix)

t = nrow(data_matrix)
m = ncol(data_matrix)

varagins=c(5,5,0)

fid="rmr-ibov.csv"
rmr_result = rmr(fid,data_matrix,varagins)
write.csv(file = "rmr.csv",rmr_result)
plot(seq(from = 1, to = t),t(rmr_result[[2]]),col="black",type="l",ylim=c(0,100000),xlab="TIME(2001-2006)",ylab="TOTAL WEALTH in ibov")



tc=0
fid="BEST-ibov.csv"
best_result = best_run(fid,data_matrix,tc)
write.csv(file = "BEST.csv",best_result)
lines(seq(from = 1, to = t),best_result[[2]],col="red",type="l")



tc=0
fid="pamr-ibov.csv"
pamr_result = pamr_run(fid,data_matrix,tc)
write.csv(file = "pamr.csv",pamr_result)
lines(seq(from = 1, to = t),t(pamr_result[[2]]),col="blue",type="l")


tc=0
fid="olmar-ibov.csv"
olmar_result = olmar_run(fid,data_matrix,tc)
write.csv(file = "olmar.csv",olmar_result)
lines(seq(from = 1, to = t),t(olmar_result[[2]]),col="green",type="l")

vargins2=c(0,5)
fid="Market-ibov.csv"
market_result = market(fid,data_matrix,vargins2)
write.csv(file = "olmar.csv",market_result)
lines(seq(from = 1, to = t),t(market_result[[2]]),col="yellow",type="l")




title("Total Wealth of Different Strategies in IBOV",lwd=3)
legend("topright",cex=0.8,c("rmr","best","pamr","olmar","market"),col=c("black","red","blue","yellow","green"),lty=1:5)

