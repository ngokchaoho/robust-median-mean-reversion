# install.packages("Ecdat")
# install.packages("tseries")
# install.packages("fGarch")
data(Tbrate,package="Ecdat")
library(tseries)
library(fGarch)
# r = the 91-day treasury bill rate
# y = the log of real GDP
# pi = the inflation rate
Tbill = Tbrate[,1]
Del.Tbill = diff(Tbill)
del.log.tbill = diff(log(Tbill))
Del.Tbill2 =Del.Tbill^2

adf.test(Tbill)
kpss.test(Tbill)

par(mfrow=c(2,2))
acf(Del.Tbill)
pacf(Del.Tbill)
acf(Del.Tbill2)
pacf(Del.Tbill2)

garch.model.Tbill = garchFit(formula= ~arma(1,0) + garch(1,0),Del.Tbill)
summary(garch.model.Tbill)
garch.model.Tbill@fit$matcoef

res = residuals(garch.model.Tbill)
res_std = res / garch.model.Tbill@sigma.t
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res_std)
acf(res_std)
acf(res_std^2)

garch.model.log.Tbill = garchFit(formula= ~arma(1,0) + garch(1,1),del.log.tbill)
summary(garch.model.log.Tbill)
garch.model.log.Tbill@fit$matcoef

res = residuals(garch.model.log.Tbill)
res_std = res / garch.model.log.Tbill@sigma.t
par(mfrow=c(2,3))
plot(res)
acf(res)
acf(res^2)
plot(res_std)
acf(res_std)
acf(res_std^2)

garch.model.Tbill_log_11 = garchFit(formula = ~arma(1,0)+garch(1,1),del.log.tbill)
summary(garch.model.Tbill_log_11)
res_log_11 = residuals(garch.model.Tbill_logg_11)
res_std_log_11 = res_log_11/ garch.model.Tbill_log_11@sigma.t
par(mfrow=c(1,2))
plot (Tbill [-1] ,abs(res_std) ,xlab="Tbill" ,ylab="abs std residual", main="no log
transform")
lines(lowess (Tbill [-1], abs (res_std)), col="red", lwd=3)
plot (Tbill [-1] , abs (res_std_log_11), xlab="Tbill", ylab="abs std residual",
      main="log
      transform")
lines(lowess(Tbill[-1] ,abs(res_std_log_11)),col="red",lwd=3)
