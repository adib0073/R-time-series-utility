# Univariate Series

plot.predicted.with.cband <- function(outpred, data, data.title){
n = length(data)
nfit = n-8
ubound = outpred$pred+1.96*outpred$se
lbound = outpred$pred-1.96*outpred$se
ymin = min(lbound)
ymax = max(ubound)
vol=time(data)
par(mfrow=c(1,1))
plot(vol[nfit:n],data[nfit:n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab =data.title,main= paste("ARIMA Predictions",data.title))
points(vol[(nfit+1):n],outpred$pred,col="red")
lines(vol[(nfit+1):n],ubound,lty=3,lwd= 2, col="blue")
lines(vol[(nfit+1):n],lbound,lty=3,lwd= 2, col="blue")
}
# For USD-EU Data with model order (2,1,3)
EU.model = arima(EU.train,c(2,1,3), method='ML')
EU.pred = as.vector(predict(EU.model,n.ahead=8))
plot.predicted.with.cband(EU.pred, EU.total, "USD/EU Data")


# Multivariate
## Predict using Multi Variate Time Series
preds.all <- as.vector(predict(mod,n.ahead=8))
plot.predicted.var.with.cband <- function(data, data.pred, data.title){
n = length(data)
nfit = n-8
vol=time(data)
forecast = data.pred[,1]
lower = data.pred[,2]
upper = data.pred[,3]
ymin = min(lower)
ymax = max(upper)
par(mfrow=c(1,1))
plot(vol[nfit:n],data[nfit:n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab =data.title,main= paste("VAR(2) Predictions",data.title))
points(vol[(nfit+1):n],forecast,col="red")
lines(vol[(nfit+1):n],upper,lty=3,lwd= 2, col="blue")
lines(vol[(nfit+1):n],lower,lty=3,lwd= 2, col="blue")
}
var.EU.preds <- as.numeric(preds.all$fcst$USD.EU[,1])
plot.predicted.var.with.cband(EU.total, preds.all[[1]]$USD.EU, "USD/EU Data with VAR(2)")