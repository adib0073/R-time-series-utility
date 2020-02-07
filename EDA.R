# EDA Plots

plot.check.stationarity <- function(orig.ts, title) {
par(mfrow=c(2,2))
diff.ts = diff(orig.ts)
ts.plot(diff.ts,main= paste('Time Series plot for:', title))
abline(a=mean(diff.ts),b=0,col='red')
acf(diff.ts,lag = 52*4 ,main= paste('ACF plot for:', title))
pacf(diff.ts,lag = 52*4 ,main= paste('PACF plot for:', title))
qqnorm(diff.ts, main= paste('Q-Q plot for:', title))
qqline(diff.ts)
}

plot.check.stationarity(EU.total, 'USD/EU Diff Data')
plot.check.stationarity(GBP.total, 'USD/GBP Diff Data')
plot.check.stationarity(AU.total, 'USD/AU Diff Data')
plot.check.stationarity(NZ.total, 'USD/NZ Diff Data')