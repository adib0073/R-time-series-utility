# Univariate

# Auto-correlation
acf(final.resid,na.action = na.pass,lag=60)
Box.test(final.resid,lag=6,type="Ljung",fitdf=5)

#Normality
par(mfrow=c(1,2))
hist(final.resid,breaks="FD",xlab='Standardized Residuals',main='Residual')
qqnorm(final.resid)
qqline(final.resid)

jarque.bera.test(na.omit(final.resid))

# Remove NA values na.omit()

residuals <- resid(arima_final)
#par(mfrow=c(3,1))
ts.plot(residuals)
acf(residuals)
pacf(residuals)
Box.test(resid(arima_final), lag = 7, type = "Box-Pierce", fitdf = 6)
Box.test(resid(arima_final), lag = 7, type = "Ljung-Box", fitdf = 6)
### Box test, lag used is sum of orders (7 = 3+1+3)
### Degrees of freedom, fitdf is lag - 1, so 6 here


#Multivariate
vs <- VARselect(train.ts)
vs$selection
#VAR(2)
mod <- VAR(train.ts,p=2)
## ARCH, Residual Analysis: Constant Variance Assumption
arch.test(mod)
## J-B, Residual Analysis: Normality Assumption
normality.test(mod)
## Portmantau, Residual Analysis: Uncorrelated Errors Assumption
serial.test(mod)