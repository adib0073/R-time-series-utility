## Moving Average
time.pts = c(1:length(temp))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
mav.fit = ksmooth(time.pts, temp, kernel = "box")
temp.fit.mav = ts(mav.fit$y,start=1950,frequency=12)
ts.plot(temp,ylab="Temperature")
lines(temp.fit.mav,lwd=2,col="purple")
abline(temp.fit.mav[1],0,lwd=2,col="blue")

## Parametric Quadratic Polynomial
x1 = time.pts
x2 = time.pts^2
lm.fit = lm(temp~x1+x2)
summary(lm.fit)
temp.fit.lm = ts(fitted(lm.fit),start=1950,frequency=12)
ts.plot(temp,ylab="Temperature")
lines(temp.fit.lm,lwd=2,col="green")
abline(temp.fit.mav[1],0,lwd=2,col="blue")

## Local Polynomial - Non parametric
loc.fit = loess(temp~time.pts)
temp.fit.loc = ts(fitted(loc.fit),start=1950,frequency=12)
ts.plot(temp,ylab="Temperature")
lines(temp.fit.loc,lwd=2,col="brown")
abline(temp.fit.loc[1],0,lwd=2,col="blue")

## Splines Regression - Non Parametric
gam.fit = gam(temp~s(time.pts))
temp.fit.gam = ts(fitted(gam.fit),start=1950,frequency=12)
ts.plot(temp,ylab="Temperature")
lines(temp.fit.gam,lwd=2,col="red")
abline(temp.fit.gam[1],0,lwd=2,col="blue")


## Construct and plot residuals
res1 = temp-temp.fit.mav # Coming as NULL?
res2= residuals(lm.fit)
res3= residuals(loc.fit)
res4 = residuals(gam.fit)
res1.ts = ts(res1,start=1950,frequency=12)
res2.ts = ts(res2,start=1950,frequency=12)
res3.ts = ts(res3,start=1950,frequency=12)


res4.ts = ts(res4,start=1950,frequency=12)
par(mfrow=c(2,2))
##Residual Plot
ts.plot(res1.ts,main='Moving Average Residual Plot',col='blue')
abline(a=mean(res1.ts),b=0,col='red')
ts.plot(res2.ts,main='PQP Residual Plot',col='blue')
abline(a=mean(res2.ts),b=0,col='red')
ts.plot(res3.ts,main='Local Polynomial Residual Plot',col='blue')
abline(a=mean(res3.ts),b=0,col='red')
ts.plot(res4.ts,main='Splines Residual Plot',col='blue')
abline(a=mean(res4.ts),b=0,col='red')
#ACF Plots
acf(res1.ts,main='Moving Average Residual ACF')
acf(res2.ts,main='PQP Residual ACF')
acf(res3.ts,main='Local POlynomial Residual ACF')
acf(res4.ts,main='Splines Residual ACF')