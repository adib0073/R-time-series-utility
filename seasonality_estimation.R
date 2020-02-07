## Categorical Linear Regression (ANOVA)
month = season(temp)
model1 = lm(temp~month-1)
summary(model1)
temp.fit.aov = model1$fitted
temp.fit.aov = ts(temp.fit.aov,start=1950,frequency=12)
par(mfrow=c(1,1))
ts.plot(temp,main='ANOVA Fit',col='blue',ylab='Temp (F)')
lines(temp.fit.aov,col='red')

## COS-SIN
har=harmonic(temp)
model2=lm(temp~har)
temp.fit.cossin = fitted(model2)
temp.fit.cossin = ts(temp.fit.cossin,start=1950,frequency=12)
ts.plot(temp,main='Cos-Sin Fit',col='black',ylab='Temp (F)')
lines(temp.fit.cossin,col='orange')


########## Seasonality Estimation ##################
## Categorical Linear Regression (ANOVA)
month = season(temp)
model1 = lm(temp~month-1)
summary(model1)
temp.fit.aov = model1$fitted
temp.fit.aov = ts(temp.fit.aov,start=1950,frequency=12)
par(mfrow=c(1,1))
ts.plot(temp,main='ANOVA Fit',col='blue',ylab='Temp (F)')
lines(temp.fit.aov,col='red')
## COS-SIN
har=harmonic(temp)
model2=lm(temp~har)
temp.fit.cossin = fitted(model2)
temp.fit.cossin = ts(temp.fit.cossin,start=1950,frequency=12)
ts.plot(temp,main='Cos-Sin Fit',col='black',ylab='Temp (F)')
lines(temp.fit.cossin,col='orange')
## Residual Analysis
resids.fit.aov=residuals(model1)
resids.fit.cossin=residuals(model2)
par(mfrow=c(2,2))
#Plot Residuals
plot(ts(resids.fit.aov,1950,frequency=12),main="ANOVA Residuals",cex=0.3,ylab="residuals",col='blue')
abline(a=mean(resids.fit.aov),b=0,col='red')
plot(ts(resids.fit.cossin,1950,frequency=12),main="Cos-Sin Residuals",cex=0.3,ylab="residuals",col='blue')
abline(a=mean(resids.fit.cossin),b=0,col='red')
#Residual ACF Plots
acf(as.numeric(resids.fit.aov),main="ACF of ANOVA Residuals",cex=0.3)
acf(as.numeric(resids.fit.cossin),main="ACF of Cos-Sin Residuals",cex=0.3)