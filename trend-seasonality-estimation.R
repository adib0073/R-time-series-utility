#Code Snippet:
time.pts = c(1:length(rate))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
## Parametric Polynomial Regression
x1 = time.pts
x2 = time.pts^2
# Anova
week = season(rate)
lm.fit = lm(rate~x1+x2+week - 1)
summary(lm.fit)
dif.fit.lm = ts((rate-fitted(lm.fit)),start=c(2000,1),frequency=52)
ts.plot(dif.fit.lm,ylab="Residual Process", main='Parametric Polynomial Regression Residual Plot')
abline(a=mean(dif.fit.lm),b=0,col='red')
acf(dif.fit.lm, lag.max=52 * 3, main='PPR Residual ACF')

## Non-parametric model for trend and linear model for seasonality
gam.fit = gam(rate~s(time.pts)+week)
dif.fit.gam = ts((rate-fitted(gam.fit)),start=c(2000,1),frequency=52)
ts.plot(dif.fit.gam,ylab="Residual Process")
abline(a=mean(dif.fit.gam),b=0,col='red')
acf(dif.fit.gam, lag.max=52 * 2, main='Non-parametric ACF')

