new.mod = arima(level.ts, order=c(1,1,3),seasonal=list(order=c(0,1,1), period=12), method="ML")
new.mod
BIC(new.mod)
AIC(new.mod)
HQIC()


infocriteria(new.mod)