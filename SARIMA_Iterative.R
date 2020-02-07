#### Seasonal ARIMA Iteration Order Selecion Approach

# Example 1
final.aic = Inf
final.arima.order = c(0,1,0)
final.seasonal.order = c(0,1,0)
for (p in 0:2){
  for (q in 0:2){
    for (P in 0:2) {
      for (Q in 0:2) {
        mod = arima(level.ts, order=c(p,1,q),
                    seasonal=list(order=c(P,1,Q), period=12), method="ML")
        current.aic = AIC(mod)
        print(paste(p,q,P,Q,current.aic,sep=" "))
        if (current.aic < final.aic){
          final.aic = current.aic
          final.arima.order = c(p,1,q)
          final.seasonal.order = c(P,1,Q)
          final.arima = mod
        }
      }
    }
  }
}

final.arima.order
final.seasonal.order
final.aic
final.arima
#seasonal arima fit = arima(data, order = c(1,1,1), seasonal = list(order = c(2,1,2), period = 12), method = "ML")

#Example 2
log.house =log(house)
n= length(log.house)
final.aicc = Inf
for (p in 0:3) for (d in 0:1) for (q in 0:3) for (D in 0:1){
  temp= arima( log.house ,  order= c(p,d,q), seasonal=list(order=c(0,D,0), period=4) )
  temp.aicc = temp$aic - 2*(p+q+1) + 2*n*(p+q+1)/(n-p-q-2)
  if ( temp.aicc < final.aicc ){
    final.aicc=temp.aicc
    final.order = c(p = p, d = d, q = q, D=D)
  }
}

final.order
final.aicc

temp= arima( log(house) ,  order= c(3,1,2), seasonal=list(order=c(0,1,0), period=4), method='ML' )