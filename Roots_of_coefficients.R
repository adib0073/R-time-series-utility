#Example 1
temp= arima( log(house) ,  order= c(3,1,2), seasonal=list(order=c(0,1,0), period=4), method='ML' )

abs( polyroot( c(1 , coef(temp)[1:3]) ))
#[1] 1.155430 1.814989 1.155430
abs( polyroot( c(1 , coef(temp)[4:5]) ))

#Example 2
arima_new
polyroot(c( -0.1500,0.8671,0.0655)) # all roots not >1 hence not causal
polyroot(c(-0.4864, 0.3650)) # Invertibile since roots > 1