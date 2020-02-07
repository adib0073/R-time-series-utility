# Example 1
find.model.orders <- function(orig.ts) {
	test_model <- function(p,d,q){
		mod = arima(orig.ts, order=c(p,d,q), method="ML")
		current.aic = AIC(mod)
		df = data.frame(p,d,q,current.aic)
		names(df) <- c("p","d","q","AIC")
		print(paste(p,d,q,current.aic,sep=" "))
		return(df)
		}
	orders = data.frame(Inf,Inf,Inf,Inf)
	names(orders) <- c("p","d","q","AIC")
	for (p in 0:5){
	for (d in 0:1){
	for (q in 0:5) {
		possibleError <- tryCatch(
		orders<-rbind(orders,test_model(p,d,q)),
		error=function(e) e
		)
	if(inherits(possibleError, "error")) next
	}
	}
	}
	orders <- orders[order(-orders$AIC),]
	print('The best fit orders are as follows:')
	tail(orders)
}

find.model.orders(EU.total)

#Example 2
test_modelA <- function(p,d,q){
mod = arima(chart, order=c(p,d,q), method="ML")
current.aic = AIC(mod)
df = data.frame(p,d,q,current.aic)
names(df) <- c("p","d","q","AIC")
print(paste(p,d,q,current.aic,sep=" "))
return(df)
}
orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")

for (p in 0:3){
for (d in 0:1){
for (q in 0:3) {
possibleError <- tryCatch(
orders<-rbind(orders,test_modelA(p,d,q)),
error=function(e) e
)
if(inherits(possibleError, "error")) next
}
}
}
orders <- orders[order(-orders$AIC),]
tail(orders)
arima_final <- arima(chart, order=c(3,1,3), method="ML")
arima_final