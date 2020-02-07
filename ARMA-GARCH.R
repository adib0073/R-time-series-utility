#Code Snippet for Order selection using Iteration approach for ARIMA model:
test_modelA <- function(p,d,q){
mod = arima(data.growth, order=c(p,d,q), method="ML")
current.aic = AIC(mod)
df = data.frame(p,d,q,current.aic)
names(df) <- c("p","d","q","AIC")
print(paste(p,d,q,current.aic,sep=" "))
return(df)
}
orders = data.frame(Inf,Inf,Inf,Inf)
names(orders) <- c("p","d","q","AIC")
for (p in 0:4){
for (d in 0:1){
for (q in 0:4) {
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

# for serial correlation
Box.test(resids,lag=1,type='Ljung',fitdf=0)
# for heteroscedasticity in residuals (ARCH effect)
Box.test((resids)^2,lag=1,type='Ljung',fitdf=0)



#Code snippet:
#GARCH update
test_modelAGG <- function(m,n){
spec = ugarchspec(variance.model=list(garchOrder=c(m,n)),
mean.model=list(armaOrder=c(4,2),
include.mean=T), distribution.model="std")
fit = ugarchfit(spec, data.growth.train, solver = 'hybrid')
current.bic = infocriteria(fit)[2]
df = data.frame(m,n,current.bic)
names(df) <- c("m","n","BIC")
print(paste(m,n,current.bic,sep=" "))
return(df)
}
orders = data.frame(Inf,Inf,Inf)
names(orders) <- c("m","n","BIC")
for (m in 0:2){
for (n in 0:2){
possibleError <- tryCatch(
orders<-rbind(orders,test_modelAGG(m,n)),
error=function(e) e
)
if(inherits(possibleError, "error")) next
}
}
orders <- orders[order(-orders$BIC),]
tail(orders)
# minimum BIC order 1,2
#ARMA update
#ARIMA-GARCH ARIMA order
test_modelAGA <- function(p,q){
spec = ugarchspec(variance.model=list(garchOrder=c(1,2)),
mean.model=list(armaOrder=c(p,q),
include.mean=T), distribution.model="std")
fit = ugarchfit(spec, data.growth.train, solver = 'hybrid')
current.bic = infocriteria(fit)[2]
df = data.frame(p,q,current.bic)
names(df) <- c("p","q","BIC")
print(paste(p,q,current.bic,sep=" "))
return(df)
}
orders = data.frame(Inf,Inf,Inf)
names(orders) <- c("p","q","BIC")
for (p in 0:4){
for (q in 0:4){
possibleError <- tryCatch(
orders<-rbind(orders,test_modelAGA(p,q)),
error=function(e) e
)
if(inherits(possibleError, "error")) next
}
}
orders <- orders[order(-orders$BIC),]
tail(orders)
# minimum BIC value 4,4
#Final Garch Order
#GARCH update
test_modelAGG <- function(m,n){
spec = ugarchspec(variance.model=list(garchOrder=c(m,n)),
mean.model=list(armaOrder=c(4,4),
include.mean=T), distribution.model="std")
fit = ugarchfit(spec, data.growth.train, solver = 'hybrid')
current.bic = infocriteria(fit)[2]
df = data.frame(m,n,current.bic)
names(df) <- c("m","n","BIC")
print(paste(m,n,current.bic,sep=" "))
return(df)
}
orders = data.frame(Inf,Inf,Inf)
names(orders) <- c("m","n","BIC")
for (m in 0:2){
for (n in 0:2){
possibleError <- tryCatch(
orders<-rbind(orders,test_modelAGG(m,n)),
error=function(e) e
)
if(inherits(possibleError, "error")) next
}
}
orders <- orders[order(-orders$BIC),]
tail(orders)
#minimum BIC order 1,1
#Final Arima-Garch Model
spec = ugarchspec(variance.model=list(garchOrder=c(1,1)),
mean.model=list(armaOrder=c(4, 4),
include.mean=T), distribution.model="std")
final.model = garchFit(~ arma(4,4)+ garch(1,1), data=data.growth.train, trace = FALSE)
summary(final.model)