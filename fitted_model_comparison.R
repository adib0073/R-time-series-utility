# Remove NA values na.omit()

########## Fitted Model Comaprison ########
fit=na.omit(level.ts)-na.omit(final.arima$residuals)
mae=mean(abs(fit-level.ts))
pm=sum((fit-na.omit(level.ts))^2)/sum((na.omit(level.ts)-mean(na.omit(level.ts))^2))
plot(level.ts, main = "Model 3(a)")
lines(fit, col="red")
mae
pm


fit=na.omit(level.ts)-na.omit(new.mod$residuals)
mae=mean(abs(fit-na.omit(level.ts)))
pm=sum((fit-na.omit(level.ts))^2)/sum((na.omit(level.ts)-mean(na.omit(level.ts)))^2)
plot(level.ts, main = "Model 3(c)")
lines(fit, col="red")
pm
mae