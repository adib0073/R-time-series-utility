## Prediction of the return time series
nfore = length(data.growth.test)
fore.series = NULL
for(f in 1: nfore){
## Fit models
data = data.growth.train
if(f>=2)
data = c(data.growth.train,data.growth.test[1:(f-1)])
final.model = ugarchfit(spec, data, solver = 'hybrid')
## Forecast
fore = ugarchforecast(final.model, n.ahead=1)
fore.series = c(fore.series, fore@forecast$seriesFor)
}

### Mean Absolute Prediction Error (MAPE)
100*mean(abs(fore.series - data.growth.test)/abs(data.growth.test))
#120.9002
### Precision Measure (PM) sum((fore.series - data.growth.test)^2)/sum((data.growth.test-mean(data.growth.test))^2)