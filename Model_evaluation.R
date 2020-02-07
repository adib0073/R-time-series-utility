
calculate.mape <- function (preds, data.test) {
#MAPE
preds = as.numeric(preds$pred)
obs = as.numeric(data.test)
print(100*mean(abs(preds-obs)/obs))
}
calculate.mape(EU.pred, EU.test)

### Calculate MAE, MAPE and PM
observed.output = chart[(nfit+1):n]
predicted.output = outpred$pred
### Mean Absolute Prediction Error (MAE)
mean(abs(predicted.output-observed.output))
### Mean Absolute Percentage Error (MAPE)
mean(abs(predicted.output-observed.output)/observed.output)
### Precision Measure (PM)
sum((predicted.output-observed.output)^2)/sum((observed.output-mean(observed.output))^2)


## VAR model accuracy
calculate.var.mape <- function (preds, data.test) {
#MAPE
obs = as.numeric(data.test)
print(100*mean(abs(preds-obs)/obs))
}
calculate.var.mape(var.EU.preds, EU.test)
# 0.6303623