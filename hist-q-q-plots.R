#Normality
par(mfrow=c(1,2))
hist(final.resid,breaks="FD",xlab='Standardized Residuals',main='Residual')
qqnorm(final.resid)
qqline(final.resid)

jarque.bera.test(na.omit(final.resid))