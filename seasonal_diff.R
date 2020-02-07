#this is how we find seasonal difference
seasonal.diff = diff(level.ts,12) #Monthly seasonal k = 12
plot.ts(seasonal.diff, main="Difference Level Data")
acf(seasonal.diff, na.action = na.pass, lag.max = 7 * 12, main = "ACF")
pacf(seasonal.diff, na.action = na.pass,lag.max = 7 * 12, main = "PACF")