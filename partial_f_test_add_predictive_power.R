

## Q2
time.pts = c(1:length(HFS.ts))
time.pts = c(time.pts - min(time.pts))/max(time.pts)
log.house = log(HFS.ts)
with.season= lm(log.house~time.pts + har)
no.season = lm(log.house~time.pts)

anova(with.season,no.season)
#The p-value from anova analysis is very large, indicating that the incremental gain in explanatory power from adding seasonality components is not statistically significant. 
#The seasonal terms unlikely add predictive power