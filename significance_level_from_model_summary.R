# Harmonic/Cosine-Sine Fit
level = data[,2]; 
level = level[1:600]
H = harmonic(ts(level, start = 1/12, end = 50, deltat = 1/12), 2)

#model.summary to check significance level
time.pts = c(1:length(level))
time.pts = c(time.pts-min(time.pts))/max(time.pts)
erie.np = gam(level~s(time.pts)+H)
summary(erie.np)