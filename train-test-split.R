##Pre-process data. Split into train and test

house.train = house[c(1:160)]
house.test = house[-c(1:160)]
time.pts = (c(1:length(house)) - min(1:length(house)))/max(1:length(house))
train.time.pts = time.pts[c(1:160)]
test.time.pts = time.pts[-c(1:160)]