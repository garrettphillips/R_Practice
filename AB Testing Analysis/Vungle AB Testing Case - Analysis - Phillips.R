# Open Data File
setwd("C:/Users/Garrett/Documents/Spring 21/MKT 4480")
dat <- read.csv("Vungle AB Testing Case data.csv")

# Descriptive Statistics
## Overall column means
colMeans(dat[, 3:10])

## Means split by algorithm
aggregate(dat[, 3:10], by=list(Algorithm=dat$Strategy),FUN=mean)

# Agregate data by date, calculated performance of A vs. B
## Mine differs from the example in that I pasted my Algorithm B first in Excell rather than Algorithm A
agg.data <- aggregate(dat[, 3:10], by=list(Day=dat$Date), FUN=diff)

# Regressions
## eRPM
summary(lm(eRPM ~ 1, data = agg.data))
## Completion Rate
summary(lm(Completion.Rate ~ 1, data = agg.data))
## Click Through Rate
summary(lm(Click.Through.Rate ~ 1, data = agg.data))
## Conversion Rate
summary(lm(Conversion.Rate ~ 1, data = agg.data))

