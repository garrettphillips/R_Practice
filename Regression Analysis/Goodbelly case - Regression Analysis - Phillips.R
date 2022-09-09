# Open data file
setwd("")

# Access .csv file
dat<- read.csv("")

# Descriptives
## Examine levels of identifying variables
length(unique(dat$Store))
unique(dat$Store)

length(unique(dat$Region))
unique(dat$Region)

length(unique(dat$Date))
unique(dat$Date)

## Check if the data is balanced or not
table(dat$Store, dat$Date)
table(dat$Store, dat$Date) != 1
sum(table(dat$Store, dat$Date) != 1)

## Examine means of the columns
mean(dat$Units.Sold)
mean(dat$Average.Retail.Price)
mean(dat$Sales.Rep)
mean(dat$Endcap)
mean(dat$Demo)
mean(dat$Demo1.3)
mean(dat$Demo4.5)
mean(dat$Natural)
mean(dat$Fitness)

## Mean of sales across different stores
tapply(dat$Units.Sold, dat$Store, mean)
?tapply

## Mean of all numeric variables across different stores or different dates
aggregate(x = dat[,4:12], by=list(Store=dat$Store), FUN=mean)
aggregate(x = dat[,4:12], by=list(Date=dat$Date), FUN=mean)

# Examine plots
plot(x=dat$Average.Retail.Price, y=dat$Units.Sold,
     xlab="Average Price",
     ylab="Units Sold")

plot(x=jitter(dat$Sales.Rep), y=dat$Units.Sold,
     xlab="Sales Rep",
     ylab="Units Sold")

# Regressions
summary(lm(Units.Sold~Average.Retail.Price, data=dat))
mod_1<- (lm(Units.Sold~Average.Retail.Price, data=dat))
plot(mod_1)
summary(lm(Units.Sold~Sales.Rep, data=dat))
summary(lm(Units.Sold~Endcap, data=dat))
summary(lm(Units.Sold~Demo, data=dat))
summary(lm(Units.Sold~Demo1.3, data=dat))
summary(lm(Units.Sold~Demo4.5, data=dat))
summary(lm(Units.Sold~Natural, data=dat))
summary(lm(Units.Sold~Fitness, data=dat))
summary(lm(Units.Sold~Average.Retail.Price+Sales.Rep+Endcap+Demo+Demo1.3+Demo4.5+Natural+Fitness, data=dat))

# Finding correlations between all variables
round(cor(dat[,4:12]), 2)
