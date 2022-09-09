# Set Working DIrectory
setwd("C:/Users/Garrett/Documents/Spring 21/MKT 4480")

# Open data file
dat<- read.csv("Professor Salary data.csv")

summary(lm(salary ~ yrs.since.phd, data=dat))
summary(lm(salary ~ yrs.service, data=dat))

dat$female <- ifelse(dat$sex == "Female", 1, 0)

summary(lm(salary ~ female, data=dat))

# Convert to factor
dat$sex <- factor(dat$sex)
dat$rank <- factor(dat$rank)
dat$discipline <- factor(dat$discipline)

# Code the catagorical columns
contrasts(dat$sex) <- contr.treatment(2, 1)
contrasts(dat$rank)

summary(lm(salary ~ sex, data=dat))
summary(lm(salary ~ rank, data=dat))

contrasts(dat$rank) <- contr.treatment(n=3, base=2)
summary(lm(salary ~ rank, data=dat))

contrasts(dat$discipline)
summary(lm(salary ~ discipline, data=dat))
