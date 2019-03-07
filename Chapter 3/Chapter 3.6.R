library(MASS)
library(ISLR)

#fix(Boston)
names(Boston)
attach(Boston)
lm.fit = lm(medv~lstat, Boston)

summary = summary(lm.fit)

confidence_interval = confint(lm.fit)

# Confidence interval 
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "confidence")

# Prediction interval
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval = "predict")

plot(lstat, medv)
#abline(lm.fit)


#abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
#plot(lstat,medv,col="red")
#plot(lstat,medv,pch=20)
#plot(lstat,medv,pch="+")
#plot(1:20,1:20,pch=1:20)

par(mfrow=c(2,2))
plot(lm.fit)

par(mfrow=c(2,2))
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

plot(age, nox)