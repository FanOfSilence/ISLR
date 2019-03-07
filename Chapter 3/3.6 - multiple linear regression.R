library(MASS)
library(ISLR)

# Contains vif function
# vif = variance inflation factor
library(car)

oldw <- getOption("warn")
options(warn = -1)

#fix(Boston)
names(Boston)
attach(Boston)


# Fit medv on lstat and age
lm.fit = lm(medv~lstat+age)

# Fit medv on all predictors
lm.fit_all = lm(medv~., data = Boston)

variance_inflation = vif(lm.fit_all)

lm.fit_no_age = lm(medv~.-age, data = Boston)

par(mfrow=c(2,2))
plot(lm.fit_no_age)