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

# Add both the individual predictors lstat and age, and their interaction
# Same as lstat+age+lstat:age
sum = summary(lm(medv~lstat*age, data = Boston))

