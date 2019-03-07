library(MASS)
library(ISLR)

# Contains vif function
# vif = variance inflation factor
library(car)

oldw <- getOption("warn")
options(warn = -1)

#fix(Boston)
names(Carseats)
attach(Carseats)

# Interaction terms
lm.fit = lm(Sales~.+Income:Advertising + Price:Age + Price:ShelveLoc, data = Carseats)

# Show dummy variables for qualitative predictors
dummies = contrasts(ShelveLoc)