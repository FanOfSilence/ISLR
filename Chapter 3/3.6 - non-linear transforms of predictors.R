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

# lstat^2 is included
lm.fit_non_linear = lm(medv~lstat+I(lstat^2))

lm.fit = lm(medv~lstat, data = Boston)

# Compare the two models with hypothesis h0 is that the models fit the data equally well
h0 = anova(lm.fit, lm.fit_non_linear)


# In the non-linear model there is little discernible pattern
par(mfrow=c(2,2))
plot(lm.fit)
par(mfrow=c(2,2))
plot(lm.fit_non_linear)

# Fifth order polynomial with every level up to it
lm.fit5 = lm(medv~poly(lstat,5))

# Log transform
lm.fit_log = lm(medv~log(rm), data = Boston)
par(mfrow=c(2,2))
plot(lm.fit_log)