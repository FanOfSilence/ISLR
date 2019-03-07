rm(list=ls())
library(ISLR)

attach(Carseats)

# Linear model fit with Sales as response and Price, Urban, US as predictors
lm.fit = lm(Sales~Price+Urban+US, data = Carseats)

# Price: p-value shows it statistical significance. Slightly negative => the higher the price, the less sales
# UrbanYes: Not statistically significant
# UsYes: Statistically significant where the sales seem to be higher if it is from America (or it is sold in America?)

# Sales = 13.043469 + (-0.054459)*Price + 0 * Urban + 1.200573 * US
#     where Urban = 0 or 1 and US = 0 or 1

# Null hypothesis H0: Bj = 0 can be rejected for the intercept, Price, and USYes

# Skip Urban since can't reject null hypothesis
lm.fit_no_urban = lm(Sales~Price+US, data = Carseats)

h0 = anova(lm.fit, lm.fit_no_urban)

# They fit the data equally badly (only around 0.23 is explained for R-squared)

# Confindence intervals for the parameters
confide_interval = confint(lm.fit_no_urban)

par(mfrow=c(2,2))
plot(lm.fit_no_urban)

# Looking at the plots there are some outliers but none have that high leverage