library(ISLR)

attach(Auto)

# Scatterplot matrix of Auto data
plot(Auto)

# Correlation matrix for all variables, exluding ones that are not a number
cor_matrix = cor(Auto[sapply(Auto, function(x) is.numeric(x))])

# Linear model fit with mpg as response and all variables except for name as predictors
lm.fit = lm(mpg~. -name, Auto)

# Yes, there is a relationship between predictors and response

# displacement, weight, year, and origin have low p-values under 0.05 and therefore are stastically significant
# predictors in this model (horsepower is no longer a predictor since it has a strong correlation with displacement and weight)

# For the year variable it seems that mpg is improving steadily over time

par(mfrow=c(2,2))
plot(lm.fit)

# Residuals vs fitted seems to indicate a bit of a higher degree polynomial
# The plots show some outliers. In Standardized residuals vs Leverage there is one outlier affecting the line A LOT.

lm.fit_interactions = update(lm.fit, mpg~. + horsepower:displacement + horsepower:weight + displacement:cylinders + weight:displacement)

par(mfrow=c(2,2))
plot(lm.fit_interactions)

# displacement:horsepower seems to have a small effect. Nothing else is significant
# Residuals vs Fitted plot has lost some of its higher polynomial look.

lm.fit_non_linear = update(lm.fit, mpg~. + I(horsepower^2) + I(displacement^2) + I(origin^2))

# horsepower and displacement seem to have a significance as non-linear terms while origin does not
par(mfrow=c(2,2))
plot(lm.fit_non_linear)

# The plots looks similar to the one with the interactions but now the Residuals vs Leverage is not so dependent on a single point

# Compare the two models with hypothesis h0 is that the models fit the data equally well
h0_interactions = anova(lm.fit, lm.fit_interactions)

h0_non_linear = anova(lm.fit, lm.fit_non_linear)

h0_compare = anova(lm.fit_interactions, lm.fit_non_linear)

# Both the more complicated models are better than the simpler model
# The model with interactions has a slightly lower sum of squares
