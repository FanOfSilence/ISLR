rm(list=ls())

set.seed(1)
n = 100
x = rnorm(n)
y = 2*x*rnorm(n)

# Regress y onto x without an intercept (+0)
lm.fit = lm(y~x+0)

# coefficient estimate: -0.4508, Std. error: 0.1573, t-statistic value: -2.866, p-value: 0.00508
# With these values H0: Beta = 0 can be rejected

# Regress x onto y without an intercept (+0)
lm.fit_x_onto_y = lm(x~y+0)

# coefficient estimate: -0.1699, Std. error: 0.0593, t-statistic value: -2.866, p-value: 0.00508
# Coefficient seems to have a lower value but standard error has the same relative change giving again a very low p-value,
# so we can reject the null hypothesis

# The relationship between the models is that they both are determined by each other

par(mfrow = c(2,2))
plot(x, y)
plot(y, x)

# The t-statistic when not using an intercept
sum = (sqrt(n - 1) * sum(x*y)) / sqrt(sum(x*x) * sum(y*y) - sum(x*y) ^ 2)

# From the equation it doesn't matter which argument comes first so regressing y onto x is the same as regressing x onto y

# Grabbing the t-statistic from the lm function that has an intercept
t_statistic_lm = summary(lm.fit)[["coefficients"]][, "t value"]

# Make sure they are equal (disregarding floating-point errors)
stopifnot(all.equal(t_statistic_lm, sum))

