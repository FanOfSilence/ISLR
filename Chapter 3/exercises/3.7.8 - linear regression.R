library(ISLR)

attach(Auto)

lm.fit = lm(mpg~horsepower, Auto)

# Yes there is a relationship between predictor and response.
# Predictor horsepower has a slight negative slope for mpg
# The relationship is only -0.16 so not that strong

# Predict mpg based on horsepower = 98
prediction = predict(lm.fit, data.frame(horsepower=98))


confide_interval = predict(lm.fit, data.frame(horsepower=98), interval = "confidence")
prediction_interval = predict(lm.fit, data.frame(horsepower=98), interval = "prediction")

plot(horsepower, mpg)
abline(lm.fit)

par(mfrow=c(2,2))
plot(lm.fit)

# Residuals vs fitted shows non-linearity
