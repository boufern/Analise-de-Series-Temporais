## Time series analysis for prediction - ARFIMA Model
# Date: 30/09/2022
# Author: Bruno Fernandes
# If you find any error, please contact bruno.fernandes03@outlook.com
# -------------------------------------------------------------------------------------

library(astsa)
library(lmtest)
library(fracdiff)
library(pracma)
library(arfima)
library(moments)

set.seed(251200)

# Simulating the ARFIMA model
serieSim = ts(arfima.sim(1000, model = list(phi = 0.3, dfrac = 0.45)))

# plotting the serie
plot(serieSim)

# plotting the ACF and PACF
acf2(serieSim)

# Fitting an ARFIMA model to the simulated serie
fit1 = arfima(serieSim, order = c(1,0,0), dmean = FALSE)
summary(fit1)

# Fitting an ARIMA model to the simulated serie
return = diff(serieSim)
acf2(return)

fit2 = arima(return, c(0,0,3), include.mean = FALSE)
fit2
coeftest(fit2)

# Comparing both fittings
#fit1
resid.fit1 = resid(fit1)$Mode1
plot(resid.fit1, type = "l")
acf2(resid.fit1)
qqnorm(resid.fit1)
qqline(resid.fit1)

test.fit1 = vector()
for (i in 1:36) {
  test.fit1[i] = Box.test(resid.fit1, lag = i, type = "Ljung-Box", fitdf = 0)$p.value
  
}
plot(test.fit1)

#fit2
resid.fit2 = fit2$residuals
plot(resid.fit2, type = "l")
acf2(resid.fit2)
qqnorm(resid.fit2)
qqline(resid.fit2)

test.fit2 = vector()
line = vector()
lag = 1:36
for (i in 1:36) {
  test.fit2[i] = Box.test(resid.fit2, lag = i, type = "Ljung-Box", fitdf = 0)$p.value
  line[i] = 0.05
}

plot(1:36, test.fit2)
lines(1:36, line, type="l", col="red" )

# Predictions

prediction = predict(fit1, n.ahead = 12)
prediction
plot(prediction, numback = 100)
