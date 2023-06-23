## Time series analysis for prediction - ARMA Model
# Date: 30/09/2022
# Author: Bruno Fernandes
# If you find any error, please contact bruno.fernandes03@outlook.com
# -------------------------------------------------------------------------------------

library(readxl)
library(astsa)
library(lmtest)
library(fracdiff)
library(pracma)
library(moments)


data = read_excel("~/Library/CloudStorage/GoogleDrive-bfernandes@usp.br/My Drive/Mestrado/Sétimo e Oitavo Bimestres/Análise Séries Temporais/Provas/Prova Prática I/prova 1- seires/prova 1-seire 05.xlsx", 
                  skip = 1)
serie = ts(data = data$Xt, start = 1948, frequency = 12)

# Estatisticas descritivas da serie original

plot(serie, type = 'l') # serie plot
mean(serie) # sample mean
var(serie) # sample variance
skewness(serie) # skewness coeficient
kurtosis(serie) # kurtosis coeficient
max(serie) # maximum
min(serie) # minimum
hist(serie) # histogram

acf2(serie)

#logreturn transformation 
logserie = log(serie)
logreturn = diff(logserie)


plot(logreturn, type = 'l') # serie plot
mean(logreturn) # sample mean
var(logreturn) # sample variance
skewness(logreturn) # skewness coeficient
kurtosis(logreturn) # kurtosis coeficient
max(logreturn) # maximum
min(logreturn) # minimum
hist(logreturn) # histogram

acf2(logreturn)

# rooting out the logreurns seasonal tendency 
seaslogreturn = diff(logreturn, lag = 12)

acf2(seaslogreturn, 35)

## Fit Choice 

# Fit 1 - SARIMA (3,0,0)x(0,0,1)[12] over the logreturns with no seasonal tendency 
fit1 = arima(seaslogreturn, order = c(3,0,0), 
             seasonal = list(order = c(0,0,1), period = 12), method = "ML")

acf2(fit1$residuals)
coeftest(fit1)
AIC(fit1)

# Fit 2 - SARIMA (2,0,0)x(0,0,1)[12] over the logreturns with no seasonal tendency
fit2 = arima(seaslogreturn, order = c(2,0,0), 
             seasonal = list(order = c(0,0,1), period = 12), fixed = c(0,NA,NA), include.mean = FALSE, method = "ML")

acf2(fit2$residuals)
coeftest(fit2)
AIC(fit2)

# Fit 3 - SARIMA (2,0,1)x(0,0,1)[12] over the logreturns with no seasonal tendency
fit3 = arima(seaslogreturn, order = c(2,0,1), 
             seasonal = list(order = c(0,0,1), period = 12), include.mean = FALSE, method = "ML")

acf2(fit3$residuals)
coeftest(fit3)
AIC(fit3)

# Fit 4 - SARIMA (2,0,2)x(0,0,1)[12] over the logreturns with no seasonal tendency
fit4 = arima(seaslogreturn, order = c(2,0,2), 
             seasonal = list(order = c(0,0,1), period = 12), include.mean = FALSE, method = "ML")

## Fit4 crashed ##


### Fit chosen: Fit 3 - SARIMA (2,0,1)x(0,0,1)[12] with no mean

test.fit2 = vector()
line = vector()
for (i in 1:48) {
  test.fit2[i] = Box.test(fit2$residuals, lag = i, type = "Ljung-Box", fitdf = 0)$p.value
  line[i] = 0.05
}

plot(1:48, test.fit2)
lines(1:48, line, type="l", col="red" )

jarque.bera.test(fit3$residuals)

test.fit3 = vector()
line = vector()
for (i in 1:48) {
  test.fit3[i] = Box.test(fit3$residuals, lag = i, type = "Ljung-Box", fitdf = 0)$p.value
  line[i] = 0.05
}

plot(1:48, test.fit3)
lines(1:48, line, type="l", col="red" )

tsdiag(fit3, gof.lag = 48)



# Serie forecast using tbe SARIMA (2,1,1)x(0,1,1)[12] model
fcast = arima(logserie, order = c(2,1,1), 
              seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")

qqnorm(fcast$residuals)
qqline(fcast$residuals)

acf2(fcast$residuals^2)

coeftest(fcast)

seas_fcast = forecast(fcast, h=12) #predictions
seas_fcast
plot(seas_fcast)