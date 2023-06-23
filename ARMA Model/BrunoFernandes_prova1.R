library(moments)
library(readxl)
library(lmtest)
library(tseries)
library(forecast)
library(astsa)

data = read_excel("~/Library/CloudStorage/GoogleDrive-bfernandes@usp.br/My Drive/Mestrado/Sétimo e Oitavo Bimestres/Análise Séries Temporais/Provas/Prova Prática I/prova 1- seires/prova 1-seire 05.xlsx", 
                  skip = 1)
serie = ts(data = data$Xt, start = 1948, frequency = 12)

# Estatisticas descritivas da serie original

plot(serie, type = 'l') #plot da serie
mean(serie) #media amostral
var(serie) #variancia amostral
skewness(serie) #coeficiente de assimetria
kurtosis(serie) #coeficiente de curtose
max(serie) #maximo
min(serie) #minimo
hist(serie) #histograma

acf2(serie)

#transformaçao logretorno 
logserie = log(serie)
logreturn = diff(logserie)


plot(logreturn, type = 'l') #plot da série
mean(logreturn) #media amostral
var(logreturn) #variancia amostral
skewness(logreturn) #coeficiente de assimetria
kurtosis(logreturn) #coeficiente de curtose
max(logreturn) #maximo
min(logreturn) #minimo
hist(logreturn) #histograma

acf2(logreturn)

# Retirando tendencia sazonal dos logretornos
seaslogreturn = diff(logreturn, lag = 12)

acf2(seaslogreturn, 35)

## Fit modelo final 

# Fit 1 - SARIMA (3,0,0)x(0,0,1)[12] na serie de logretornos sem tendencia sazonal
fit1 = arima(seaslogreturn, order = c(3,0,0), 
              seasonal = list(order = c(0,0,1), period = 12), method = "ML")

acf2(fit1$residuals)
coeftest(fit1)
AIC(fit1)

# Fit 2 - SARIMA (2,0,0)x(0,0,1)[12] sem média na serie de logretornos sem tendencia sazonal
fit2 = arima(seaslogreturn, order = c(2,0,0), 
             seasonal = list(order = c(0,0,1), period = 12), fixed = c(0,NA,NA), include.mean = FALSE, method = "ML")

acf2(fit2$residuals)
coeftest(fit2)
AIC(fit2)

# Fit 3 - SARIMA (2,0,1)x(0,0,1)[12] sem média na serie de logretornos sem tendencia sazonal
fit3 = arima(seaslogreturn, order = c(2,0,1), 
             seasonal = list(order = c(0,0,1), period = 12), include.mean = FALSE, method = "ML")

acf2(fit3$residuals)
coeftest(fit3)
AIC(fit3)

# Fit 4 - SARIMA (2,0,2)x(0,0,1)[12] sem média na serie de logretornos sem tendencia sazonal
fit4 = arima(seaslogreturn, order = c(2,0,2), 
             seasonal = list(order = c(0,0,1), period = 12), include.mean = FALSE, method = "ML")

# Fit4 deu pau 


### modelo escolhido: Fit 3 - SARIMA (2,0,1)x(0,0,1)[12] sem média

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

#forecast com a série o modelo escolhido: SARIMA (2,1,1)x(0,1,1)[12]
fcast = arima(logserie, order = c(2,1,1), 
              seasonal = list(order = c(0,1,1), period = 12), include.mean = FALSE, method = "ML")


coeftest(fcast)

seas_fcast = forecast(fcast, h=12) #previsóes
seas_fcast
plot(seas_fcast)