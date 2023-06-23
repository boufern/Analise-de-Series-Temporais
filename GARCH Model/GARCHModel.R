## Time series analysis for prediction - ARMA Model
# Date: 30/09/2022
# Author: Bruno Fernandes
# If you find any error, please contact bruno.fernandes03@outlook.com
# -------------------------------------------------------------------------------------


library(readxl)
library(astsa)
library(fGarch)
library(rugarch)
library(lmtest)
library(fracdiff)
library(pracma)
library(moments)

set.seed(251200)

# Modeling the first moment
data = read_excel("~/Downloads/MAE518-5882-series prova2.xlsx", sheet = 20)$x

plot(data, type = "l")
mean(data)
var(data)
skewness(data)
kurtosis(data)
max(data)
min(data)

acf2(data)

mod1 = arima(data, c(1,0,0), include.mean = FALSE)
coeftest(mod1)

resid.mod1 = mod1$residuals
acf2(resid.mod1)
qqnorm(resid.mod1)
qqline(resid.mod1)

test.mod1 = vector()
for (i in 1:48) {
  test.mod1[i] = Box.test(resid.mod1, lag = i, type = "Ljung-Box", fitdf = 0)$p.value
  
}
plot(test.mod1)

# Modeling the second moment

acf2(resid.mod1^2)

# fit 1
especifica1 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(1,0)),
                         distribution.model="norm", fixed.pars = list(mu=0))
modGarch1 = ugarchfit(especifica1, data)
modGarch1

# fit 2 
especifica2 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,0)),
                         mean.model=list(armaOrder=c(1,0)),
                         distribution.model="norm", fixed.pars = list(mu=0))

modGarch2 = ugarchfit(especifica2, data)
modGarch2

# fit 3
especifica3 = ugarchspec(variance.model=list(model="sGARCH", garchOrder=c(1,1)),
                         mean.model=list(armaOrder=c(1,0)),
                         distribution.model="norm", 
                         fixed.pars = list(mu=0, omega=0))

modGarch3 = ugarchfit(especifica3, data)
modGarch3


rugarch::plot(modGarch2, which=11)

#escolha entre TGARCHs ou EGARCH
# fit 4
especifica4 =  ugarchspec(variance.model=list(model="fGARCH", garchOrder=c(1,0), submodel = "TGARCH"),
                          mean.model=list(armaOrder=c(1,0)),
                          distribution.model="norm", fixed.pars = list(mu=0))

modGarch4 = ugarchfit(especifica4, data)
modGarch4 

# fit 5
especifica5 = ugarchspec(variance.model=list(model="gjrGARCH", garchOrder=c(1,0)),
                         mean.model=list(armaOrder=c(1,0)),
                         distribution.model="norm", fixed.pars = list(mu=0))

modGarch5 = ugarchfit(especifica5, resid.mod1)
modGarch5

# fit 6
especifica6 = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,0)),
                         mean.model=list(armaOrder=c(1,0)),
                         distribution.model="norm", fixed.pars = list(mu=0))

modGarch6 = ugarchfit(especifica6, resid.mod1)
modGarch6 

rugarch::plot(modGarch2, which=9)
rugarch::plot(modGarch2, which=10)
rugarch::plot(modGarch2, which=11)

# Serie forecast using tbe ARMA(1,0) + GARCH(1,0) model

model.forecast = ugarchforecast(modGarch2, n.ahead=12)
model.forecast
rugarch::plot(model.forecast)
