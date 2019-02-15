library(TSA)
library(tseries)
library(forecast)

bos = read.csv("C:\\OSU\\Sem3\\STAT 5053\\Project\\ts_boston_crime.csv", sep = ',', header = T)
bos.ts = ts(df, start = c(2015,7), frequency = 12)
plot(bos.ts, main = "Number of crime incidents in Boston from the year 2015 to 2018", xlab = "Time (years)", ylab = "Number of incidents")
acf(bos.ts, lag.max = 30, main = "ACF plot of the original Boston crime data") #seasonality is present

adf.test(bos.ts, k=7)

BoxCox.ar(bos.ts) #use original data

acf(diff(bos.ts), lag.max = 30,main = "ACF plot of the differenced original data")

bos.sa.diff = diff(df.ts, 12) #seasonal difference

plot(bos.sa.diff, main = 'Plot after taking the seasonal difference', xlab = "Time (years)") #downward trend
adf.test(bos.sa.diff) #not stationary
acf(bos.sa.diff, main = "ACF plot of the seasonal differenced data")

bos.diff = diff(bos.sa.diff) #normal difference to make data stationary
plot(bos.diff, main = "Plot of the differenced seasonally adjusted data", xlab="Time (years)")

adf.test(bos.diff, k = 0)
acf(bos.diff, main = "ACF plot of the differenced seasonally adjusted data")
pacf(bos.diff, main = "PACF plot of the differenced seasonally adjusted data")
eacf(bos.diff, 5,5) #ARMA(0,0)
plot(armasubsets(bos.diff, nar = 4, nma = 4)) #MA(3)

#Models

model = arima(bos.ts, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
arima(bos.ts, order = c(1,1,0), seasonal = list(order = c(0,1,0), period = 12))
confint(arima(bos.ts, order = c(1,1,0), seasonal = list(order = c(0,1,0), period = 12)))

arima(bos.ts, order = c(0,1,1), seasonal = list(order = c(0,1,0), period = 12))
confint(arima(bos.ts, order = c(0,1,1), seasonal = list(order = c(0,1,0), period = 12)))

arima(bos.ts, order = c(1,1,1), seasonal = list(order = c(0,1,0), period = 12))
confint(arima(bos.ts, order = c(1,1,1), seasonal = list(order = c(0,1,0), period = 12)))

#Residual Analysis
plot(rstandard(model), type='o', main = "Checking for the constant mean and variance")
shapiro.test(rstandard(model))
qqnorm(rstandard(model), main = "Checking for normality")
qqline(rstandard(model))

acf(rstandard(model), main = "ACF plot of the residuals")
tsdiag(model, 30)

#Outlier detection

detectAO(model)
detectIO(model)

#Forecasting

get <- function (ts1,period){
  fit <- arima(ts1, order = c(0,1,0), seasonal = list(order = c(0,1,0), period = 12))
  fit$x <- ts1
  return(forecast(fit,period))
}

predictions = get(bos.ts, 24)

plot(predictions, main = "Forecast of the Boston Crime Incidents", xlab = "Time (Years)", ylab = "Number of Incidents")

predictions
