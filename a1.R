data('EuStockMarkets')
tsData = EuStockMarkets[,1]
plot(tsData)

components.ts = decompose(tsData)

plot(components.ts)

library("urca")

kpss_test = ur.kpss(tsData, type = c("tau"), lags = c("short"), 
                    use.lag = NULL)
print(summary(kpss_test))

tsstationary = diff(tsData, differences = 1)

plot(tsstationary)

timeseriesseasonalyadjusted = tsstationary - components.ts$seasonal

tsstationary = diff(timeseriesseasonalyadjusted, differences = 1)

plot(tsstationary)

acf(tsstationary, lag.max= 40)

pacf(tsstationary, lag.max= 40)

# sample test
fitARIMA = arima(tsData, order = c(1, 1, 1),
                 seasonal = list(order = c(1, 0, 0), period = 12),
                 method = 'ML')
res = fitARIMA$residuals
plot(res)

library(forecast)
model <- auto.arima(tsData, trace = TRUE)

plot(model$residuals)

predicted_values = forecast(model, h = 200,
                            level = c(99.5))

plot(predicted_values)