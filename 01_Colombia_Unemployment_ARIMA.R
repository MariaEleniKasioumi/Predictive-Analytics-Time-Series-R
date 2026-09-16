library(forecast)
library(tseries)
library(ggplot2)
unemp <- c(
  11.990,11.724,11.807,11.147,10.581,11.596,
  10.886,10.935,11.325,10.638,10.925,10.975,
  11.153,10.265,9.750,10.683,10.403,9.622,
  9.512,9.569,9.774,10.095,10.388,10.699,
  10.282,10.572,10.969,10.597,10.221,10.573,
  9.874,9.966,9.673,9.981,9.458,9.746,
  9.442,9.356,9.353,8.752,8.981,8.823,
  8.816,8.882,8.655,8.968,8.162,8.539,
  8.805,8.371,8.556,8.753)

y <- ts(unemp,
        start = c(2022,1),
        frequency = 12)
plot(y,
     main = "Monthly Unemployment Rate in Colombia",
     xlab = "Year",
     ylab = "Percent",
     col = "blue",
     lwd = 2)

grid()

ggseasonplot(y) +
  ggtitle("Seasonal Plot")
par(mfrow = c(1,2))

acf(y,
    main = "ACF")

pacf(y,
     main = "PACF")

par(mfrow = c(1,1))

adf.test(y)

ndiffs(y)
d1 <- diff(y)

plot(d1,
     main = "First Differences",
     ylab = expression(Delta*"Unemployment"),
     col = "darkred",
     lwd = 2)

abline(h = 0,
       lty = 2)

grid()

par(mfrow = c(1,2))

acf(d1,
    main = "ACF First Differences")

pacf(d1,
     main = "PACF First Differences")

par(mfrow = c(1,1))

adf.test(d1)

ndiffs(d1)
fit <- auto.arima(y,
                  seasonal = FALSE,
                  stepwise = FALSE,
                  approximation = FALSE)

summary(fit)

ord <- arimaorder(fit)

cat("Επιλεγμένο μοντέλο: ARIMA(",
    ord[1], ",", ord[2], ",", ord[3], ")\n")
m010 <- Arima(y, order = c(0,1,0))
m110 <- Arima(y, order = c(1,1,0))
m011 <- Arima(y, order = c(0,1,1))
m111 <- Arima(y, order = c(1,1,1))

comparison <- data.frame(
  
  Model = c("ARIMA(0,1,0)",
            "ARIMA(1,1,0)",
            "ARIMA(0,1,1)",
            "ARIMA(1,1,1)",
            "auto.arima"),
  
  AIC = c(m010$aic,
          m110$aic,
          m011$aic,
          m111$aic,
          fit$aic),
  
  BIC = c(BIC(m010),
          BIC(m110),
          BIC(m011),
          BIC(m111),
          BIC(fit))
)

comparison
if(adf.test(d1)$p.value > 0.05){
  
  d2 <- diff(d1)
  
  plot(d2,
       main = "Second Differences",
       col = "purple",
       lwd = 2)
  
  grid()
  
  par(mfrow = c(1,2))
  
  acf(d2,
      main = "ACF")
  
  pacf(d2,
       main = "PACF")
  
  par(mfrow = c(1,1))
  
  adf.test(d2)
  
}else{
  
  cat("Οι πρώτες διαφορές είναι στάσιμες.\n")
  
}
checkresiduals(fit)

Box.test(residuals(fit),
         lag = 12,
         type = "Ljung-Box",
         fitdf = ord[1] + ord[3])
fitAR <- tryCatch(
  
  Arima(y,
        order = c(ord[1] + 1,
                  ord[2],
                  ord[3])),
  
  error = function(e) NULL
)

fitMA <- tryCatch(
  
  Arima(y,
        order = c(ord[1],
                  ord[2],
                  ord[3] + 1)),
  
  error = function(e) NULL
)

cat("AIC βασικού μοντέλου:",
    round(fit$aic,3), "\n")

if(!is.null(fitAR))
  cat("AIC +AR:",
      round(fitAR$aic,3), "\n")

if(!is.null(fitMA))
  cat("AIC +MA:",
      round(fitMA$aic,3), "\n")
forecast24 <- forecast(fit, h = 24)

print(forecast24)

autoplot(forecast24) +
  ggtitle("Forecast of Colombian Unemployment Rate") +
  xlab("Year") +
  ylab("Unemployment Rate (%)")
data.frame(
  
  Forecast = forecast24$mean,
  
  Lower80 = forecast24$lower[,1],
  Upper80 = forecast24$upper[,1],
  
  Lower95 = forecast24$lower[,2],
  Upper95 = forecast24$upper[,2]
  
)
cat("\n=====================================\n")

cat("Τελικό μοντέλο:\n")

print(fit)

cat("\nAIC :", AIC(fit))
cat("\nBIC :", BIC(fit))

cat("\n=====================================\n")


