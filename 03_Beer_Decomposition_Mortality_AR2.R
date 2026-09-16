library(astsa)
library(TTR)
x <- cmort

x1 <- stats::lag(cmort,-1)
x2 <- stats::lag(cmort,-2)

dat <- ts.intersect(x,x1,x2,dframe=TRUE)
reg <- lm(x ~ x1 + x2, data = dat)

summary(reg)
fit <- ar.ols(cmort,
              order.max = 2,
              aic = FALSE,
              demean = FALSE,
              intercept = TRUE)

fit

fit$asy.se.coef
h <- 4

fc <- predict(fit,
              n.ahead = h)

pred <- fc$pred
se <- fc$se

lower <- pred - 1.96*se
upper <- pred + 1.96*se

forecast.table <- data.frame(
  
  Week = 1:h,
  
  Forecast = round(as.numeric(pred),3),
  
  SE = round(as.numeric(se),3),
  
  Lower95 = round(as.numeric(lower),3),
  
  Upper95 = round(as.numeric(upper),3)
  
)

forecast.table


plot(cmort,
     main = "AR(2) Forecast",
     ylab = "Mortality")

n <- length(cmort)

points((n+1):(n+4),
       pred,
       col = "red",
       pch = 16)

arrows((n+1):(n+4),
       lower,
       (n+1):(n+4),
       upper,
       angle = 90,
       code = 3,
       length = 0.05,
       col = "blue")
beer <- ts(
  
  c(
    
    1,3,6,4,
    
    2,2,7,5,
    
    2,4,8,5,
    
    1,3,8,6
    
  ),
  
  start = c(2016,1),
  
  frequency = 4)

plot(beer,
     type="o",
     pch=16,
     main="Beer Sales",
     ylab="Million Bottles")

grid()
cma <- filter(
  
  beer,
  
  c(1/8,1/4,1/4,1/4,1/8),
  
  sides = 2)

cma <- ts(as.numeric(cma),
          
          start = c(2016,1),
          
          frequency = 4)
si <- beer/cma

quarter <- cycle(beer)

raw.index <- tapply(si,
                    
                    quarter,
                    
                    mean,
                    
                    na.rm=TRUE)

season.index <- raw.index*(4/sum(raw.index))

season.index
season.full <- ts(
  
  season.index[quarter],
  
  start=c(2016,1),
  
  frequency=4)

deseason <- beer/season.full

plot(beer,
     type="o",
     col="grey")

lines(deseason,
      type="o",
      col="blue")
t <- 1:length(beer)

trend <- lm(as.numeric(deseason) ~ t)

summary(trend)

trend.line <- ts(
  
  fitted(trend),
  
  start=c(2016,1),
  
  frequency=4)

lines(trend.line,
      col="red",
      lwd=2)
cycle.component <- (cma/trend.line)*100

cycle.component

plot(cycle.component,
     type="o",
     pch=16,
     col="darkorange",
     main="Cyclical Component")

abline(h=100,
       lty=2)

grid()
dec <- decompose(beer,
                 type="multiplicative")

plot(dec)
