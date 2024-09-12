
length(unique(rsei_fed$submission.year))

ts_lbs <- ts(rsei_fed$rsei.modeled.pounds, start = 2011, end = 2022, freq = 1445)

plot(ts_lbs)


# remove trend
t <- as.vector(time(ts_lbs))
gnp.x <- as.numeric(ts_lbs)

# with a linear model
gnp.lm <- lm(gnp.x ~ t)
summary(gnp.lm)
gnp.trend <- fitted(gnp.lm)

par(mfrow=c(2,1))
plot(t, gnp.x, main='US GNP', type='l')
lines(t, gnp.trend, col='red')
plot(t, gnp.x - gnp.trend, type='l', main="Linear Detrended")
abline(h=0, lty=2)

# filtering
gnp.flt <- stats::filter(ts_lbs, filter = rep(1/5, 5))
par(mfrow=c(2,1))
plot(ts_lbs, main='US GNP', lwd=2)
lines(gnp.flt,col="red", lwd=2)
plot(ts_lbs-gnp.flt)



# autocorrelation
library(forecast)
Acf(ts_lbs, lag.max = 3)

# differenced ts
Acf(gnp.flt)

# automatic ARIMA
gnp.arima <- auto.arima(ts_lbs, trace = TRUE)











