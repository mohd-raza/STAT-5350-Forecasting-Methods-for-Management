data <- read.csv('~/Desktop/Fall 24 Materials/STATS 535/HWS/HW3/MRTSSM4453USN.csv')

View(data)

library(lubridate)

sales <- ts(data$MRTSSM4453USN, start=1992,end=2023+11/12,frequency=12)
dates <- lubridate::ymd(data$DATE[1:length(sales)])

#1
plot(sales, main = "Raw Sales Data", ylab = "Sales (in millions)", xlab = "Time", col = "blue", type = "l")

med <- matrix(sales,ncol = 12, byrow =TRUE)
medpol <-medpolish(med,trace=TRUE)
plot(medpol)
residuals_vector <- as.vector(medpol$residuals)
plot(residuals_vector, main = "Residuals from Median Polish", type = "p", xlab = "Index", ylab = "Residuals")



#2

sales.stl <- stl(log(sales), s.window=9)
sales.stl
trend_component <- sales.stl$time.series[, "trend"]
seasonal_component <- sales.stl$time.series[, "seasonal"]
remainder_component <- sales.stl$time.series[, "remainder"]

adjusted_sales_component <- trend_component + remainder_component
adjusted_sales_ <- exp(adjusted_sales_component)

plot(sales, col = "blue", type = "l", ylab = "Sales (in millions)", xlab = "Time", main = "Original vs Seasonally Adjusted Sales")
lines(adjusted_sales_, col = "red")

# Add a legend to differentiate the two lines
legend("topleft", legend = c("Original Sales", "Seasonally Adjusted Sales"), col = c("blue", "red"), lty = 1)



#3

# Corrected plot function
plot(trend_component, col = 'blue', type = 'l', ylab = 'Log(Trend Component)', xlab = 'Time', main = 'Trend Component of Sales (Log Scale)')

abline(v=2020 + 2/12, col='red', lty=2)
     
#4
pre_covid_seasonal <- window(seasonal_component, start = c(2018, 1), end = c(2019, 12))
post_covid_seasonal <- window(seasonal_component, start = c(2022, 1), end = c(2023, 12))
par(mfrow = c(1, 2))
print(pre_covid_seasonal)
print(post_covid_seasonal)
matplot(t(matrix(pre_covid_seasonal, ncol = 12, byrow = TRUE)), type = "l", col = 1:2, lty = 1, xlab = "Month", ylab = "Seasonal Component", main = "Pre-Covid (2018-2019)")
legend("topleft", legend = c("2018", "2019"), col = 1:2, lty = 1)

matplot(t(matrix(post_covid_seasonal, ncol = 12, byrow = TRUE)), type = "l", col = 3:4, lty = 1, xlab = "Month", ylab = "Seasonal Component", main = "Post-Covid (2022-2023)")
legend("topleft", legend = c("2022", "2023"), col = 3:4, lty = 1)


#5

count_weekdays <- function(from, to) {
  sum(!wday(seq(from, to, "days")) %in% c(1,7)) }
next.date <- c(dates[-1], ymd("2024-01-01"))-1
wdays <- mapply(count_weekdays, dates, next.date)
ndays <- lubridate::days_in_month(dates)
library(dynlm)
remain <- window(sales.stl$time.series[,'remainder'], end=2018.99)
n <- length(remain)
wdays <- wdays[1:n]
ndays <- ndays[1:n]
month <- factor(rep(month.abb, n/12), levels=month.abb)
regr <- dynlm(remain ~ month + ndays + wdays)

summary(regr)


suppressPackageStartupMessages({
  library(astsa);
  library(lubridate); 
  library(car);
  library(dynlm)
})

fit_arma_models <- function(xt, max.p=5, max.q=5, verbose=TRUE) {
  logLike <- matrix(NA, nrow=1+max.p, ncol=1+max.q)
  AIC     <- matrix(NA, nrow=1+max.p, ncol=1+max.q)
  BIC     <- matrix(NA, nrow=1+max.p, ncol=1+max.q)
  rownames(AIC) <- rownames(BIC) <- rownames(logLike) <- paste0("p=",0:max.p)
  colnames(AIC) <- colnames(BIC) <- colnames(logLike) <- paste0("q=",0:max.q)
  for (p in 0:max.p) {
    if (verbose) cat("\np=",p)
    for (q in 0:max.q) {
      z <- arima(xt, order=c(p,0,q))      # sarima from astsa decorates `arima`
      logLike[p+1,q+1] <- z$loglik
      AIC[p+1,q+1]     <- z$aic
      BIC[p+1,q+1]     <- z$aic + (1+p+q)*(log(length(xt)) - 2)
    }
  }
  return( list(ll=logLike, aic=AIC-min(AIC), bic=BIC-min(BIC)) )
}

#6



a3_series <- read_csv("Desktop/Fall 24 Materials/STATS 535/HWS/HW3/a3_series.csv")
View(a3_series)

ts.6 <- ts(a3_series[,2])
ts.7 <- ts(a3_series[,3])
ts.8 <- ts(a3_series[,4])



# Set the maximum lag
# Visualize ACF and PACF for ts.6
acf2(ts.6)

z <- fit_arma_models(ts.6)

# Print AIC and BIC to identify the best model
round(z$aic, 3)  # AIC values

round(z$bic, 3)  # BIC values
sarima(ts.6, p=2,d=0,q=0,details = FALSE)
sarima(ts.6, p=4,d=0,q=1,details = FALSE)
sarima(ts.6, p=2,d=0,q=1, details = FALSE)



#7

# Set the maximum lag
# Visualize ACF and PACF for ts.6
acf2(ts.7)

z_7 <- fit_arma_models(ts.7)

# Print AIC and BIC to identify the best model
round(z$aic, 3)  # AIC values

round(z$bic, 3)  # BIC values

#8

# Set the maximum lag

acf2(ts.8)
z_8 <- fit_arma_models(ts.8)

# Print AIC and BIC to identify the best model
round(z_8$aic, 3)  # AIC values

round(z_8$bic, 3)  # BIC values

sarima(ts.7, p=1, d=0, q=2, details=FALSE)
sarima(ts.7, p=1, d=0, q=3, details=FALSE)
sarima(ts.7, p=0, d=0, q=2, details=FALSE)

# Fit both models using the sarima function
arma_22 <- sarima(ts.7, p=1, d=0, q=2, details=FALSE)  # Based on visual inspection (ARMA(2, 2))
arma_21 <- sarima(ts.7, p=0, d=0, q=2, details=FALSE)  # Based on BIC (ARMA(2, 1))

# Compare the AIC, BIC, and log-likelihood for both models
cat("AIC for ARMA(1, 2):", arma_22$fit$aic, "\n")
cat("AIC for ARMA(0, 2):", arma_21$fit$aic, "\n")

cat("BIC for ARMA(1, 2):", arma_22$fit$bic, "\n")
cat("BIC for ARMA(0, 2):", arma_21$fit$bic, "\n")

cat("Log-likelihood for ARMA(2, 2):", arma_22$fit$loglik, "\n")
cat("Log-likelihood for ARMA(2, 1):", arma_21$fit$loglik, "\n")


sarima(ts.7, p=1, d=0, q=2, details=FALSE)
sarima(ts.7, p=0, d=0, q=2, details=FALSE)


#9
set.seed(54)
xt <- arima.sim(n = 800, list(ar = c(1.6, -0.8)), sd = 1) 
acf2(xt)

sarima(xt, p=2,d=0,q=0,details=FALSE)

set.seed(62)
yt <- xt + rnorm(length(xt))
acf2(yt)

z <- fit_arma_models(yt)
round(z$aic, 3)
round(z$bic, 3)

sarima(yt, p=2,d=0,q=2,details = FALSE)


#10
#A)

AR <- c(1, -0.80, 0.15)
ar_roots <- polyroot(AR)
print(ar_roots)

MA <- c(1, -0.30)
ma_roots <- polyroot(MA)
print(ma_roots)



set.seed(123)
xt <- arima.sim(n = 1000, model = list(ar = c(0.8,-0.15), ma = c(-0.3)))

sarima(xt, p = 2, d = 0, q = 1,details = FALSE)

sarima(xt, p = 1, d = 0, q = 0,details = FALSE)



AR <- c(1, -1, 0.50)
ar_roots <- polyroot(AR)
print(ar_roots)

MA <- c(1, -1)
ma_roots <- polyroot(MA)
print(ma_roots)

set.seed(123)
yt <- arima.sim(n = 1000, model = list(ar = c(1,-0.5), ma = c(-1)))

sarima(yt, p = 2, d = 0, q = 1,details = FALSE)

sarima(yt, p = 1, d = 0, q = 0,details = FALSE)


AR <- c(1, -0.8)
ar_roots <- polyroot(AR)
print(ar_roots)

AR <- c(1, -1, 0.50)
ar_roots <- polyroot(AR)
print(ar_roots)

