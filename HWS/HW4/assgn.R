# Load necessary libraries
library(readr)      # For reading the CSV file
library(ggplot2)    # For plotting
library(lubridate)  # For handling dates

# Load data
data <- read_csv("Desktop/Fall 24 Materials/STATS 535/HWS/HW4/a4_mg_price_23-24.csv", show_col_types = FALSE)
data_2023 <- data[data$date >= ymd("2023-01-02"), ]
data$date <- ymd(data_2023$date)
plot(data_2023$date, data_2023$price, type = "l", col = "blue",
     main = "Weekly Gasoline Prices (2023-2024)",
     xlab = "Date", ylab = "Price (in dollars per gallon)")
mean(data_2023$price)
var(data_2023$price)
suppressPackageStartupMessages({
  library(astsa);
  library(lubridate); 
  library(data.table);
  library(car);
  library(dynlm)
})
price_2023<-data_2023$price
acf2(price_2023)
diff_price <- diff(data_2023$price)

plot(data_2023$date[-1], diff_price, type = "l", col = "red",
     main = "Differenced Weekly Gasoline Prices (2023-2024)",
     xlab = "Date", ylab = "Differenced Price")
acf2(diff_price)
price_2023
adf_test <- adf.test(price_2023, alternative = "stationary")
print(adf_test)

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

price <- data_2023$price
acf2(price)

results <- fit_arma_models(price)
results$aic
results$bic


#3

# Assuming `data_2023$price` contains the price data
price_ts <- ts(data_2023$price, start = 1)

# Fit AR(2) model using MLE
mle_model <- arima(price_ts, order = c(2, 0, 0))
mle_model
mle_coefficients <- mle_model$coef
mle_se <- sqrt(diag(mle_model$var.coef))

# Display MLE estimates and standard errors
mle_results <- cbind(mle_coefficients, mle_se)
colnames(mle_results) <- c("Estimate (MLE)", "Std. Error (MLE)")
print(mle_results)


ls_model <- dynlm(price_ts ~ L(price_ts, 1) + L(price_ts, 2))
ls_model
# Extract LS coefficients and standard errors
ls_summary <- summary(ls_model)
ls_summary
ls_coefficients <- coef(ls_summary)
ls_results <- cbind(ls_coefficients[, "Estimate"], ls_coefficients[, "Std. Error"])
colnames(ls_results) <- c("Estimate (LS)", "Std. Error (LS)")
print(ls_results)

# Check the number of parameters in each model result
num_mle <- nrow(mle_results)
num_ls <- nrow(ls_results)

# Adjust the Method and Parameter vectors based on the number of parameters
comparison_results <- data.frame(
  "Method" = c(rep("MLE", num_mle), rep("LS", num_ls)),
  "Parameter" = c(rownames(mle_results), rownames(ls_results)),
  "Estimate" = c(mle_results[, 1], ls_results[, 1]),
  "Std.Error" = c(mle_results[, 2], ls_results[, 2])
)

print(comparison_results)

#4
# Run SARIMA diagnostics on the fitted AR(2) model
sarima(price_ts, p = 2, d = 0, q = 0)
plot(ls_model)

# Assuming `ls_model` is your fitted dynlm model
par(mfrow = c(2, 2))  # Set up the 2x2 plotting area

# Plot the four diagnostic plots for dynlm
plot(ls_model, which = 1:4)  # Using 'which = 1:4' gives the first four diagnostic plots

# Reset the plotting layout
par(mfrow = c(1, 1))  # Reset to default (single plot layout)


#5
# Assuming `price_ts` is the time series of weekly gasoline prices
forecast_results <- sarima.for(price_ts, n.ahead = 36, p = 2, d = 0, q = 0, main = "36-Week Forecast for Weekly Gasoline Prices using BIC")
# Optional: Add a horizontal line for the estimated mean
abline(h = ar_model$fit$coef["xmean"], col = "blue", lty = 2)

forecast_values <- forecast_results$pred
forecast_se <- forecast_results$se

# Create a data frame to display each week’s forecasted value and standard error
weeks <- seq(1, 36)
forecast_df <- data.frame(
  Week = weeks,
  Forecast = forecast_values,
  Std_Error = forecast_se
)

# Display the data frame
print(forecast_df)

#6
mean_price <- mean(price_ts)
print(mean_price)

t_test <- t.test(price_ts, mu = 0)
print(t_test)


ar_model <- sarima(price_ts, p = 2, d = 0, q = 0,details=FALSE)
phi_1 <- ar_model$fit$coef["ar1"]
phi_2 <- ar_model$fit$coef["ar2"]
mu <- ar_model$fit$coef["xmean"]

price_ts

X_n <- tail(price_ts, 1)             # Most recent observation
X_n_minus_1 <- tail(price_ts, 2)[1]  # Second most recent observation
intercept <- mu*(1-phi_1-phi_2)
intercept
# Week 1 forecast (2024-09-23)
X_n_plus_1 <- intercept + phi_1 * X_n + phi_2 * X_n_minus_1

# Week 2 forecast (2024-09-30)
X_n_plus_2 <- intercept + phi_1 * X_n_plus_1 + phi_2 * X_n

# Week 3 forecast (2024-10-07)
X_n_plus_3 <- intercept + phi_1 * X_n_plus_2 + phi_2 * X_n_plus_1

forecast_values <- c(X_n_plus_1, X_n_plus_2, X_n_plus_3) #
names(forecast_values) <- c("2024-09-23", "2024-09-30", "2024-10-07")
print(forecast_values)

forecast_values_auto <- forecast_results$pred[1:3]
print(forecast_values_auto)


#7

# Extract the residual variance (sigma^2) from the model
ar_model
sigma2 <- ar_model$fit$sigma2
sigma2
sigma <- sqrt(sigma2)  # Standard deviation (sigma)
sigma


# Step 1: One-step-ahead forecast standard error
var_forecast_1 <- sigma2  # Variance for one step
se_forecast_1 <- sqrt(var_forecast_1)

# Step 2: Two-step-ahead forecast standard error
var_forecast_2 <- sigma2 * (1 + phi_1^2)
se_forecast_2 <- sqrt(var_forecast_2)

# Step 3: Three-step-ahead forecast standard error
var_forecast_3 <- sigma2 * (1 + phi_1^2 + (phi_1^2 + phi_2)^2)
se_forecast_3 <- sqrt(var_forecast_3)


# Display the results
standard_errors <- c(se_forecast_1, se_forecast_2, se_forecast_3)
names(standard_errors) <- c("2024-09-23", "2024-09-30", "2024-10-07")
print(standard_errors)

forecast_se_auto <- forecast_results$se[1:3]
print(forecast_se_auto)

#8

forecasted_values <- forecast_results$pred[1:5]
print(forecasted_values)

observed_prices <- c(3.074, 3.087, 3.026, 3.069, 3.044)

forecast_errors <- observed_prices - forecasted_values

comparison_df <- data.frame(
  Week = c("2024-09-23", "2024-09-30", "2024-10-07", "2024-10-14", "2024-10-21"),
  Observed = observed_prices,
  Forecasted = forecasted_values,
  Error = forecast_errors
)

comparison_df


forecasted_values <- forecast_results$pred[1:5]
print(forecasted_values)

observed_prices <- c(3.074, 3.087, 3.026, 3.069, 3.044)

forecast_errors <- observed_prices - forecasted_values

comparison_df <- data.frame(
  Week = c("2024-09-23", "2024-09-30", "2024-10-07", "2024-10-14", "2024-10-21"),
  Observed = observed_prices,
  Forecasted = forecasted_values,
  Error = forecast_errors,
  SE_of_Forecasted_Values = forecast_results$se[1:5]
)

print(comparison_df)





#9
#AIC
results
aic_model <- sarima(price_ts, p=2,d=0,q=2)

aic_forecast <- sarima.for(price_ts, n.ahead = 36, p = 2, d = 0, q = 2, main = "36-Week Forecast for Weekly Gasoline Prices using AIC")
abline(h = aic_model$fit$coef["xmean"], col = "blue", lty = 2)

ar2_forecast_values <- forecast_results$pred
aic_forecast_values <- aic_forecast$pred

comparison_df <- data.frame(
  Week = 1:36,
  AR2_Forecast = ar2_forecast_values,
  ARMA22_Forecast = aic_forecast_values
)

comparison_df$Difference <- comparison_df$AR2_Forecast - comparison_df$ARMA22_Forecast

print(comparison_df)

plot(1:36, ar2_forecast_values, type = "l", col = "red", lty = 1, ylim = range(c(ar2_forecast_values, aic_forecast_values)),
     ylab = "Forecasted Price", xlab = "Weeks Ahead", main = "Forecast Comparison: AR(2) vs. ARMA(2,2)")
lines(1:36, aic_forecast_values, col = "blue", lty = 2)
legend("topright", legend = c("AR(2) Model", "ARMA(2,2) Model"), col = c("red", "blue"), lty = c(1, 2))

#10

data
data$date <- ymd(data$date)

# Convert to time series object for analysis
price_ts_long <- ts(data$price, frequency = 52, start = c(2020, 1))

# Plot the sequence of weekly prices
plot(data$date, data$price, type = "l", col = "blue",
     main = "Weekly Gasoline Prices (2020-2024)",
     xlab = "Date", ylab = "Price (in dollars per gallon)")

# ACF and PACF plots for the longer time series
x <- acf2(price_ts_long, main = "ACF and PACF of Weekly Gasoline Prices (2020-2024)")

# First difference the series if non-stationary
diff_price_ts_long <- diff(price_ts_long)

# Plot the differenced series to check for stationarity
plot(data$date[-1], diff_price_ts_long, type = "l", col = "red",
     main = "Differenced Weekly Gasoline Prices (2020-2024)",
     xlab = "Date", ylab = "Differenced Price")

# ACF and PACF for the differenced series
acf2(diff_price_ts_long, main = "ACF and PACF of Differenced Series")

install.packages("tseries")
# Load the tseries package
library(tseries)

# Perform ADF test
adf_test <- adf.test(data$price, alternative = "stationary")
print(adf_test)




