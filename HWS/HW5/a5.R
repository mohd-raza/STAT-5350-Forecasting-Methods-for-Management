suppressPackageStartupMessages({
  library(astsa);
  library(lubridate); 
  library(car);
  library(dynlm)
})

#Q1

# Load necessary libraries
library(lubridate)

# Load the data
library(readr)
data <- read_csv("Desktop/Fall 24 Materials/STATS 535/HWS/HW5/a5_furniture_sales.csv")
View(data)

# Read the data

# Convert DATE column to Date type
data$DATE <- as.Date(data$DATE)

# Create a basic time series plot
plot(data$DATE, data$sales, type = "l",
     main = "Monthly Furniture Sales (1992-2024)",
     xlab = "Date", ylab = "Sales (Millions of Dollars)")

plot(data$DATE, log(data$sales), type = "l",
     main = "Monthly Furniture Sales (1992-2024)",
     xlab = "Date", ylab = "Log Scales Sales")

# Convert DATE column to Date format
data$DATE <- as.Date(data$DATE)

# Subset data prior to COVID, January 1992 through December 2019
subset_data <- subset(data, DATE >= as.Date("1992-01-01") & DATE <= as.Date("2019-12-31"))

# Extract year for grouping
subset_data$year <- year(subset_data$DATE)

# Calculate annual mean and standard deviation for sales
annual_means <- tapply(subset_data$sales, subset_data$year, mean)
annual_sds <- tapply(subset_data$sales, subset_data$year, sd)

# Calculate annual mean and standard deviation for log(sales)
log_sales <- log(subset_data$sales)
annual_means_log <- tapply(log_sales, subset_data$year, mean)
annual_sds_log <- tapply(log_sales, subset_data$year, sd)

# Plot the relationship
par(mfrow = c(1, 2)) # Side-by-side plots
plot(annual_means, annual_sds, main = "Sales Data", xlab = "Annual Mean", ylab = "Annual SD")
plot(annual_means_log, annual_sds_log, main = "Log(Sales) Data", xlab = "Annual Mean (Log)", ylab = "Annual SD (Log)")

par(mfrow = c(1, 1))  # Reset to single plot layout



#2

data$log_sales <- log(data$sales)

subset_1995_1996 <- subset(data, DATE >= as.Date("1995-01-01") & DATE <= as.Date("1996-12-31"))
subset_2005_2006 <- subset(data, DATE >= as.Date("2005-01-01") & DATE <= as.Date("2006-12-31"))
subset_2015_2016 <- subset(data, DATE >= as.Date("2015-01-01") & DATE <= as.Date("2016-12-31"))

# Extract month from date for seasonal comparison
subset_1995_1996$Month <- month(subset_1995_1996$DATE, label = TRUE)
subset_2005_2006$Month <- month(subset_2005_2006$DATE, label = TRUE)
subset_2015_2016$Month <- month(subset_2015_2016$DATE, label = TRUE)

# Aggregate mean log(sales) by month for each period
seasonal_1995_1996 <- aggregate(log_sales ~ Month, data = subset_1995_1996, mean)
seasonal_2005_2006 <- aggregate(log_sales ~ Month, data = subset_2005_2006, mean)
seasonal_2015_2016 <- aggregate(log_sales ~ Month, data = subset_2015_2016, mean)

# Combine the data for plotting
seasonal_data <- merge(merge(seasonal_1995_1996, seasonal_2005_2006, by = "Month", suffixes = c("_1995_1996", "_2005_2006")),
                       seasonal_2015_2016, by = "Month")
colnames(seasonal_data)[3] <- "log_sales_2015_2016"

# Plot seasonal patterns
# Plot for 1995-1996
plot(as.numeric(seasonal_1995_1996$Month), seasonal_1995_1996$log_sales,
     type = "l", col = "blue", xlab = "Month (Numeric)", ylab = "Mean Log(Sales)",
     main = "Seasonal Patterns of Log(Sales) (1995-1996)", xaxt = "n")
axis(1, at = 1:12, labels = 1:12)

# Plot for 2005-2006
plot(as.numeric(seasonal_2005_2006$Month), seasonal_2005_2006$log_sales,
     type = "l", col = "red", xlab = "Month (Numeric)", ylab = "Mean Log(Sales)",
     main = "Seasonal Patterns of Log(Sales) (2005-2006)", xaxt = "n")
axis(1, at = 1:12, labels = 1:12)

# Plot for 2015-2016
plot(as.numeric(seasonal_2015_2016$Month), seasonal_2015_2016$log_sales,
     type = "l", col = "green", xlab = "Month (Numeric)", ylab = "Mean Log(Sales)",
     main = "Seasonal Patterns of Log(Sales) (2015-2016)", xaxt = "n")
axis(1, at = 1:12, labels = 1:12)



#3
data$DATE <- as.Date(data$DATE)
# Extract year and month from the DATE column
data$year <- as.numeric(format(data$DATE, "%Y"))
data$month <- as.numeric(format(data$DATE, "%m"))  # Ensure numeric month values
data

# Filter training data for 2011-2019
train_data <- data[data$year >= 2011 & data$year <= 2019, ]

# Convert the log-transformed sales data to a time series
nYears <- 11  # Total number of years including forecast years (9 years + 2 forecast years)
yt <- ts(train_data$log_sales, start = c(2011, 1), frequency = 12)

# Create the time index and month factor variable
tt <- 2011 + 0:(nYears * 12 - 1) / 12  # Time index
mnth <- factor(rep(month.abb, nYears), levels = month.abb)  # Month factor variable

# Create a design matrix with time and month dummies, omitting January
X <- model.matrix(~ tt + mnth - 1)[, -2]  # Remove January (2nd column after tt)

# Fit the SARIMA(1,0,0) model with regression
regr <- sarima(yt, p = 1, d = 0, q = 0, xreg = X[1:((nYears - 2) * 12), ])

# Print the summary of the fitted model
print("hello")
print(regr)

#3

library(forecast)
library(ggplot2)

# Assuming 'data' is your dataset with 'DATE' and 'log_sales' columns
data$DATE <- as.Date(data$DATE)
data$year <- as.numeric(format(data$DATE, "%Y"))
data$month <- as.numeric(format(data$DATE, "%m"))

# Filter training data for 2011-2019
train_data <- data[data$year >= 2011 & data$year <= 2019, ]

# Convert the log-transformed sales data to a time series
nYears <- 11
yt <- ts(train_data$log_sales, start = c(2011, 1), frequency = 12)

# Create the time index and month factor variable
tt <- 2011 + 0:(nYears * 12 - 1) / 12
mnth <- factor(rep(month.abb, nYears), levels = month.abb)

# Create a design matrix with time and month dummies, omitting January
X <- model.matrix(~ tt + mnth - 1)[, -2]

# Fit the SARIMA(1,0,0) model with regression
sarima_model <- Arima(yt, order = c(1, 0, 0), xreg = X[1:((nYears - 2) * 12), ])

# Forecast using the model to get fitted values
fitted_values <- fitted(sarima_model)

# Prepare data for plotting
plot_data <- data.frame(
  Date = seq(as.Date("2011-01-01"), by = "month", length.out = length(yt)),
  Actual = as.numeric(yt),
  Fitted = as.numeric(fitted_values)
)

# Plot actual vs fitted values using ggplot2
ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual")) +
  geom_line(aes(y = Fitted, color = "Fitted")) +
  labs(title = "Actual vs Fitted Sales (Log-Transformed)",
       x = "Date",
       y = "Log Sales",
       color = "Legend") +
  theme_minimal()

#4


# Create future time periods for prediction (2020-2021)
future_periods <- 24  # 2 years × 12 months

# Create future xreg matrix for prediction
tt_future <- seq(2020, 2021 + 11/12, by = 1/12)
mnth_future <- factor(rep(month.abb, 2), levels = month.abb)
X_future <- model.matrix(~ tt_future + mnth_future - 1)[, -2]

# Generate forecasts using sarima.for
forecast_results <- sarima.for(yt, n.ahead = future_periods, p = 1, d = 0, q = 0, 
                               xreg = X[1:((nYears - 2) * 12), ], 
                               newxreg = X_future)

# Create a time sequence for plotting
forecast_dates <- seq(as.Date("2020-01-01"), by = "month", length.out = future_periods)

# Create a data frame for plotting
forecast_df <- data.frame(
  Date = forecast_dates,
  Forecast = forecast_results$pred,
  Lower = forecast_results$pred - 1.96 * forecast_results$se,
  Upper = forecast_results$pred + 1.96 * forecast_results$se
)

forecast_df


# Plot using ggplot2
ggplot(forecast_df, aes(x = Date)) +
  geom_line(aes(y = Forecast), color = "blue") +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, fill = "blue") +
  labs(title = "Sales Forecast for 2020-2021 with 95% Prediction Intervals",
       x = "Date",
       y = "Log Sales") +
  theme_minimal()



#5

date_seq <- seq(as.Date("1992-01-01"), as.Date("2021-12-31"), by = "month")

num_weekdays <- sapply(date_seq, function(x) {
  days_in_month <- seq(x, x + months(1) - days(1), by = "day")
  # Count weekdays
  sum(!weekdays(days_in_month) %in% c("Saturday", "Sunday"))
})

num_total_days <- sapply(date_seq, function(x) {
  # total no. of days in month
  days_in_month <- seq(x, x + months(1) - days(1), by = "day")
  length(days_in_month)
})

days_data <- data.frame(
  Month = date_seq,
  Weekdays = num_weekdays,
  TotalDays = num_total_days
)


acf(days_data$TotalDays, main = "ACF of Total Days in a Month")
pacf(days_data$TotalDays, main = "PACF of Total Days in a Month")


#6
library(forecast)

data$log_sales <- log(data$sales)

diff_log_sales <- diff(data$log_sales)

seasonal_diff_log_sales <- diff(diff_log_sales, lag = 12)

par(mfrow = c(1, 2))
acf(seasonal_diff_log_sales, lag.max = 36, main = "ACF: Differenced Log(Sales)")
pacf(seasonal_diff_log_sales, lag.max = 36, main = "PACF: Differenced Log(Sales)")
par(mfrow = c(1, 1))

# Fit the SARIMA model
sarima_model <- sarima(filtered_data$log_sales, 
                       p = 2, d = 1, q =2, 
                       P = 1, D = 1, Q = 1,S=12)


#7
data$DATE <- as.Date(data$DATE, format = "%Y-%m-%d")

# Create a sequence of dates for each month in the dataset
date_seq <- seq(from = min(data$DATE), to = max(data$DATE), by = "month")

# Calculate the number of weekdays in each month
num_weekdays <- sapply(date_seq, function(x) {
  days_in_month <- seq(x, x + months(1) - days(1), by = "day")
  sum(!weekdays(days_in_month) %in% c("Saturday", "Sunday"))
})

# Calculate the total number of days in each month
num_total_days <- sapply(date_seq, function(x) {
  days_in_month <- seq(x, x + months(1) - days(1), by = "day")
  length(days_in_month)
})

# Ensure the length of predictors matches the dataset
if (length(num_weekdays) != nrow(data)) {
  stop("Mismatch in the length of predictors and dataset. Check alignment.")
}

# Add predictors to the dataset
data$Weekdays <- num_weekdays
data$TotalDays <- num_total_days

# Log-transform sales data
data$log_sales <- log(data$sales)

# Fit the SARIMA model with exogenous predictors using the sarima function
sarima_with_predictors <- sarima(
  data$log_sales, 
  p = 2, d = 1, q = 2, 
  P = 1, D = 1, Q = 1, S = 12, 
  xreg = cbind(data$Weekdays, data$TotalDays)
)
sarima_with_predictors


#8
sarima_revised <- sarima(
  data$log_sales, 
  p = 2, d = 1, q = 1, 
  P = 0, D = 1, Q = 1, S = 12, 
  xreg = cbind(data$Weekdays, data$TotalDays)
)

#9
library(forecast)
library(ggplot2)

# Load and prepare the data
data$DATE <- as.Date(data$DATE)
data$log_sales <- log(data$sales)

# Filter data for 2011-2019 (training data)
train_data <- data[data$DATE >= as.Date("2011-01-01") & data$DATE <= as.Date("2019-12-31"), ]

# Convert to time series object
ts_data <- ts(train_data$log_sales, frequency = 12, start = c(2011, 1))

# Fit SARIMA model (assuming this was done in Q8)
model <- auto.arima(ts_data, seasonal = TRUE)

# Generate forecasts for 2020-2021
forecast_period <- data[data$DATE >= as.Date("2020-01-01") & data$DATE <= as.Date("2021-12-31"), ]
forecast_results <- forecast(model, h = nrow(forecast_period))

# Create plot
ggplot() +
  geom_line(data = data[data$DATE >= as.Date("2020-01-01") & data$DATE <= as.Date("2021-12-31"), ],
            aes(x = DATE, y = log_sales, color = "Actual")) +
  geom_line(data = data.frame(DATE = forecast_period$DATE, forecast = forecast_results$mean),
            aes(x = DATE, y = forecast, color = "Forecast")) +
  geom_ribbon(data = data.frame(DATE = forecast_period$DATE,
                                lower = forecast_results$lower[, 2],
                                upper = forecast_results$upper[, 2]),
              aes(x = DATE, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
  scale_color_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  labs(title = "SARIMA Forecast vs Actual Log(Furniture Sales) for 2020-2021",
       x = "Date", y = "Log(Sales)",
       color = "Legend") +
  theme_minimal()


#9


library(ggplot2)

# Historical data (up to 2019) and actual data for 2020-2021
historical_actual_data <- data[data$DATE >= as.Date("2020-01-01") & data$DATE <= as.Date("2021-12-31"), ]
forecast_dates <- seq(as.Date("2020-01-01"), by = "month", length.out = 24)

# Data for forecast and prediction intervals
forecast_data <- data.frame(
  DATE = forecast_dates,
  forecast = forecast_results$mean,
  lower_95 = forecast_results$lower[, 2],
  upper_95 = forecast_results$upper[, 2]
)

# Plotting with ggplot
ggplot() +
  # Actual data
  geom_line(data = historical_actual_data,
            aes(x = DATE, y = log_sales, color = "Actual"),
            size = 1.2, linetype = "solid") +
  # Forecasted data
  geom_line(data = forecast_data,
            aes(x = DATE, y = forecast, color = "Forecast"),
            size = 1.2, linetype = "solid") +
  # Prediction interval ribbon
  geom_ribbon(data = forecast_data,
              aes(x = DATE, ymin = lower_95, ymax = upper_95),
              fill = "blue", alpha = 0.3) +
  # Color customization
  scale_color_manual(values = c("Actual" = "darkgreen", "Forecast" = "red")) +
  # Labels and title
  labs(title = "SARIMA Forecast vs Actual Log(Sales) for 2020-2021",
       x = "Date",
       y = "Log(Sales)",
       color = "Legend") +
  # Custom theme for uniqueness
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom",
    legend.title = element_blank()
  )


comparison_table <- data.frame(
  Date = seq(as.Date("2020-01-01"), by = "month", length.out = 24),
  Actual = actual_data$log_sales,  # Actual values for 2020-2021
  Forecast = forecast_results$mean,  # Forecasted values
  Lower_95 = forecast_results$lower[, 2],  # Lower bound of 95% prediction interval
  Upper_95 = forecast_results$upper[, 2]   # Upper bound of 95% prediction interval
)

# Print the comparison table
print(comparison_table)


# Assuming forecast_df and comparison_table are already loaded in your environment

# Merge the two dataframes on the 'Date' column
comparison_df <- merge(forecast_df, comparison_table, by = "Date")

# Select only the necessary columns: Date, Actual, Forecast (from both models)

# Print the final comparison dataframe
print(comparison_df)

# Select and rename the required columns
comparison_df <- comparison_df[, c("Date", "Actual", "Forecast.x", "Forecast.y")]
colnames(comparison_df) <- c("Date", "Actual", "Regression", "SARIMA")

# Round all numeric columns to 3 decimal places
comparison_df[, c("Actual", "Regression", "SARIMA")] <- 
  round(comparison_df[, c("Actual", "Regression", "SARIMA")], 3)

# Print the modified dataframe
print(comparison_df)


#10)
# Load necessary library
library(ggplot2)

# Create the data frame
comparison_data <- data.frame(
  Date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-04-01", 
                   "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", 
                   "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", 
                   "2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", 
                   "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01", 
                   "2021-09-01", "2021-10-01", "2021-11-01", "2021-12-01")),
  Actual = c(9.151, 9.147, 8.995, 8.260, 8.885, 9.189, 9.249, 9.299, 9.297, 9.295, 9.300, 9.371,
             9.253, 9.201, 9.426, 9.380, 9.379, 9.359, 9.386, 9.391, 9.382, 9.389, 9.451, 9.443),
  Regression = c(9.116, 9.139, 9.280, 9.210, 9.284, 9.258, 9.288, 9.335, 9.291, 9.284, 9.372, 9.454,
                 9.203, 9.203, 9.333, 9.257, 9.329, 9.301, 9.331, 9.378, 9.334, 9.326, 9.414, 9.496),
  SARIMA = c(9.111, 9.097, 9.239, 9.178, 9.251, 9.212, 9.245, 9.289, 9.246, 9.249, 9.329, 9.375,
             9.144, 9.133, 9.273, 9.204, 9.276, 9.245, 9.272, 9.317, 9.274, 9.274, 9.357, 9.414)
)

# Reshape the data for ggplot
comparison_data_long <- reshape2::melt(comparison_data, id.vars = "Date", 
                                       variable.name = "Type", 
                                       value.name = "LogSales")

# Create the plot
ggplot(comparison_data_long, aes(x = Date, y = LogSales, color = Type)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = c("Actual" = "darkgreen", 
                                "Regression" = "blue", 
                                "SARIMA" = "red")) +
  labs(title = "Comparison of Actual, Regression, and SARIMA Log(Sales) (2020-2021)",
       x = "Date",
       y = "Log(Sales)",
       color = "Legend") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "bottom"
  )



