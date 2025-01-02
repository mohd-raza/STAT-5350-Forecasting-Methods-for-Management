# Load necessary libraries
set.seed(123)  # Setting seed for reproducibility

# Define parameters
n <- 60   # Length of each random walk
n_walks <- 25  # Number of random walks

# Simulate random walks
random_walks <- matrix(0, nrow = n, ncol = n_walks)
for (i in 1:n_walks) {
  random_walks[, i] <- cumsum(rnorm(n))  # Cumulative sum of standard normal white noise
}

# Plot all random walks
png("random_walks.png", width = 800, height = 600)
matplot(random_walks, type = "l", lty = 1, col = rainbow(n_walks),
        xlab = "Time", ylab = "Value", main = "25 Random Walks (δ = 0)")
dev.off()  



time_index <- 1:n

slopes <- numeric(n_walks)
p_values <- numeric(n_walks)

for (i in 1:n_walks) {
  model <- lm(random_walks[, i] ~ time_index)
  slopes[i] <- coef(model)[2]  # Slope estimate (β1)
  p_values[i] <- summary(model)$coefficients[2, 4]  # p-value of t-test for H0: β1 = 0
}

png("slopes_and_p_values.png", width = 800, height = 600)
par(mfrow = c(2, 1))  # Set up 2-panel plot

# Histogram of slopes
hist(slopes, main = "Histogram of Slopes", xlab = "Slope", col = "blue", border = "white")

# Histogram of p-values
hist(p_values, main = "Histogram of P-values", xlab = "P-value", col = "green", border = "white")

dev.off()  # Save the plot to a PNG file


# Initialize a vector to store the first autocorrelations
first_autocorrelations <- numeric(n_walks)

# Loop over each random walk to calculate the first autocorrelation of the residuals
for (i in 1:n_walks) {
  # Fit the linear model again (as done before)
  model <- lm(random_walks[, i] ~ time_index)
  
  # Get residuals from the model
  residuals_rw <- residuals(model)
  
  # Calculate the first autocorrelation of the residuals
  acf_result <- acf(residuals_rw, lag.max = 1, plot = FALSE)
  first_autocorrelations[i] <- acf_result$acf[2]  # Lag 1 autocorrelation is at index 2
}

# Plot histogram of the first autocorrelations
png("autocorrelation_histogram.png", width = 800, height = 600)
hist(first_autocorrelations, main = "Histogram of First Autocorrelations of Residuals",
     xlab = "Autocorrelation", col = "purple", border = "white")
dev.off()





land_window <- window(gtemp_land, start=1960)
ocean_window <- window(gtemp_ocean, start=1960)
attributes(land_window)
attributes(ocean_window)


land_index <- gtemp_land[111:174]
ocean_index <- gtemp_ocean[111:174]
attributes(land_index)
attributes(ocean_index)
View(land_index)



land_window <- window(gtemp_land, start=1960)
ocean_window <- window(gtemp_ocean, start=1960)


# Save Land plot with its fitted line
png("land_temperature_deviation.png", width = 800, height = 600)

# Plot land temperature deviations and fit the regression line
plot(land_window ~ time(land_window), type='l', col='red', xlab='Time', ylab='Temperature Deviation',
     main='Land Temperature Deviation with Fitted Line')
land_lm <- lm(land_window ~ time(land_window))  # Fit the linear model for land
abline(land_lm, col='blue', lty=2)  # Add the regression line

dev.off()  # Close the plot device

# Save Ocean plot with its fitted line
png("ocean_temperature_deviation.png", width = 800, height = 600)

# Plot ocean temperature deviations and fit the regression line
plot(ocean_window ~ time(ocean_window), type='l', col='green', xlab='Time', ylab='Temperature Deviation',
     main='Ocean Temperature Deviation with Fitted Line')
ocean_lm <- lm(ocean_window ~ time(ocean_window))  # Fit the linear model for ocean
abline(ocean_lm, col='orange', lty=2)  # Add the regression line

dev.off()  # Close the plot device


summary(land_lm)
summary(ocean_lm)

land_res <- residuals(land_lm)
land_res


# Calculate residuals for land and ocean
land_residuals <- residuals(land_lm)
ocean_residuals <- residuals(ocean_lm)

# Plot residuals vs. time for land temperature model
png("land_residuals_vs_time.png", width = 800, height = 600)
plot(time(land_window), land_residuals, type = 'p', col = 'blue', xlab = 'Year', 
     ylab = 'Residuals', main = 'Land Temperature Model Residuals vs Time')
abline(h = 0, col = 'red', lty = 2)  # Add a horizontal line at 0
dev.off()

# Plot residuals vs. time for ocean temperature model
png("ocean_residuals_vs_time.png", width = 800, height = 600)
plot(time(ocean_window), ocean_residuals, type = 'p', col = 'green', xlab = 'Year', 
     ylab = 'Residuals', main = 'Ocean Temperature Model Residuals vs Time')
abline(h = 0, col = 'red', lty = 2)  # Add a horizontal line at 0
dev.off()

# Residuals vs. Fitted values for land temperature model
png("land_residuals_vs_fitted.png", width = 800, height = 600)
plot(fitted(land_lm), land_residuals, type = 'p', col = 'blue', xlab = 'Fitted Values', 
     ylab = 'Residuals', main = 'Land Temperature Model Residuals vs Fitted Values')
abline(h = 0, col = 'red', lty = 2)
dev.off()

# Residuals vs. Fitted values for ocean temperature model
png("ocean_residuals_vs_fitted.png", width = 800, height = 600)
plot(fitted(ocean_lm), ocean_residuals, type = 'p', col = 'green', xlab = 'Fitted Values', 
     ylab = 'Residuals', main = 'Ocean Temperature Model Residuals vs Fitted Values')
abline(h = 0, col = 'red', lty = 2)
dev.off()


# Calculate correlation between the residuals of the land and ocean models
correlation <- cor(land_residuals, ocean_residuals)
(correlation)
# Cross-correlation function (CCF) to check for lagged relationships
ccf(land_residuals, ocean_residuals, main = "Cross-Correlation of Land and Ocean Residuals")
ccf
