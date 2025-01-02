##HW2

#q1

d <- read.csv('~/Desktop/Fall 24 Materials/STATS 535/HWS/HW2/a2_series.csv')
View(d)

xt <- ts(d$xt, start=1)
plot(xt, main="Time Series Plot of xt", ylab="xt", xlab="Time")

yt <- ts(d$yt, start = 1)
plot(yt, main="Time series Plot of yt", ylab='yt',xlab='Time')

xt_lm <- lm(xt ~ time(xt))
detrend_xt_lm <- residuals(xt_lm)
plot(detrend_xt_lm, main="Linearly Detrended Time Series of xt", ylab="Detrended xt", xlab="Time")


diff_xt <- diff(xt)
plot(diff_xt, main="Differenced Detrended Time Series of xt", ylab="Detrended xt", xlab="Time")





yt_lm <- lm(yt ~ time(yt))
detrend_yt_lm <- residuals(yt_lm)
plot(detrend_yt_lm, main="Linearly Detrended Time Series of yt", ylab="Detrended yt", xlab="Time")

diff_yt <- diff(yt)
plot(diff_xt, main="Differenced Detrended Time Series of yt", ylab="Detrended yt", xlab="Time")


raw_correlation <- cor(d$xt, d$yt)
print(raw_correlation)

detrended_correlation <- cor(diff_xt, diff(d$yt))
print(detrended_correlation)

detrended_correlation_lm <- cor(detrend_xt_lm, residuals(lm(yt ~ time(yt))))
print(detrended_correlation)



data <- read.csv("~/Desktop/Fall 24 Materials/STATS 535/HWS/HW2/unrate_quarterly.csv")
View(data)

library(astsa)
z <- data("hor")
z
View(z)

occRate <- c(hor, c(80.7, 77.5, 80.5, 77.5,   # 2016
                    81.4, 79.4, 81.4, 78.6,   # 2017
                    83.5, 81.1, 80.1, 76.4,   # 2018
                    80.5, 80.3, 82.8, 79.6))  

occRate <- ts(occRate, start=1982, frequency=4)
occRate


# Step 1: Load the unemployment rate data from the CSV
u <- read.csv("~/Desktop/Fall 24 Materials/STATS 535/HWS/HW2/unrate_quarterly.csv")
u <- ts(u["UNRATE"], start=1948, frequency=4)  # Convert to time series
unRate <- window(u, start=1982, end=2019.75)  # Trim the data from 1982 to 2019

# Step 2: Calculate percentage change in GNP
x <- window(GNP, start=1981.75, end=2019.75)
pctChgGNP <- diff(x) / x[-length(x)]  # Percentage change formula

# quarter categorical feature identifying quarter
quarter <- as.factor(rep(1:4, length(occRate)/4))

unRate
pctChgGNP
occRate
# Load the required package
library(dynlm)

# Define the regression model with lagged variables
regr_1 <- dynlm(occRate ~ L(occRate, 1) + L(occRate, 4)      # Lagged occupancy rates
                + quarter                            # Quarter as a categorical variable
                + L(unRate, 1) + L(unRate, 2))        # Lagged unemployment rates

# Show a summary of the regression model
summary(regr_1)


regr_2 <- dynlm(occRate ~ L(occRate,1) + L(occRate, 4)
                + quarter
                + L(unRate,1) + L(unRate,2)
                + L(pctChgGNP,1) + L(pctChgGNP,2))

summary(regr_2)

anova(regr_1, regr_2)

length(occRate)
?vif

vif_values <- vif(regr_2)
print(vif_values)



plot(regr_2)

par(mfrow = c(2, 2))

plot(regr_2)

library(car)

durbinWatsonTest(regr_2)

regr_simplified <- dynlm(occRate ~ L(occRate, 1) + L(occRate, 4) + quarter + diff(unRate, 1))
summary(regr_simplified)
# Compare the original and simplified models using an ANOVA F-test
anova(regr_2, regr_simplified)

occRate
