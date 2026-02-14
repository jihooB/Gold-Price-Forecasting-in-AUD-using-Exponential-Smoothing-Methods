# ETW3420_A1
# Byun Jihoo_31858945
rm(list=ls())

#  = = = = = = = = Stage 1: Data Collection & Pre-processing = = = = = = = = 

# Loading essential packages
library(readxl)
library(dplyr)
library(ggplot2)
library(dplyr)
library(tsibble)
library(feasts)
library(forecast)
library(tsibble)
library(feasts)
library(zoo)
library(tseries)

# Loading the excel format data and save it
data <- read_excel("/Users/jihoo/Downloads/Gold_price_averages_in_a range_of_currencies_since_1978.xlsx", sheet = 4)

# save the data into csv format in working directory
write.csv(data,"goldprice.csv",  row.names = FALSE)

# Loading the saved data
gp_data <- read.csv("/Users/jihoo/R_coding_study_002/goldprice.csv")
gp_data

# Extract interested column of the csv data sheet
gp_aud_data <- gp_data[, c("...2", "...21")]
View(gp_aud_data)

# Changing the colnames of data according to its currency
colnames(gp_aud_data)[1] <- "time"
colnames(gp_aud_data)[2] <- "AUD"

# removing NA values
clean_data <- gp_aud_data %>% filter(!is.na(time) & !is.na(AUD))
gold.price.data <- clean_data

#Changing to time series data.
gold_ts <- ts(as.numeric(gold.price.data$AUD), start = c(1978, 2), frequency = 12)
View(gold_ts)

# Stage 2: Exploratory Data Analysis
  
# VISUALIZATION_Trend
autoplot(gold_ts) + labs(title = "Trend Visualisation of Gold Price")

# VISUALIZATION_SEASONALITY
ggsubseriesplot(gold_ts) + labs(title = "Seasonality Visualisation of Gold Price")

# VISUALIZATION_VOLATILITY
fit <- stl(gold_ts, s.window = "periodic")
autoplot(fit$time.series[, "remainder"]) + labs(title = "Voltility Visualisation of Gold Price")

# Box Cox tranformation of the data
lambda <- BoxCox.lambda(gold_ts)
gold_boxcox <- BoxCox(gold_ts, lambda)

# Trend visualization for boxcox data
autoplot(gold_boxcox) + labs(title = "(boxcox)Trend Visualisation of Gold Price")

# Volatility visualization for boxcox data
fit_log_vis <- stl(gold_boxcox, s.window = "periodic")
autoplot(fit_log_vis$time.series[, "remainder"]) + labs(title = "(boxcox)Voltility Visualisation of Gold Price")

# Seasonality visualization for boxcox data
ggsubseriesplot(gold_boxcox) + labs(title = "(boxcox)Seasonality Visualisation of Gold Price")



# ============================= Stage 3: Forecasting Model Development ================
# A simple forecasting method (e.g., naïve, moving average).
snaive(gold_ts)
summary(snaive(gold_ts))

# Plot forecast
autoplot(snaive(gold_ts)) +
  labs(title = "Forecast using Seasonal Naive Model", y = "Gold Price (AUD)")


# An ETS (Exponential Smoothing) model of your choosing based on your analysis in Stage 2.

# Mannual ETS_AAA model with boxcox-transformation
ets_AAA <- ets( gold_boxcox, model = "AAA", damped = FALSE, lambda = NULL,
     biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
     opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE)
summary(ets_AAA)


# Mannual ETS_AAN model with boxcox-transformation
ets_AAN <- ets( train_au_bc, model = "AAN", damped = FALSE, lambda = NULL,
     biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
     opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE)

summary(ets_AAN)

# An ETS (Exponential Smoothing) model using the ets() function.
ets(gold_boxcox)
summary(ets(gold_boxcox))


# = = = = = = = = Stage 4: Model Evaluation & Selection = = = = = = = = 
train_au <- window(gold_ts, end = c(2018, 12))
test_au <- window(gold_ts, start = c(2019, 1))

train_au_bc <- window(gold_boxcox, end = c(2018, 12))
test_au_bc <- window(gold_boxcox, start = c(2019, 1))



fit_snaive <- snaive(train_au, h = length(test_au))
accuracy(fit_snaive, test_au)

autoplot(fit_snaive) +
  autolayer(test_au, series = "Test Data") +
  labs(title = "Seasonal Naive Forecast vs Actual",
       y = "Gold Price (AUD)")


fit_ets_AAA <- ets( train_au_bc, model = "AAA", damped = FALSE, lambda = NULL,
                    biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
                    opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE)

fc_fit_ets_AAA <- forecast(fit_ets_AAA, h = length(test_au_bc))

accuracy(fc_fit_ets_AAA, test_au_bc)

autoplot(fc_fit_ets_AAA) +
  autolayer(test_au_bc, series = "Test Data") +
  labs(title = "Mannual ETS_AAA model Forecast vs Actual",
       y = "Gold Price (AUD)")



fit_ets_AAN <- ets( train_au_bc, model = "AAN", damped = FALSE, lambda = NULL,
                    biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
                    opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE)

fc_fit_ets_AAN <- forecast(fit_ets_AAN, h = length(test_au_bc))

accuracy(fc_fit_ets_AAN, test_au_bc)

autoplot(fc_fit_ets_AAN) +
  autolayer(test_au_bc, series = "Test Data") +
  labs(title = "Mannual ETS_AAN model Forecast vs Actual",
       y = "Gold Price (AUD)")


fit_ets <- ets(train_au_bc)
fc_fit_ets <- forecast(fit_ets, h = length(test_au_bc))

accuracy(fc_fit_ets, test_au_bc)

autoplot(fc_fit_ets) +
  autolayer(test_au_bc, series = "Test Data") +
  labs(title = "Automatic ETS model Forecast vs Actual",
       y = "Gold Price (AUD)")


# ○ Residual diagnostics:

checkresiduals(snaive(gold_ts))

checkresiduals(ets( gold_boxcox, model = "AAA", damped = FALSE, lambda = NULL,
                    biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
                    opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE))


checkresiduals(ets( gold_boxcox, model = "AAN", damped = FALSE, lambda = NULL,
                    biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
                    opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE))

checkresiduals(ets(gold_boxcox))







  # ○ Time-series cross-validation:

# Bench_Mark cross-validation
e1 <- tsCV(gold_boxcox, forecastfunction = snaive, h = 12)
BM_MDL <- colMeans(e1^2, na.rm = TRUE)
BM_MDL


# ets_AAA cross-validation
f_ets_AAA <- function(gold_boxcox, h) {
  ets_fit_AAA <- ets( gold_boxcox, model = "AAA", damped = FALSE, lambda = NULL,
                      biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
                      opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE)
  forecast(ets_fit_AAA, h = h)
  }

e2 <- tsCV(gold_boxcox, forecastfunction = f_ets_AAA, h = 12)

ets_AAA_MDL <- colMeans(e2^2, na.rm = TRUE)
ets_AAA_MDL

# ets_AAN cross-validation
f_ets_AAN <- function(gold_boxcox, h) {
  ets_fit_AAN <- ets( gold_boxcox, model = "AAN", damped = FALSE, lambda = NULL,
                      biasadj = TRUE,  additive.only = TRUE ,allow.multiplicative.trend = TRUE,
                      opt.crit = "lik",bounds = "both", ic = "aicc", restrict = TRUE)
  forecast(ets_fit_AAN, h = h)
}

e3 <- tsCV(gold_boxcox, forecastfunction = f_ets_AAN, h = 12)

ets_AAN_MDL <- colMeans(e3^2, na.rm = TRUE)
ets_AAN_MDL



# ETS_auto(AAdN) cross-validation
f_ets <- function(gold_boxcox, h) {
  ets_fit <- ets(gold_boxcox)
  forecast(ets_fit, h = h)
}

e4 <- tsCV(gold_boxcox, forecastfunction = f_ets, h = 12)

ets_MDL <- colMeans(e4^2, na.rm = TRUE)
ets_MDL


# ○  Champion Model Selection:
accuracy(fit_snaive, test_au_bc)

accuracy(fc_fit_ets_AAA, test_au_bc)

accuracy(fc_fit_ets_AAN, test_au_bc)

accuracy(fc_fit_ets, test_au)

# Compare Cross-Validation_ets log
BM_MDL

ets_AAA_MDL

ets_AAN_MDL

ets_MDL

# CHAMPION MODEL:
ets_AAA_MDL



# Stage 5: Forecasting

ets_AAA_frct <- forecast(ets( gold_boxcox, model = "AAA", damped = FALSE, 
                              lambda = 0, biasadj = TRUE, additive.only = TRUE, 
                              allow.multiplicative.trend = TRUE, opt.crit = "lik", 
                              bounds = "both", ic = "aicc", restrict = TRUE),
                         h=12)
ets_AAA_frct
autoplot(ets_AAA_frct) + labs(title = "ETS_AAA model Forecast for Gold Prices",
                              y = "Gold Price (AUD)",
                              x = "Time")





