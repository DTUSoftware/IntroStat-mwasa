###########################################################################
## Name: Marcus Wahlers Sand                                             ##
## Student Number: s215827 (mwasa)                                       ##
## Project: Project 2 - Heating in Sønderborg II                         ##
## 02323 Introduction to Statistics - Fall 22                            ##
###########################################################################

###########################################################################
## Get dependencies

# If RStudioAPI is not installed, install it (should be installed with RStudio, but can also be installed without)
# We use it for getting the current directory
if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
# purrr is a dependency for RStudioAPI to get current directory
if (!require("purrr")) install.packages("purrr"); library("purrr")
# We use MESS for Q-Q plots
if (!require("MESS")) install.packages("MESS"); library("MESS")
# We use car for scatterplots
if (!require("car")) install.packages("car"); library("car")
# We use tidyverse for ggplot2
if (!require("tidyverse")) install.packages("tidyverse"); library("ggplot2")
# We use reshape2 to melt data into a long format
if (!require("reshape2")) install.packages("reshape2"); library("reshape2")


###########################################################################
## Set the working directory

# We use the RStudioAPI to get the active document path - keep the data in the same folder as .R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###########################################################################
## Read data into R

# Read the dataset 'soenderborg2_data.csv' into R
D <- read.table("soenderborg2_data.csv", sep = ";", header = TRUE)


###########################################################################
## Processing of data

# Make 't' a date variable in R
D$t <- as.Date(D$t, format = "%d/%m/%Y")

# Choose data from 15 Oct 2009 to 15 Apr 2010 for the four houses
D_model <- subset(D, ("2009-10-15" <= t & t < "2010-04-16") &
  (houseId %in% c(3, 5, 10, 17)))

# Remove observations with missing values
D_model <- na.omit(D_model)


###########################################################################
## Short descriptive analysis and summary of the data

variables <- c("Q", "Ta", "G")

# Scatterplots
ggplot(D_model, aes(x = Q, y = Ta)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Heat Consumption (Q) [kW]", y = "Outdoor Temperature (Ta) [Celsius]", title = "Scatter plot between the heat consumption (Q) and the outdoor temperature (Ta)")

ggplot(D_model, aes(x = Q, y = G)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Heat Consumption (Q) [kW]", y = "Solar Irradiance (G) [W/m^2]", title = "Scatter plot between the heat consumption (Q) and the solar irradiance (G)")

# Histograms
ggplot(D_model, aes(Q)) +
  geom_histogram(aes(y = ..density..), bins = 15) +
  geom_density() +
  labs(x = "Heat Consumption (Q) [kW]", y = "Density", title = "Histogram of empirical density of the heat consumption (Q)")

ggplot(D_model, aes(Ta)) +
  geom_histogram(aes(y = ..density..), bins = 15) +
  geom_density() +
  labs(x = "Outdoor Temperature (Ta) [Celsius]", y = "Density", title = "Histogram of empirical density of the outdoor temperature (Ta)")

ggplot(D_model, aes(G)) +
  geom_histogram(aes(y = ..density..), bins = 15) +
  geom_density() +
  labs(x = "Solar Irradiance (G) [W/m^2]", y = "Density", title = "Histogram of empirical density of the solar irradiance (G)")

# Boxplots
ggplot(melt(D_model[variables[1]], id.vars = NULL), aes(x = variable, y = value)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  labs(x = "Variable", y = "Heat Consumption [kW]", title = "Box plot showing heat consumption")

ggplot(melt(D_model[variables[2]], id.vars = NULL), aes(x = variable, y = value)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  labs(x = "Variable", y = "Outdoor Temperature [Celsius]", title = "Box plot showing outdoor temperature")

ggplot(melt(D_model[variables[3]], id.vars = NULL), aes(x = variable, y = value)) +
  stat_boxplot(geom = 'errorbar') +
  geom_boxplot() +
  labs(x = "Variable", y = "Solar Irradiance [W/m^2]", title = "Box plot showing solar irradiance")

# Summary Table
Tbl <- apply(D_model[, variables], 2, function(x) {
  c(
    n = sum(!is.na(x)),                                   ## Total number of observations (doesn't include missing values if there are any)
    mean = mean(x, na.rm = TRUE),                         ## Sample mean of daily heat consumption
    var = var(x, na.rm = TRUE),                           ## Sample variance of daily heat consumption
    sd = sd(x, na.rm = TRUE),                             ## Sample standard deviance
    lq = unname(quantile(x, probs = 0.25, na.rm = TRUE)), ## Lower quartile, Q1
    median = median(x, na.rm = TRUE),                     ## Median, Q2 (could also have used "quantile(x, probs=0.5, na.rm=TRUE)")
    hq = unname(quantile(x, probs = 0.75, na.rm = TRUE))  ## Upper quartile, Q3
  )
})
Tbl


###########################################################################
## Multiple Linear regression

# Estimate multiple linear regression model
fit <- lm(Q ~ Ta + G, data = D_model)

# Show parameter estimates etc.
summary(fit)


###########################################################################
## Plots for model validation

D_fit <- data.frame(fitted_values = fit$fitted.values, residuals = fit$residuals,
                    Q = fit$model$Q, Ta = fit$model$Ta, G = fit$model$G)

# Observations against fitted values
ggplot(D_fit, aes(x = fitted_values, y = Q)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Fitted values", y = "Heat Consumption (Q) [kW]", title = "Scatter plot for observations against fitted values")

# Residuals against each of the explanatory variables
ggplot(D_fit, aes(x = Ta, y = residuals)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Outdoor Temperature (Ta) [Celsius]", y = "Residuals", title = "Scatter plot for residuals against outdoor temperature")

ggplot(D_fit, aes(x = G, y = residuals)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Solar Irradiance (G) [W/m^2]", y = "Residuals", title = "Scatter plot for residuals against solar irradiance")

# Residuals against fitted values
ggplot(D_fit, aes(x = fitted_values, y = residuals)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Fitted values", y = "Residuals", title = "Scatter plot for residuals against fitted values")

# Normal QQ-plot of the residuals
qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...) }
## qq-plot of the residuals
wallyplot(D_fit$residuals, FUN = qqnorm.wally, ylab = "Residuals", xlab = "Z-scores", main = "", hide = FALSE) # Multiple (simulated) Q-Q Plots for House 1


###########################################################################
## Confidence intervals for the model coefficients

## CI for the heat consumption, manually
n <- length(D_fit$Q)
beta_1 <- unname(fit$coefficients[2])
conf_level <- 0.95
a <- 1 - conf_level
t_0.975 <- qt(1 - a / 2, n - (2 + 1))
c(beta_1 - t_0.975 * 0.0058260, unname(fit$coefficients[2]) + t_0.975 * 0.0058260) # and this is also just manually put in

# Confidence intervals for the model coefficients
confint(fit, level = 0.95)

###########################################################################
## Hypothesis

# test statistic
t_obs <- (beta_1 - (-0.25)) / 0.0058260

## Test statistic distribution
x <- seq(-4, 4, by = 0.01)
y <- dt(x, df = n - 2)

ggplot(data.frame(x = x, y = y), aes(x = x, y = y)) +
  geom_line() +
  annotate(geom = "text", x = 2.0, y = 0.2, label = "Black: t(573)", col = "black") +
  geom_polygon(data = data.frame(x = c(x[x >= t_0.975], max(x), t_0.975), y = c(y[x >= t_0.975], 0, 0)), aes(x = x, y = y), fill = "red", alpha = 0.75) +
  annotate(geom = "text", x = t_0.975, y = -0.01, label = "t_0.975", col = "red") +
  geom_polygon(data = data.frame(x = c(min(x), x[x <= -t_0.975], -t_0.975), y = c(y[x <= -t_0.975], 0, 0)), aes(x = x, y = y), fill = "red", alpha = 0.75) +
  annotate(geom = "text", x = -t_0.975, y = -0.01, label = "-t_0.975", col = "red") +
  labs(y = "Density(x)", title = "Distribution of the test statistic (n-(p+1) (573) degrees of freedom)")


###########################################################################
## Backward selection

summary(fit)
ggplot(data.frame(fitted_values = fit$fitted.values, Q = fit$model$Q), aes(x = fitted_values, y = Q)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Fitted values", y = "Heat Consumption (Q) [kW]", title = "Observations against fitted values (Q ~ Ta + G)")


fit2 <- lm(Q ~ Ta, data = D_model)
summary(fit2)
ggplot(data.frame(fitted_values = fit2$fitted.values, Q = fit2$model$Q), aes(x = fitted_values, y = Q)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Fitted values", y = "Heat Consumption (Q) [kW]", title = "Observations against fitted values (Q ~ Ta)")


fit3 <- lm(Q ~ G, data = D_model)
summary(fit3)
ggplot(data.frame(fitted_values = fit3$fitted.values, Q = fit3$model$Q), aes(x = fitted_values, y = Q)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype = "dashed") +     # Loess method
  labs(x = "Fitted values", y = "Heat Consumption (Q) [kW]", title = "Observations against fitted values (Q ~ G)")


###########################################################################
## Validation of the final model

# Make dataset for validation
D_test <- subset(D, (t == "2008-12-06" & houseId == 3) |
  (t == "2009-02-22" & houseId == 5) |
  (t == "2009-03-12" & houseId == 10) |
  (t == "2009-04-01" & houseId == 17))

# Predictions and 95% prediction intervals
pred <- predict(fit, newdata = D_test,
                interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(houseId = D_test$houseId, Q = D_test$Q, pred)

# Compare predictions to observed heat consumption of the validation set
n <- length(D_test$houseId)
sum <- 0
for (i in 1 : n) {
  print(pred[i]-D_test$Q[i])
  sum <- sum + abs(pred[i]-D_test$Q[i])
}
avg <- sum/n
avg
