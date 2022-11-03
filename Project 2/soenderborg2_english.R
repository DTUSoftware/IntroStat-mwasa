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

# Estimate multiple linear regression model
fit <- lm(Q ~ Ta + G, data = D_model)

# Show parameter estimates etc.
summary(fit)


###########################################################################
## Plots for model validation

# Observations against fitted values
plot(fit$fitted.values, D_model$Q, xlab = "Fitted values",     
       ylab = "Heat consumption")

# Residuals against each of the explanatory variables
plot(D_model$EXPLANATORY_VARIABLE, fit$residuals, 
        xlab = "INSERT TEXT", ylab = "Residuals")

# Residuals against fitted values
plot(fit$fitted.values, fit$residuals, xlab = "Fitted values", 
     ylab = "Residuals")

# Normal QQ-plot of the residuals
qqnorm(fit$residuals, ylab = "Residuals", xlab = "Z-scores", 
       main = "")
qqline(fit$residuals)


###########################################################################

# Confidence intervals for the model coefficients
confint(fit, level = 0.95)


# Make dataset for validation
D_test <- subset(D, (t == "2008-12-06" & houseId == 3)|
                          (t == "2009-02-22" & houseId == 5)|
                          (t == "2009-03-12" & houseId == 10)|
                          (t == "2009-04-01" & houseId == 17))

# Predictions and 95% prediction intervals
pred <- predict(FINAL_MODEL, newdata = D_test, 
                interval = "prediction", level = 0.95)

# Observed values and predictions
cbind(id = D_test$houseId, Q = D_test$Q, pred)

