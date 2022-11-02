###########################################################################
## Name: Marcus Wahlers Sand
## Student Number: s215827
## Project: Project 1 - Heating in Sønderborg
## 02323 Introduction to Statistics - Fall 22

###########################################################################
## Get dependencies

## If RStudioAPI is not installed, install it (should be installed with RStudio, but can also be installed without)
## We use it for getting the current directory
if (!require("rstudioapi")) install.packages("rstudioapi"); library("rstudioapi")
## purrr is a dependency for RStudioAPI to get current directory
if (!require("purrr")) install.packages("purrr"); library("purrr")
## We use MESS for Q-Q plots
if (!require("MESS")) install.packages("MESS"); library("MESS")
## We use car for scatterplots
if (!require("car")) install.packages("car"); library("car")
## We use tidyverse for ggplot2
if (!require("tidyverse")) install.packages("tidyverse")
library("ggplot2")
## We use reshape2 to melt data into a long format
if (!require("reshape2")) install.packages("reshape2"); library("reshape2")


###########################################################################
## Set the working directory

## In RStudio the working directory is easily set via the menu
## "Session -> Set Working Directory -> To Source File Location" 
## Note: In R only "/" is used for separating in paths 
## (i.e. no backslash).

## We use the RStudioAPI to get the active document path - keep the data in the same folder as .R file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###########################################################################
## Read data into R

## Read data from soenderborg1_data.csv
D <- read.table("soenderborg1_data.csv", header=TRUE, sep=";",
                as.is=TRUE)


###########################################################################
## Simple overview of the data

## Dimensions of D (number of rows and columns)
cat(sprintf("Dimensions: "))
dim(D)
##  Column/variable names
cat(sprintf("\nColumn names: "))
names(D)
## The first rows/observations
cat(sprintf("\nThe first rows:\n"))
head(D)
## The last rows/observations
# tail(D)
## Selected summary statistics
cat(sprintf("\nSummary:\n"))
summary(D)
## Another type of summary of the dataset
cat(sprintf("\nAnother summary:\n"))
str(D)

var(D$Q1, na.rm=TRUE)
var(D$Q2, na.rm=TRUE)
var(D$Q3, na.rm=TRUE)
var(D$Q4, na.rm=TRUE)


###########################################################################
## Histogram (empirical density)

## Histogram describing the empirical density of the daily heat 
## consumptions of House 1 (histogram of daily consumptions normalized 
## to have an area of 1)
# hist(D$Q1, xlab="Heat consumption (House 1) [kW/day]", ylab="Density", prob=TRUE, main="Histogram of empirical density of the daily heat consumption (House 1)")
ggplot(D, aes(Q1)) + geom_histogram(aes(y=..density..), bins=15) + geom_density() + labs(x="Heat consumption (House 1) [kW/day]", y="Density", title="Histogram of empirical density of the daily heat consumption (House 1)")

###########################################################################
## Conversion to a date variable

## Converts the variable 't' to a date variable in R
D$t <- as.Date(x=D$t, format="%Y-%m-%d")
## Checks the result
summary(D$t)


###########################################################################
## Plot of data over time

## Plot of heat consumption over time
# plot(D$t, D$Q1, type="l", xlim=as.Date(c("2008-10-02","2010-10-01")),
#      ylim=c(0,9), xlab="Date", xaxt="n", ylab="Heat consumption [kW/day]", col=2, main="Heat consumption over time (October 2nd, 2008 to October 1st, 2010)")
# axis(1, D$t, format(D$t, "%b %d"), cex.axis=.7) # https://stackoverflow.com/a/4844261
# lines(D$t, D$Q2, col=3)
# lines(D$t, D$Q3, col=4)
# lines(D$t, D$Q4, col=5)
# ## Add a legend
# legend("topright", legend=paste0("Q", c(1,2,3,4)), lty=1, col=2:5)

colors <- c("House 1 (Q1)" = 2, "House 2 (Q2)" = 3, "House 3 (Q3)" = 4, "House 4 (Q4)" = 5)
ggplot(D, aes(x=t)) +
  geom_line(aes(y=Q1, color="House 1 (Q1)")) +
  geom_line(aes(y=Q2, color="House 2 (Q2)")) +
  geom_line(aes(y=Q3, color="House 3 (Q3)")) +
  geom_line(aes(y=Q4, color="House 4 (Q4)")) +
  scale_x_date(limits = as.Date(c("2008-10-02", "2010-10-01")), date_labels = "%b %d %Y") +
  scale_color_manual(values = colors) +
  labs(x="Date", y="Heat consumption [kW/day]", title="Heat consumption over time (October 2nd, 2008 to October 1st, 2010)", colour="Legend")


###########################################################################
## Taking a subset

## Subset of the data: only Jan-Feb 2010
Dsel <- subset(D, "2010-01-01" <= t & t < "2010-3-01")


###########################################################################
## Box plot

## Box plot of daily heat consumption by house
# boxplot(Dsel[ ,c("Q1","Q2","Q3","Q4")],
#         xlab="House", ylab="Heat consumption [kW/day]", main="Box plot showing daily heat consumption by house (Jan-Feb 2010)")

ggplot(melt(Dsel[ ,c("Q1","Q2","Q3","Q4")]), aes(x=variable, y=value)) +
  stat_boxplot(geom ='errorbar') +
  geom_boxplot() +
  labs(x="House", y="Heat consumption [kW/day]", title="Box plot showing daily heat consumption by house (Jan-Feb 2010)")

###########################################################################
## Summary statistics for daily heat consumptions

## We use the code from Remark 2.2
selected <- c("Q1","Q2","Q3","Q4")
Tbl <- apply(Dsel[, selected], 2, function(x) {
  c(
    n=sum(!is.na(x)),                               ## Total number of observations (doesn't include missing values if there are any)
    mean=mean(x, na.rm=TRUE),                       ## Sample mean of daily heat consumption
    var=var(x, na.rm=TRUE),                         ## Sample variance of daily heat consumption
    sd=sd(x, na.rm=TRUE),                           ## Sample standard deviance
    lq=unname(quantile(x, probs=0.25, na.rm=TRUE)), ## Lower quartile, Q1
    median=median(x, na.rm=TRUE),                   ## Median, Q2 (could also have used "quantile(x, probs=0.5, na.rm=TRUE)")
    hq=unname(quantile(x, probs=0.75, na.rm=TRUE))  ## Upper quartile, Q3
  )
})
# Tbl <- as.data.frame(Tbl)
Tbl

## The argument 'na.rm=TRUE' ensures that the statistic is
## computed even in cases where there are missing values.


###########################################################################
## qq-plot for model validation
## couldn't figure out whether wallyplot works with ggplot

qqnorm.wally <- function(x, y, ...) { qqnorm(y, ...); qqline(y, ...)}
## qq-plot of daily heat consumption (House 1)
fitQ1 <- lm(Dsel$Q1 ~ 1)
wallyplot(fitQ1, FUN = qqnorm.wally, main = "", hide = FALSE) # Multiple (simulated) Q-Q Plots for House 1
## qq-plot of daily heat consumption (House 2)
fitQ2 <- lm(Dsel$Q2 ~ 1)
wallyplot(fitQ2, FUN = qqnorm.wally, main = "", hide = FALSE) # Multiple (simulated) Q-Q Plots for House 2
## qq-plot of daily heat consumption (House 3)
fitQ3 <- lm(Dsel$Q3 ~ 1)
wallyplot(fitQ3, FUN = qqnorm.wally, main = "", hide = FALSE) # Multiple (simulated) Q-Q Plots for House 3
## qq-plot of daily heat consumption (House 4)
fitQ4 <- lm(Dsel$Q4 ~ 1)
wallyplot(fitQ4, FUN = qqnorm.wally, main = "", hide = FALSE) # Multiple (simulated) Q-Q Plots for House 4


###########################################################################
## Confidence interval for the mean

## CI for the mean daily heat consumption of House 1, manually
n <- Dsel$Q1
conf_level <- 0.95
a <- 1-conf_level
t_0.975 <- qt(1-a/2, n-1)
c(mean(Dsel$Q1, na.rm=TRUE)-t_0.975*(sd(Dsel$Q1, na.rm=TRUE)/sqrt(n)), mean(Dsel$Q1, na.rm=TRUE)+t_0.975*(sd(Dsel$Q1, na.rm=TRUE)/sqrt(n)))

## CI for the mean daily heat consumption of House 1, with R
t.test(Dsel$Q1, conf.level=0.95)$conf.int
t.test(Dsel$Q2, conf.level=0.95)$conf.int ## and House 2
t.test(Dsel$Q3, conf.level=0.95)$conf.int ## and House 3
t.test(Dsel$Q4, conf.level=0.95)$conf.int ## and House 4


###########################################################################
## One-sample t-test

## Calculating t_obs manually
t_obs <- (mean(Dsel$Q1, na.rm=TRUE)-2.38)/(sd(Dsel$Q1, na.rm=TRUE)/sqrt(55))

## Test statistic distribution
n <- sum(!is.na(Dsel$Q1))
x <- seq(-4, 4, by = 0.01)
y <- dt(x, df = n-1)
t_0.975 <- qt(0.975, df = n-1)
# plot(x, y, type = "l", col = "black", ylab = "Density(x)", main = "Distribution of the test statistic (n-1 (54) degrees of freedom)")
# polygon(c(x[x>=t_0.975], max(x), t_0.975), c(y[x>=t_0.975], 0, 0), col="red")
# polygon(c(min(x), x[x<=-t_0.975], -t_0.975), c(y[x<=-t_0.975], 0, 0), col="red")
# text(2.0, 0.2,"Black: t(54)", col = "black")
# text(t_0.975, -0.01,"t0.975", col = "red")
# text(-t_0.975, -0.01,"-t0.975", col = "red")

ggplot(data.frame(x=x,y=y), aes(x=x, y=y)) +
  geom_line() +
  annotate(geom="text", x=2.0, y=0.2, label="Black: t(54)", col="black") +
  geom_polygon(data=data.frame(x=c(x[x>=t_0.975], max(x), t_0.975), y=c(y[x>=t_0.975], 0, 0)), aes(x=x, y=y), fill="red", alpha=0.75) +
  annotate(geom="text", x=t_0.975, y=-0.01, label="t_0.975", col="red") +
  geom_polygon(data=data.frame(x=c(min(x), x[x<=-t_0.975], -t_0.975), y=c(y[x<=-t_0.975], 0, 0)), aes(x=x, y=y), fill="red", alpha=0.75) +
  annotate(geom="text", x=-t_0.975, y=-0.01, label="-t_0.975", col="red") +
  labs(y="Density(x)", title="Distribution of the test statistic (n-1 (54) degrees of freedom)")

## p-value, manually
p <- 2*(1-pt(abs(t_obs), df = n-1))

##  Testing hypothesis mu=2.38 for daily heat consumption 
## (House 1, Jan-Feb 2010)
t.test(Dsel$Q1, mu=2.38)


###########################################################################
## Welch t-test for comparing two (independent) samples

## Calculating t_obs manually
x1 <- Dsel$Q1
n1 <- sum(!is.na(x1))
barx1 <- mean(x1, na.rm=TRUE)
s1 <- sd(x1, na.rm=TRUE)
x2 <- Dsel$Q2
n2 <- sum(!is.na(x2))
barx2 <- mean(x2, na.rm=TRUE)
s2 <- sd(x2, na.rm=TRUE)
delta <- 0
t_obs <- ((barx1-barx2)-delta)/sqrt((s1^2/n1)+(s2^2/n2))

## Calculating v manually
v <- (s1^2/n1 + s2^2/n2)^2 / (((s1^2/n1)^2)/(n1-1) + ((s2^2/n2)^2)/(n2-1))

## Test statistic distribution
x <- seq(-4, 4, by = 0.01)
y <- dt(x, df = v)
t_0.975 <- qt(0.975, df = v)
# plot(x, y, type = "l", col = "black", ylab = "Density(x)", main = "Distribution of the test statistic (v (108.26) degrees of freedom)")
# polygon(c(x[x>=t_0.975], max(x), t_0.975), c(y[x>=t_0.975], 0, 0), col="red")
# polygon(c(min(x), x[x<=-t_0.975], -t_0.975), c(y[x<=-t_0.975], 0, 0), col="red")
# text(2.0, 0.2,"Black: t(108.26)", col = "black")
# text(t_0.975, -0.01,"t0.975", col = "red")
# text(-t_0.975, -0.01,"-t0.975", col = "red")

ggplot(data.frame(x=x,y=y), aes(x=x, y=y)) +
  geom_line() +
  annotate(geom="text", x=2.0, y=0.2, label="Black: t(108.26)", col="black") +
  geom_polygon(data=data.frame(x=c(x[x>=t_0.975], max(x), t_0.975), y=c(y[x>=t_0.975], 0, 0)), aes(x=x, y=y), fill="red", alpha=0.75) +
  annotate(geom="text", x=t_0.975, y=-0.01, label="t_0.975", col="red") +
  geom_polygon(data=data.frame(x=c(min(x), x[x<=-t_0.975], -t_0.975), y=c(y[x<=-t_0.975], 0, 0)), aes(x=x, y=y), fill="red", alpha=0.75) +
  annotate(geom="text", x=-t_0.975, y=-0.01, label="-t_0.975", col="red") +
  labs(y="Density(x)", title="Distribution of the test statistic (v (108.26) degrees of freedom)")

## p-value, manually
p <- 2*(1-pt(abs(t_obs), df = v))

## Comparing the heat consumption of House 1 and 2
t.test(Dsel$Q1, Dsel$Q2)


###########################################################################
## Computing correlations

## Manually computing correlations
Dsub <- subset(D[, c("Q1","G")], !is.na(Q1) & !is.na(G))  ## We take a subset of values, where both Q1 and G exists
n <- sum(!is.na(Dsub$Q1))
mean_Q1 <- mean(Dsub$Q1)
mean_G <- mean(Dsub$G)
s_xy <- 1/(n-1) * sum((Dsub$Q1 - mean_Q1) * (Dsub$G - mean_G))  ## Covariance
r <- s_xy/(sd(Dsub$Q1) * sd(Dsub$G))  ## Correlation

## Scatterplot
# plot(Dsub$Q1, Dsub$G, type = "p", col = "black")
# abline(lm(Dsub$G ~ Dsub$Q1, data = Dsub), col = "blue")

## Scatterplot with car library
# scatterplot(Dsub$G ~ Dsub$Q1, data = Dsub, ylab = "Global radiation (G) [W/m^2]", xlab = "Heat consumption in House 1 (Q1) [kW/day]", main = "Scatter plot between the daily heat consumption of House 1 (Q1) and the global radiation (G)")

## Scatterplot with ggplot
ggplot(Dsub, aes(x=Q1, y=G)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE) + # Regression line, without confidence interfal
  geom_smooth(linetype="dashed") + # Loess method
  labs(x="Heat consumption in House 1 (Q1) [kW/day]", y="Global radiation (G) [W/m^2]", title="Scatter plot between the daily heat consumption of House 1 (Q1) and the global radiation (G)")

## Covariance between heat consumption and global radiation
cov(D[, c("Q1","G")], use="pairwise.complete.obs")
## Correlation between heat consumption and global radiation
cor(D[, c("Q1","G")], use="pairwise.complete.obs")
