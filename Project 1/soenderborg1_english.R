###########################################################################
## Name: Marcus Wahlers Sand
## Student Number: s215827
## Project: Project 1 - Heating in Sønderborg
## 02323 Introduction to Statistics - Fall 22

###########################################################################
## Get dependencies

## If RStudioAPI is not installed, install it (should be installed with RStudio, but can also be installed without)
## We use it for getting the current directory
if (!require("rstudioapi")) install.packages("rstudioapi")
## purrr is a dependency for RStudioAPI to get current directory
if (!require("purrr")) install.packages("purrr")

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
hist(D$Q1, xlab="Heat consumption (House 1) [kW/day]", ylab="Density", prob=TRUE, main="Histogram of empirical density of the daily heat consumption (House 1)")


###########################################################################
## Conversion to a date variable

## Converts the variable 't' to a date variable in R
D$t <- as.Date(x=D$t, format="%Y-%m-%d")
## Checks the result
summary(D$t)


###########################################################################
## Plot of data over time

## Plot of heat consumption over time
plot(D$t, D$Q1, type="l", xlim=as.Date(c("2008-10-02","2010-10-01")), 
     ylim=c(0,9), xlab="Date", xaxt="n", ylab="Heat consumption [kW/day]", col=2, main="Heat consumption over time (October 2nd, 2008 to October 1st, 2010)")
axis(1, D$t, format(D$t, "%b %d"), cex.axis=.7) # https://stackoverflow.com/a/4844261
lines(D$t, D$Q2, col=3)
lines(D$t, D$Q3, col=4)
lines(D$t, D$Q4, col=5)
## Add a legend
legend("topright", legend=paste0("Q", c(1,2,3,4)), lty=1, col=2:5)


###########################################################################
## Taking a subset

## Subset of the data: only Jan-Feb 2010
Dsel <- subset(D, "2010-01-01" <= t & t < "2010-3-01")


###########################################################################
## Box plot

## Box plot of daily heat consumption by house
boxplot(Dsel[ ,c("Q1","Q2","Q3","Q4")],
        xlab="House", ylab="Heat consumption [kW/day]", main="Box plot showing daily heat consumption by house (Jan-Feb 2010)")


###########################################################################
## Summary statistics for daily heat consumptions

## We use the code from Remark 2.2
selected <- c("Q1","Q2","Q3","Q4")
apply(Dsel[, selected], 2, function(x) {
  c(
    sum=sum(!is.na(x)),                     ## Total number of observations (doesn't include missing values if there are any)
    mean=mean(x, na.rm=TRUE),               ## Sample mean of daily heat consumption
    var=var(x, na.rm=TRUE),                 ## Sample variance of daily heat consumption
    dev=sd(x, na.rm=TRUE),                  ## Sample standard deviance
    lq=quantile(x, probs=0.25, na.rm=TRUE), ## Lower quartile, Q1
    median=median(x, na.rm=TRUE),           ## Median, Q2 (could also have used "quantile(x, probs=0.5, na.rm=TRUE)")
    hq=quantile(x, probs=0.75, na.rm=TRUE)  ## Upper quartile, Q3
  )
})

## The argument 'na.rm=TRUE' ensures that the statistic is
## computed even in cases where there are missing values.


###########################################################################
## qq-plot for model validation

## qq-plot of daily heat consumption (House 1)
qqnorm(Dsel$Q1)
qqline(Dsel$Q1)
## qq-plot of daily heat consumption (House 2)
qqnorm(Dsel$Q2)
qqline(Dsel$Q2)
## qq-plot of daily heat consumption (House 3)
qqnorm(Dsel$Q3)
qqline(Dsel$Q3)
## qq-plot of daily heat consumption (House 4)
qqnorm(Dsel$Q4)
qqline(Dsel$Q4)


###########################################################################
## Confidence interval for the mean

## CI for the mean daily heat consumption of House 1, manually
n <- sum(!is.na(Dsel$Q1))
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

##  Testing hypothesis mu=2.38 for daily heat consumption 
## (House 1, Jan-Feb 2010)
t.test(Dsel$Q1, mu=2.38)


###########################################################################
## Welch t-test for comparing two (independent) samples

## Comparing the heat consumption of House 1 and 2
t.test(Dsel$Q1, Dsel$Q2)


###########################################################################
## Computing correlations

## Correlation between heat consumption and global radiation
cor(D[, c("Q1","G")], use="pairwise.complete.obs")


###########################################################################
## Subsets in R

## Optional extra remark about taking subsets in R
##
## A logical vector with TRUE or FALSE for each row in D, e.g.:
## Finding days with frost
D$Ta < 0
## Can be used to extract heat consumptions for House 1 on 
## days with frost
D$Q1[D$Ta < 0]
## The 'subset' function can be used as well
subset(D, Ta < 0)
## More complex logical expressions can be made, e.g.:
## Observations from days with frost before 2010
subset(D, t < "2010-01-01" & Ta < 0)


###########################################################################
## More R tips

## Use a 'for'-loop to calculate the summary statistics for each house
## and assign the result to a new data.frame
Tbl <- data.frame()
## Find the relevant columns
selected <- c("Q1","Q2","Q3","Q4")
## Calculate the summary statistics for each house
for(i in selected){
  ## Take the relevant column
  x <- Dsel[, i]
  ## Compute the sample mean
  Tbl[i, "mean"] <- mean(x, na.rm=TRUE)
  ## Compute the sample variance
  Tbl[i, "var"] <- var(x, na.rm=TRUE) }
## View the content of Tbl
Tbl
  
## In R there are even more condensed ways to do such 
## calculations, e.g.:
apply(Dsel[, selected], 2, mean, na.rm=TRUE)
## or several calculations in one go
apply(Dsel[, selected], 2, function(x){
  c(mean=mean(x, na.rm=TRUE),
    var=var(x, na.rm=TRUE)) })
## See more useful functions with: ?apply, ?aggregate and ?lapply
## For extremely efficient data handling see, e.g., the packages: 
## dplyr, tidyr, reshape2 and ggplot2

## LaTeX tips:
## The R package "xtable" can generate LaTeX tables written to a file 
## and thereby they can automatically be included in a .tex document.
## The R package "knitr" can be used very elegantly to generate .tex 
## documents with R code written directly in the document. This  
## document and the book were generated using knitr.
