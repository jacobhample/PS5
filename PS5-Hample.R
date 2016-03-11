## Jacob Hample
## Professor Montgomery
## Applied Statistical Programming
## March 10, 2016


### Problem Set 5 ###

# Sets working directory
setwd("~/Google Drive/Senior Year/Spring 2016/Statistical Programming/Problem Sets/PS5")

# Sets seed so that randomization is constant
set.seed(12435)

# Keeps inputted strings from being categorized as factors
options(stringsAsFactors = FALSE)

# Additional packages used
library(foreign)

# Reads in data
anes <- read.dta("anes_timeseries_2012_stata12.dta")



### Question 1


## Selecting and Cleaning Variables

# DEPENDENT VARIABLE: ft_dpc
# Description: 
# Feeling Thermometer, Democratic Presidential Candidate

# Replaces negative thermometer ratings with NAs
anes$ft_dpc <- ifelse(anes$ft_dpc < 0, NA, anes$ft_dpc)


# INDEPENDENT VARIABLE 1: finance_finpast

# Question posed:
# We are interested in how people are getting along financially these days. 
# Would you say that [you/you and your family living here] are BETTER off or 
# WORSE off than you were a year ago?

# Recodes finance_finpast to make it more legible and replaces missing responses with NAs
anes$finance_finpast <- as.factor(ifelse(anes$finance_finpast == "1. Better", "1. Better",
                                  ifelse(anes$finance_finpast == "2. Worse", "2. Worse",
                                  ifelse(anes$finance_finpast == "3. The same {VOL}", "3. Same",
                                  NA))))


# INDEPENDENT VARIABLE 2 : finance_finnext

# Question posed:
# Now looking ahead, do you think that a year from now 
# [you / you and your family living here] will be BETTER OFF financially, 
# WORSE OFF, or JUST ABOUT THE SAME as now?

# Recodes finance_finnext to make it more legible and replaces missing responses with NAs
anes$finance_finnext <- as.factor(ifelse(anes$finance_finnext == "1. Better", "1. Better",
                                  ifelse(anes$finance_finnext == "2. Worse", "2. Worse",
                                  ifelse(anes$finance_finnext == "3. The same", "3. Same",
                                  NA))))


# INDEPENDENT VARIABLE 3: dem_edugroup_x
# Description: 
# PRE: SUMMARY- R level of highest education (group)

# Replaces missing responses with NAs
anes$dem_edugroup_x <- as.factor(ifelse(anes$dem_edugroup_x == "1. Less than high school credential", "1. Less than high school credential",
                                 ifelse(anes$dem_edugroup_x == "2. High school credential", "2. High school credential",
                                 ifelse(anes$dem_edugroup_x == "3. Some post-high-school, no bachelor's degree", "3. Some post-high-school, no bachelor's degree",
                                 ifelse(anes$dem_edugroup_x == "4. Bachelor's degree", "4. Bachelor's degree",
                                 ifelse(anes$dem_edugroup_x == "5. Graduate degree", "5. Graduate degree",
                                 NA))))))


# INDEPENDENT VARIABLE 4: dem2_numchild
# Description: 
# Total Number of children in HH

# Recodes dem2_numchild as a dummy variable for presence of children and replaces missing responses with NAs
anes$dem2_numchild <- as.numeric(ifelse(anes$dem2_numchild == "0. No children in household", 0,
                                 ifelse(anes$dem2_numchild == "1. One child in household", 1,
                                 ifelse(anes$dem2_numchild == "2. Two children in household", 1,
                                 ifelse(anes$dem2_numchild == "3. Three or more children in household", 1,
                                 NA)))))


# INDEPENDENT VARIABLE 5: health_insured

# Question posed:
# Do you presently have any kind of health insurance?

# Recodes health_insured as a dummy variable and replaces missing responses with NAs
anes$health_insured <- as.numeric(ifelse(anes$health_insured == "1. Yes", 1,
                                  ifelse(anes$health_insured == "2. No", 0,
                                  NA)))


# INDEPENDENT VARIABLE 6: interest_attention

# Question posed:
# How often do you pay attention to what's going on in government and politics? 
# [ALWAYS, MOST OF THE TIME, ABOUT HALF THE TIME, SOME OF THE TIME, or NEVER / 
# NEVER, SOME OF THE TIME, ABOUT HALF THE TIME, MOST OF THE TIME, or ALWAYS]?

# Replaces missing responses with NAs
anes$interest_attention <- as.factor(ifelse(anes$interest_attention == "1. Always", "1. Always",
                                     ifelse(anes$interest_attention == "2. Most of the time", "2. Most of the time",
                                     ifelse(anes$interest_attention == "3. About half the time", "3. About half the time",
                                     ifelse(anes$interest_attention == "4. Some of the time", "4. Some of the time",
                                     ifelse(anes$interest_attention == "5. Never", "5. Never",
                                     NA))))))


# INDEPENDENT VARIABLE 7: relig_import

# Question posed:
# Do you consider religion to be an IMPORTANT part of your life, or NOT?

# Recodes relig_import variable as a dummy variable and replaces missing responses with NAs
anes$relig_import <- as.numeric(ifelse(anes$relig_import == "1. Important", 1,
                                ifelse(anes$relig_import == "2. Not important", 0,
                                NA)))


# INDEPENDENT VARIABLE 8: dem_racecps_black

# Description: PRE: 
# Race self-identification: mention Black

# Recodes dem_racecps_black as a dummy variable and replaces missing responses with NAs
anes$dem_racecps_black <- as.numeric(ifelse(anes$dem_racecps_black == "0. Not selected by R", 0,
                                     ifelse(anes$dem_racecps_black == "1. Selected by R", 1,
                                     NA)))


# INDEPENDENT VARIABLE 9: dem_veteran

# Question posed:
# Did you ever serve on active duty in the U. S. Armed Forces?

# Recodes dem_veteran as a dummy variable and replaces missing responses with NAs
anes$dem_veteran <- as.numeric(ifelse(anes$dem_veteran == "1. Yes", 1,
                               ifelse(anes$dem_veteran == "2. No", 0,
                               NA)))


## Splitting Data
# Separates dataset into training and test halves
train <- sample(dim(anes)[1], dim(anes)[1] / 2, replace = FALSE)
training.anes <- anes[train, ]
test.anes <- anes[-train, ]


## Creating Models
# Obama's feeling thermometer score as a function of past financial results, financial optimism, and education level
model1 <- lm(ft_dpc ~ finance_finpast + finance_finnext + dem_edugroup_x, training.anes)
model1

# Obama's feeling thermometer score as a function of presence of children, presence of health insurance, and political interest level
model2 <- lm(ft_dpc ~ dem2_numchild + health_insured + interest_attention, training.anes)
model2

# Obama's feeling thermometer score as a function of religious importance, race, and military experience
model3 <- lm(ft_dpc ~ relig_import + dem_racecps_black + dem_veteran, training.anes)
model3



### Question 2

# Predicts each model using the test data and stores the predictions in vectors
predict1 <- c(predict(model1, newdata = test.anes))
predict2 <- c(predict(model2, newdata = test.anes))
predict3 <- c(predict(model3, newdata = test.anes))

# Combines predictions of each model into a data frame
predictALL <- data.frame(cbind(predict1, predict2, predict3))

# Creates vector NAvalues with indices of rows with NAs
NAvalues1 <- which(is.na(predictALL[1]))
NAvalues2 <- which(is.na(predictALL[2]))
NAvalues3 <- which(is.na(predictALL[3]))
NAvalues <- sort(unique(c(NAvalues1, NAvalues2, NAvalues3)))

# Removes indices with NAs from predictALL
predictALL <- predictALL[-NAvalues, ]



### Question 3 & 4

# Creates matrix of actual Obama feeling thermometer values
therm.Obama <- matrix(test.anes$ft_dpc)

# Removes indices with NAs from therm.Obama
therm.Obama <- matrix(therm.Obama[-NAvalues, ])

## Fit Statistic Functions

#' RMSE
#' 
#' This function evaluates the fit of a model by calculating the Root Mean Squared Error
#' @param predicted, Matrix of predicted values
#' @param actual, Matrix of actual values
#' @param model, Integer signifying which model to assess
#' @return Root Mean Squared Error of the model

RMSE <- function (predicted, actual, model) {
  error <- abs(predicted[, model] - actual)
  return(sqrt(mean(error^2, na.rm = TRUE)))
}

#' MAD
#' 
#' This function evaluates the fit of a model by calculating the Median Absolute Deviation
#' @param predicted, Matrix of predicted values
#' @param actual, Matrix of actual values
#' @param model, Integer signifying which model to assess
#' @return Median Absolute Deviation of the model

MAD <- function (predicted, actual, model) {
  error <- abs(predicted[, model] - actual)
  return(median(error, na.rm = TRUE))
}

#' RMSLE
#' 
#' This function evaluates the fit of a model by calculating the Root Mean Squared Logarithmic Error
#' @param predicted, Matrix of predicted values
#' @param actual, Matrix of actual values
#' @param model, Integer signifying which model to assess
#' @return Root Mean Squared Logarithmic Error of the model

RMSLE <- function (predicted, actual, model) {
  a <- log(predicted[, model] + 1) - log(actual + 1)
  return(sqrt(mean(a^2, na.rm = TRUE)))
}

#' MAPE
#' 
#' This function evaluates the fit of a model by calculating the Mean Absolute Percentage Error
#' @param predicted, Matrix of predicted values
#' @param actual, Matrix of actual values
#' @param model, Integer signifying which model to assess
#' @return Mean Absolute Percentage Error of the model

MAPE <- function (predicted, actual, model) {
  error <- abs(predicted[, model] - actual)
  percent.error <- error/actual * 100  ## some values of actual are 0, therefore error/actual = Inf in those cases
  return(mean(percent.error[percent.error != Inf], na.rm = TRUE)) ## removes said values from the MAPE calculation
}

#' MEAPE
#' 
#' This function evaluates the fit of a model by calculating the Median Absolute Percentage Error
#' @param predicted, Matrix of predicted values
#' @param actual, Matrix of actual values
#' @param model, Integer signifying which model to assess
#' @return Median Absolute Percentage Error of the model

MEAPE <- function (predicted, actual, model) {
  error <- abs(predicted[, model] - actual)
  percent.error <- error/actual * 100  ## some values of actual are 0, therefore error/actual = Inf in those cases
  return(median(percent.error[percent.error != Inf], na.rm = TRUE)) ## removes said values from the MAPE calculation
}

## Master Function (special thanks to Matt Malis whose code showed me how to do this properly)

#' FitStatistics
#' 
#' This function caluculates RMSE, MAD, RMSLE, MAPE, and/or MEAPE depending on user input
#' @param predicted, matrix of predicted values
#' @param actual, matrix of actual values
#' @params calcRMSE, calcMAD, calcRMSLE, calcMAPE, calcMEAPE: boolean of whether to calculate each of these statistics
#'        (default to FALSE for all)
#' @return matrix of fit statistics, calculated for the predicted values from existing models

FitStatistics <- function(predicted, actual, calcRMSE = FALSE, 
                                             calcMAD = FALSE, 
                                             calcRMSLE = FALSE, 
                                             calcMAPE = FALSE, 
                                             calcMEAPE = FALSE) {
  statValues <- matrix(nrow = 3)
  statNames <- c()
  if (calcRMSE) {
    statRMSE <- sapply(1:dim(predicted)[2], function(x) RMSE(predicted, actual, x))
    statValues <- cbind(statValues, (statRMSE))
    statNames <- c(statNames, "RMSE")
  }
  if (calcMAD) {
    statMAD <- sapply(1:dim(predicted)[2], function(x) MAD(predicted, actual, x))
    statValues <- cbind(statValues, (statMAD))
    statNames <- c(statNames, "MAD")
  }
  if (calcRMSLE) {
    statRMSLE <- sapply(1:dim(predicted)[2], function(x) RMSLE(predicted, actual, x))
    statValues <- cbind(statValues, (statRMSLE))
    statNames <- c(statNames, "RMSLE")
  }
  if (calcMAPE) {
    statMAPE <- sapply(1:dim(predicted)[2], function(x) MAPE(predicted, actual, x))
    statValues <- cbind(statValues, (statMAPE))
    statNames <- c(statNames, "MAPE")
  }
  if (calcMEAPE) {
    statMEAPE <- sapply(1:dim(predicted)[2], function(x) MEAPE(predicted, actual, x))
    statValues <- cbind(statValues, statMEAPE)
    statNames <- c(statNames, "MEAPE")
  }
  statValues <- data.frame(statValues[, -1])
  colnames(statValues) <- statNames 
  rownames(statValues) <- c("Model1", "Model2", "Model3") 
  return(statValues)
}



### Question 5

# Runs FitStatistics using all of the statistics to evaluate the accuracy of the models
FitStatistics(predictALL, therm.Obama, calcRMSE = TRUE, calcMAD = TRUE, calcRMSLE = TRUE, calcMAPE = TRUE, calcMEAPE = TRUE)

# Runs FitStatistics using RMSE to evaluate the accuracy of the models
FitStatistics(predictALL, therm.Obama, calcRMSE = TRUE)

# Judging by the fit statistics, Model1 seems to be the best. Model1 has the lowest
# fit statistics of all the models, though Model3 is close.

