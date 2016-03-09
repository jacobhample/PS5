## Jacob Hample
## Professor Montgomery
## Applied Statistical Programming
## March 10, 2026


### Problem Set 5 ###

# Sets working directory
setwd("~/Google Drive/Senior Year/Spring 2016/Statistical Programming/Problem Sets/PS5")

# Sets seed so that randomization is constant
set.seed(12435)

# Keeps inputted strings from being categorized as factors
options(stringsAsFactors=F)

# Additional packages used
library(foreign)

# Reads in data
anes <- read.dta("anes_timeseries_2012_stata12.dta")


### Question 1

## Selecting Variables
# VARIABLE 1 : finance_finnext

# Question posed:
# Now looking ahead, do you think that a year from now 
# [you / you and your family living here] will be BETTER OFF financially, 
# WORSE OFF, or JUST ABOUT THE SAME as now?

# Recoding finance_finnext variable to make it more legible and remove missing responses
anes$finance_finnext <- as.factor(ifelse(anes$finance_finnext == "1. Better","Better",
                                  ifelse(anes$finance_finnext == "2. Worse","Worse",
                                  ifelse(anes$finance_finnext == "3. The same", "Same",
                                  NA))))

# VARIABLE 2: health_insured

# Question posed:
# Do you presently have any kind of health insurance?

# Recoding health_insured variable to make it more legible and remove missing responses
anes$health_insured <- as.factor(ifelse(anes$health_insured == "1. Yes","Yes",
                                 ifelse(anes$health_insured == "2. No","No",
                                 NA)))

# VARIABLE 3: relig_import

# Question posed:
# Do you consider religion to be an IMPORTANT part of your life, or NOT?

# Recoding relig_import variable to make it more legible and remove missing responses
anes$relig_import <- as.factor(ifelse(anes$relig_import == "1. Important","Important",
                               ifelse(anes$relig_import == "2. Not important","Not important",
                               NA)))

## Creating Models
# Obama's feeling thermometer score as function of personal financial optimism
model1 <- lm(ft_dpc ~ finance_finnext, anes)
model1

# Obama's feeling thermometer score as function of presence of health insurance
model2 <- lm(ft_dpc ~ health_insured, anes)
model2

# Obama's feeling thermometer score as function of importance of religion
model3 <- lm(ft_dpc ~ relig_import, anes)
model3










## Jonathan's Code:

## model Obama's feeling thermometer score as function
## of Clinton's feeling thermometer score
model1 <- lm(ft_dpc ~ ft_hclinton, anes)

## make a prediction for a single observation with
## hypothetical clinton score of 77
predict(model1, data.frame(ft_hclinton=77))
## we would expect a Obama score of 71.7


## Question 1
## randomly subset the data into two partitions
## use "training set" to build at least three models 
## of Obama's feeling thermometer score
## document carefully how you deal with missingness