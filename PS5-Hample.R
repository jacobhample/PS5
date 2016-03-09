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
options(stringsAsFactors = FALSE)

# Additional packages used
library(foreign)

# Reads in data
anes <- read.dta("anes_timeseries_2012_stata12.dta")


### Question 1


## Selecting and Cleaning Variables

# DEPENDENT VARIABLE: ft_dpc
# Description: Feeling Thermometer, Democratic Presidential Candidate

# Replaces negative thermometer ratings with NAs
anes$ft_dpc <- ifelse(anes$ft_dpc < 0, NA, anes$ft_dpc)

# INDEPENDENT VARIABLE 1 : finance_finnext

# Question posed:
# Now looking ahead, do you think that a year from now 
# [you / you and your family living here] will be BETTER OFF financially, 
# WORSE OFF, or JUST ABOUT THE SAME as now?

# Recodes finance_finnext variable to make it more legible and replaces misssing responses with NAs
anes$finance_finnext <- as.factor(ifelse(anes$finance_finnext == "1. Better","Better",
                                  ifelse(anes$finance_finnext == "2. Worse","Worse",
                                  ifelse(anes$finance_finnext == "3. The same", "Same",
                                  NA))))

# INDEPENDENT VARIABLE 2: health_insured

# Question posed:
# Do you presently have any kind of health insurance?

# Recodes health_insured variable as a dummy variable and replaces misssing responses with NAs
anes$health_insured <- as.numeric(ifelse(anes$health_insured == "1. Yes", 1,
                                  ifelse(anes$health_insured == "2. No", 0,
                                  NA)))

# INDEPENDENT VARIABLE 3: relig_import

# Question posed:
# Do you consider religion to be an IMPORTANT part of your life, or NOT?

# Recodes relig_import variable as a dummy variable and replaces misssing responses with NAs
anes$relig_import <- as.numeric(ifelse(anes$relig_import == "1. Important", 1,
                                ifelse(anes$relig_import == "2. Not important", 0,
                                NA)))


## Splitting Data
# Separates dataset into training and test halves
train <- sample(dim(anes)[1], dim(anes)[1] / 2, replace = FALSE)
training.anes <- anes[train, ]
test.anes <- anes[-train, ]


## Creating Models
# Obama's feeling thermometer score as function of personal financial optimism
model1 <- lm(ft_dpc ~ finance_finnext, training.anes)
model1

# Obama's feeling thermometer score as function of presence of health insurance
model2 <- lm(ft_dpc ~ health_insured, training.anes)
model2

# Obama's feeling thermometer score as function of importance of religion
model3 <- lm(ft_dpc ~ relig_import, training.anes)
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