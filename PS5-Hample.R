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