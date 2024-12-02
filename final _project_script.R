#load packages 
## Project:  STA 215, Fall 2024, Final Project
# Located:   Folcarelli TCNJ Google Drive
# File Name: template
# Date:      2024_1_17
# Who:       Dante Folcarelli 

## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
setwd("H:/sta215")

dataset <- read_csv("raw_data.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
dataset$lnwealth <- log(dataset$wealth)
dataset_nomissing <- na.omit(dataset)


table(dataset_nomissing$abortion)


summary(dataset_nomissing$wealth)
sd(dataset_nomissing$wealth)



##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################



anova <- aov(dataset_nomissing$lnwealth ~ dataset_nomissing$abortion) 


summary(anova)
boxplot(lnwealth ~ abortion, data = dataset_nomissing) 


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
dataset_nomissing_nomissing <- na.omit(dataset_nomissing[, c("lnwealth", "age")])

plot(dataset_nomissing_nomissing$age, dataset_nomissing_nomissing$lnwealth)
meany <- mean(dataset_nomissing_nomissing$age)
meanx <- mean(dataset_nomissing_nomissing$lnwealth)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(lnwealth ~ age, data =dataset_nomissing_nomissing) 
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset_nomissing_nomissing$age, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset_nomissing$age, dataset_nomissing$wealth)

chisq.test(table(dataset_nomissing$christian, dataset_nomissing$abortion))