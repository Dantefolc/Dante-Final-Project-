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
dataset$lnwealth <- log(dataset$wealth+1)
dataset_nomissing <- na.omit(dataset)


table(dataset_nomissing$gun_policy)
table(dataset_nomissing$christian)

summary(dataset_nomissing$age)
sd(dataset_nomissing$age)



##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################



anova <- aov(dataset_nomissing$lnwealth ~ dataset_nomissing$abortion) 


summary(anova)
boxplot(lnwealth ~ abortion, data = dataset_nomissing) 


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################

plot(dataset_nomissing$age, dataset_nomissing$lnwealth)
meany <- mean(dataset_nomissing$age)
meanx <- mean(dataset_nomissing$lnwealth)
abline(h = meanx, col = "black")
abline(v = meany, col = "black")
linear_relationship <- lm(lnwealth ~ age, data =dataset_nomissing) 
summary(linear_relationship)
abline(linear_relationship, col = "red")

##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(dataset_nomissing$age, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")


##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(dataset_nomissing$christian, dataset_nomissing$abortion)

chisq.test(table(dataset_nomissing$christian, dataset_nomissing$abortion))