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
table(dataset$song_length)
mean(dataset$song_length)
sd(dataset$song_length)

table(dataset$number_of_listens)
mean(dataset$number_of_listens)
sd(dataset$number_of_listens)

table(dataset$song_descriptor)

table(dataset$song_topic)

table(dataset$personal_enjoyment)
mean(dataset$personal_enjoyment)
sd(dataset$personal_enjoyment)

table(dataset$album)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
dataset$lnwealth <- log(dataset$wealth)
dataset_anova <- na.omit(dataset[, c("lnwealth", "abortion")])


anova <- aov(dataset_anova$lnwealth ~ dataset_anova$abortion) 


summary(anova)
boxplot(lnwealth ~ abortion, data = dataset_anova) 


##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
dataset_nomissing <- na.omit(dataset[, c("lnwealth", "age")])

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
table(dataset$age, dataset$wealth)

chisq.test(table(dataset$christian, dataset$abortion))