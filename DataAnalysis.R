# Finn Tomasula Martin
# COSC-4557
# Exploratory Data Analysis
# This file contains code for analyzing the winequality-red.csv and primary-tumor.arff datasets

# Clear environment before each run
rm(list = ls())

# Load in libraries
library(polycor)

# Load in data
wine <- read.csv("winequality-red.csv", sep=";")
tumor <- read.csv("primary-tumor.csv")
tumor[tumor == ""] <- NA

# Check for missing values
missWine <- sum(is.na(wine))
missTumor <- sum(is.na(tumor))

# Remove rows with missing values
newTumor <- na.omit(tumor)

# Create histograms for all features to examine the distribution of data
par(mfrow = c(4, 3), mar = c(2, 2, 2, 2))
for (col in names(wine)) {
  hist(wine[[col]], main = col, xlab = col, col = "skyblue", border = "white")
}

par(mfrow = c(6, 3), mar = c(2, 2, 2, 2))
for (col in names(tumor)) {
  tab <- table(tumor[[col]])
  barplot(tab, main = col, xlab = col, col = "skyblue", border = "white")
}

# Normalize the data
normalWine <- as.data.frame(log(wine))
par(mfrow = c(4, 3))
for (col in names(normalWine)) {
  hist(normalWine[[col]], main = col, xlab = col, col = "skyblue", border = "white")
}

# Check for multicollinearity
cor(wine)
hetcor(newTumor)








