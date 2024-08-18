#This project analyzes whether budget allocation in kathmandu metro is allocated
#according to the population or the number of votes gained by mayor (Balen) 
#the analysis is based on allocation of single year

#install necessary libraries
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(readxl)
setwd("D:/R") #set working directory
#Importing dataframe
ktm <- read_excel("ktm.xlsx")
View(ktm)
names(ktm)
# Regression 1: Budget vs. Population #taking populaiton as a single variable
model_population <- lm(LnB ~ LnP, data = ktm)
summary(model_population)

#plot
install.packages("ggplot2")
library(ggplot2)
ggplot(model_population, aes(x = LnP, y = LnB)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs. Population",
       x = "Population",
       y = "Allocated Budget") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))


# Regression 2: Budget vs. Vote Share #taking mayor's vote as a single variable
model_vote <- lm(LnB ~ LnBV, data = ktm)
summary(model_vote)

#plot
ggplot(model_vote, aes(x = LnBV, y = LnB)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Budget vs. Vote Share",
       x = "Mayor's Vote Share",
       y = "Allocated Budget") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#conclusion :compare the higher r square

#using two variables:multiple regression model
# Multiple Linear Regression: Budget vs. Population and Vote Share
model <- lm(LnB ~ LnP + LnBV, data = ktm)
summary(model)
#findings
#residual vs fitted plot for linearity check

# Install ggplot2 if you haven't already
install.packages("ggplot2")

# Load ggplot2
library(ggplot2)

#checking the assumption of OLS
# Extract residuals and fitted values from the model
residuals <- residuals(model)
fitted_values <- fitted(model)

# Create a data frame for plotting
plot_data <- data.frame(Fitted = fitted_values, Residuals = residuals)

# Generate the plot
# red line is flat so homoscedasticity assumption is met
ggplot(plot_data, aes(x = Fitted, y = Residuals)) +
  geom_point() +                 # Scatter plot of residuals
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a regression line
  labs(title = "Residuals vs Fitted",
       x = "Fitted values",
       y = "Residuals") +
  theme_minimal() +             # Apply a minimal theme for aesthetics
  theme(plot.title = element_text(hjust = 0.5))  # Center the plot title

#for independence test (durbin and watson) for autocorrelation

# Install the car package if not already installed
install.packages("car")
library(car)

# Durbin-Watson Test for autocorrelation
durbinWatsonTest(model)

#finding: no autocorrelation

#cheching assumtion of homoscedasticity 
# Scale-Location Plot
#plot(model, which = 3)
#plot(model, which = 2) # Q-Q plot in base R
# Install lmtest package if not already installed
install.packages("lmtest")

# Load lmtest package
library(lmtest)

# Perform Breusch-Pagan test for statistical (heteroscaadisticity)
bp_test <- bptest(model)

# Print test result
print(bp_test)




#normality of residuals
# Extract residuals from the model
residuals <- residuals(model)

# Create a data frame for plotting
plot_data <- data.frame(Residuals = residuals)

# Generate the Q-Q Plot using ggplot2 for normality test #this should go up
ggplot(plot_data, aes(sample = Residuals)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal()


#multicollinearity check
# VIF Test
vif(model)
#less than 10  so good








