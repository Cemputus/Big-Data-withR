#### SUPERVISED LEARNING: REGRESSION ####
# Regression models are used to predict continuous target variables by assuming a linear relationship.
# Two main types:
# 1. Simple Linear Regression – with one independent variable.
# 2. Multiple Linear Regression – with multiple independent variables.

# Set working directory
setwd("~/Data_Science/year2/semester 2/Big Data -R/Practicals in Rstudio and Using Sparklyr-20250225")

# Install required packages
install.packages(c("corrplot", "nortest", "ISLR", "Hmisc", "ModelMetrics", "lmtest", 
                   "car", "olsrr", "moments", "bestNormalize", "magrittr", "rpart.plot"))

# Load necessary libraries
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caTools)
library(ggplot2)
library(caret)
library(Boruta)
library(cvms)
library(dplyr)
library(MASS)
library(corrplot)
library(nortest)
library(ISLR)
library(Hmisc)
library(ModelMetrics)
library(lmtest)
library(data.table)
library(car)
library(olsrr)
library(moments)
library(bestNormalize)
library(magrittr)
library(ggcorrplot)
library(readxl)

#### SIMPLE LINEAR REGRESSION ####
# Goal: Predict "Revenue" using one predictor variable.

## A. Feature Selection ##
# Load dataset
Data <- read_xlsx("Bike_Sales_New.xlsx") %>% view()

# Convert categorical columns to factors
categorical_cols <- c("Date", "Month", "Age_Group", "Customer_Gender", "Country",
                      "State", "Sub_Category", "Product_Category", "Product")
Data[categorical_cols] <- lapply(Data[categorical_cols], as.factor)

# Split data into categorical and continuous subsets
Cat <- Data[c("Date", "Month", "Age_Group", "Customer_Gender", "Sub_Category", 
              "Product", "Product_Category", "Country", "State")]
Cont <- Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", 
               "Unit_Price", "Profit", "Cost")]

## B. Explore Categorical Relationships with Revenue ##
check_anova <- function(var) {
  model <- aov(Revenue ~ Data[[var]], data = Data)
  print(summary(model))
  print(TukeyHSD(model))
}
sapply(names(Cat), check_anova)

# Retain: Age_Group, Customer_Gender, Product_Category, Product, Sub_Category

## C. Explore Continuous Relationships with Revenue ##
check_corr <- function(var) {
  print(cor.test(Data$Revenue, Data[[var]], method = "pearson", use = "complete.obs"))
}
sapply(names(Cont), check_corr)

# Retain: Unit_Cost, Unit_Price, Profit, Cost

# Correlation matrix
Cont2 <- Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", 
                "Unit_Price", "Profit", "Cost", "Revenue")]
rcorr(as.matrix(Cont2))
corrplot(cor(Cont2))
ggcorrplot(cor(Cont2))

## D. Prepare Final Dataset ##
Data_final <- Data[c("Unit_Cost", "Unit_Price", "Profit", "Cost", "Age_Group", 
                     "Customer_Gender", "Sub_Category", "Product", 
                     "Product_Category", "Revenue")]

## E. Train-Test Split ##
set.seed(123)
split <- sample.split(Data_final$Revenue, SplitRatio = 0.7)
train_set <- subset(Data_final, split == TRUE)
test_set <- subset(Data_final, split == FALSE)

## F. Simple Linear Regression Model ##
plot(train_set$Unit_Cost, train_set$Revenue)
linear_model <- lm(Revenue ~ Unit_Cost, data = train_set)
summary(linear_model)

# Visualize
ggplot(data = train_set, aes(x = Unit_Cost, y = Revenue)) +
  geom_point() +
  stat_smooth(method = lm)

#### MULTIPLE LINEAR REGRESSION ####
# Goal: Predict "Revenue" using multiple predictors.

# Reload data to ensure it's clean
Data <- read.csv("Bike_Sales_New.csv")
Data[categorical_cols] <- lapply(Data[categorical_cols], as.factor)
Data <- subset(Data, select = -c(X))  # Drop column X if present

# Reuse selected predictors from earlier
Data_final <- Data[c("Unit_Cost", "Unit_Price", "Profit", "Cost", "Age_Group", 
                     "Customer_Gender", "Sub_Category", "Product", 
                     "Product_Category", "Revenue")]

## A. Train-Test Split ##
set.seed(123)
split <- sample.split(Data_final$Revenue, SplitRatio = 0.7)
train_set <- subset(Data_final, split == TRUE)
test_set <- subset(Data_final, split == FALSE)

## B. Fit Multiple Linear Regression Model ##
multi_model <- lm(Revenue ~ ., data = train_set)
summary(multi_model)

## C. Check for Multicollinearity ##
vif(multi_model)  # VIF > 5 indicates multicollinearity concerns

## D. Model Diagnostics ##
par(mfrow = c(2, 2))
plot(multi_model)

# Residual normality
hist(residuals(multi_model), main = "Residuals Histogram", col = "lightblue", breaks = 20)
qqnorm(residuals(multi_model)); qqline(residuals(multi_model), col = "red")
ad.test(residuals(multi_model))  # Anderson-Darling test

# Homoscedasticity
bptest(multi_model)  # Breusch-Pagan test

## E. Predict on Test Set ##
predictions <- predict(multi_model, newdata = test_set)

# Evaluate performance
actual <- test_set$Revenue
MAE <- mean(abs(predictions - actual))
RMSE <- sqrt(mean((predictions - actual)^2))
R2 <- 1 - sum((actual - predictions)^2) / sum((actual - mean(actual))^2)

cat("Model Performance on Test Set:\n")
cat("MAE:", MAE, "\n")
cat("RMSE:", RMSE, "\n")
cat("R-squared:", R2, "\n")

## F. Visualize Predicted vs Actual ##
ggplot(data = NULL, aes(x = actual, y = predictions)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(title = "Predicted vs Actual Revenue",
       x = "Actual Revenue",
       y = "Predicted Revenue") +
  theme_minimal()
