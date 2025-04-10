# Comprehensive R Script: EDA to Predictive Analysis
# Libraries required
library(dplyr)
library(ggplot2)
library(data.table)
library(lubridate)
library(caret)
library(tidyverse)
library(tidyselect)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(data.table)
library(readxl)
library(readr)
library(psych)
library(writexl)


# Set seed for reproducibility
set.seed(123)

# 1. Simulate a mock dataset (replace this with your own data import if available)
data <- data.frame(
  date = seq(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day"),
  region = sample(c("North", "South", "East", "West"), 731, replace = TRUE),
  category = sample(c("Electronics", "Clothing", "Food"), 731, replace = TRUE),
  sales = rnorm(731, mean = 1000, sd = 300),
  price = rnorm(731, mean = 50, sd = 10)
)

# Add some missing values for realism
data$sales[sample(1:731, 50)] <- NA

# Convert to data.table for efficiency
data <- as.data.table(data)

view(data)
# 2. Data Cleaning
# Check for missing values
cat("Missing values in each column:\n")
print(colSums(is.na(data)))

# Impute missing sales with median by category
data[, sales := ifelse(is.na(sales), median(sales, na.rm = TRUE), sales), by = category]

# Convert date and extract month
data[, date := as.Date(date)]
data[, month := month(date)]

# 3. Exploratory Data Analysis (EDA)
# Summary statistics
cat("\nSummary of sales by region and category:\n")
eda_summary <- data[, .(total_sales = sum(sales), avg_sales = mean(sales)), by = .(region, category)]
print(eda_summary)

view(eda_summary)

## checking for outliers

## dealing with outliers


# Visualization: Boxplot of sales by region
ggplot(data, aes(x = region, y = sales, fill = region)) +
  geom_boxplot() +
  labs(title = "Sales Distribution by Region", x = "Region", y = "Sales") +
  theme_minimal()

class(data)

# Correlation between sales and price
cor_sales_price <- cor(data$sales, data$price, use = "complete.obs")
cat("\nCorrelation between sales and price:", cor_sales_price, "\n")

## finding the correlation matrix and heatmap


# Scatterplot with regression line
ggplot(data, aes(x = price, y = sales)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Sales vs Price", x = "Price", y = "Sales") +
  theme_minimal()

# 4. Big Data Processing (Simulate large data)
# Replicate dataset 100 times
large_data <- rbindlist(replicate(100, data, simplify = FALSE))
cat("\nDimensions of large dataset:", dim(large_data), "\n")

# Time a grouped operation
system.time({
  large_summary <- large_data[, .(avg_sales = mean(sales)), by = .(region, category)]
})
cat("\nLarge dataset summary:\n")
print(head(large_summary))

# 5. Predictive Modeling
# Prepare data for modeling
model_data <- data[, .(sales, region, category, month)]
model_data <- na.omit(model_data)

# Split into training and testing sets (70% train, 30% test)
train_index <- createDataPartition(model_data$sales, p = 0.7, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train a linear regression model
model <- lm(sales ~ region + category + month, data = train_data)

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Evaluate model
rmse <- sqrt(mean((test_data$sales - predictions)^2))
r_squared <- summary(model)$r.squared
cat("\nModel Evaluation:\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Interpretation
cat("The model explains", round(r_squared * 100, 2), "% of the variance in sales.\n")

