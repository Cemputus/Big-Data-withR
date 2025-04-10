
# Libraries required
library(dplyr)
library(ggplot2)
library(data.table)
library(caret)
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

# 1. Data Reading
# Assuming data is in a CSV file named 'retail_spending.csv'
retail_data <- fread("retail_spending.csv", stringsAsFactors = FALSE)
cat("First 5 rows of Retail Spending Dataset:\n")
print(head(retail_data, 5))
cat("\nStructure of the dataset:\n")
str(retail_data)

# 2. Data Cleaning and Transformation
# Convert Channel and Region to factors with labels
retail_data[, Channel := factor(Channel, levels = c(1, 2), labels = c("Café", "Supermarket"))]
retail_data[, Region := factor(Region, levels = c(1, 2, 3), labels = c("City", "Village", "Town"))]
# Add Total_Spending column
retail_data[, Total_Spending := Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen]

# 3. Outlier Detection and Visualization (Before Treatment)
# Boxplot for Fresh spending to visualize outliers
p1 <- ggplot(retail_data, aes(x = Channel, y = Fresh, fill = Channel)) +
  geom_boxplot() +
  labs(title = "Fresh Spending by Channel (Before Outlier Treatment)", x = "Channel", y = "Fresh Spending") +
  theme_minimal()
print(p1)
ggsave("fresh_outliers_before.png", p1)

# Define a function to detect outliers using IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

# Identify outliers in Fresh spending
outliers_fresh <- retail_data[, detect_outliers(Fresh)]
cat("\nNumber of outliers in Fresh spending:", sum(outliers_fresh), "\n")

# 4. Handling Outliers (Cap them at upper and lower bounds)
retail_data[, Fresh := ifelse(Fresh > quantile(Fresh, 0.75, na.rm = TRUE) + 1.5 * IQR(Fresh, na.rm = TRUE),
                              quantile(Fresh, 0.75, na.rm = TRUE) + 1.5 * IQR(Fresh, na.rm = TRUE),
                              ifelse(Fresh < quantile(Fresh, 0.25, na.rm = TRUE) - 1.5 * IQR(Fresh, na.rm = TRUE),
                                     quantile(Fresh, 0.25, na.rm = TRUE) - 1.5 * IQR(Fresh, na.rm = TRUE),
                                     Fresh))]

# 5. Visualization After Outlier Treatment
p2 <- ggplot(retail_data, aes(x = Channel, y = Fresh, fill = Channel)) +
  geom_boxplot() +
  labs(title = "Fresh Spending by Channel (After Outlier Treatment)", x = "Channel", y = "Fresh Spending") +
  theme_minimal()
print(p2)
ggsave("fresh_outliers_after.png", p2)

# 6. Exploratory Data Analysis (EDA)
# Summary statistics of Fresh spending by Channel
summary_stats <- retail_data[, .(Mean = mean(Fresh), Median = median(Fresh), SD = sd(Fresh)), by = Channel]
cat("\nSummary Statistics of Fresh Spending by Channel:\n")
print(summary_stats)

# Bar plot of average Grocery spending by Region
p3 <- ggplot(retail_data, aes(x = Region, y = Grocery, fill = Region)) +
  stat_summary(fun = mean, geom = "bar") +
  labs(title = "Average Grocery Spending by Region", x = "Region", y = "Average Grocery Spending") +
  theme_minimal()
print(p3)
ggsave("grocery_by_region.png", p3)

# 7. Predictive Analysis (Linear Regression)
# Prepare data for modeling
model_data <- retail_data[, .(Grocery, Fresh, Detergents_Paper)]
model_data <- na.omit(model_data)

# Split into training and testing sets (80% train, 20% test)
train_index <- createDataPartition(model_data$Grocery, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train linear regression model
model <- lm(Grocery ~ Fresh + Detergents_Paper, data = train_data)

# Predict on test data
predictions <- predict(model, newdata = test_data)

# Evaluate model
rmse <- sqrt(mean((test_data$Grocery - predictions)^2))
r_squared <- summary(model)$r.squared
cat("\nLinear Regression Model Evaluation:\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")
cat("Interpretation: The model explains", round(r_squared * 100, 2), "% of the variance in Grocery spending.\n")