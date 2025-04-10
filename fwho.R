# importing the neccessary libraries  
library(tidyverse)
library(tidyr)
library(tidyselect)
library(ggplot2)
library(data.table)
library(readxl)
library(readr)
library(psych)
library(writexl)
library(sparklyr)
library(caret)

# Install and configure Spark (run once if not already set up)
# spark_install(version = "3.4.0")  # Adjust version as needed
sc <- spark_connect(master = "local")  # Connect to local Spark instance

# Set seed for reproducibility
set.seed(123)

# 1. Data Reading
retail_data <- fread("retail_spending.csv", stringsAsFactors = FALSE)
cat("First 5 rows of Retail Spending Dataset:\n")
print(head(retail_data, 5))
cat("\nStructure of the dataset:\n")
str(retail_data)

# 2. Data Cleaning and Transformation
setDT(retail_data)
retail_data <- retail_data %>%
  mutate(
    Channel = factor(Channel, levels = c(1, 2), labels = c("Café", "Supermarket")),
    Region = factor(Region, levels = c(1, 2, 3), labels = c("City", "Village", "Town")),
    Total_Spending = Fresh + Milk + Grocery + Frozen + Detergents_Paper + Delicassen
  )

# 3. Outlier Detection and Visualization (Before Treatment)
p1 <- retail_data %>%
  ggplot(aes(x = Channel, y = Fresh, fill = Channel)) +
  geom_boxplot() +
  labs(title = "Fresh Spending by Channel (Before Outlier Treatment)", 
       x = "Channel", y = "Fresh Spending") +
  theme_minimal()
print(p1)
ggsave("fresh_outliers_before.png", p1)

detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

outliers_fresh <- retail_data[, detect_outliers(Fresh)]
cat("\nNumber of outliers in Fresh spending:", sum(outliers_fresh), "\n")

# 4. Handling Outliers
retail_data <- retail_data %>%
  mutate(
    Fresh = case_when(
      Fresh > quantile(Fresh, 0.75, na.rm = TRUE) + 1.5 * IQR(Fresh, na.rm = TRUE) ~ 
        quantile(Fresh, 0.75, na.rm = TRUE) + 1.5 * IQR(Fresh, na.rm = TRUE),
      Fresh < quantile(Fresh, 0.25, na.rm = TRUE) - 1.5 * IQR(Fresh, na.rm = TRUE) ~ 
        quantile(Fresh, 0.25, na.rm = TRUE) - 1.5 * IQR(Fresh, na.rm = TRUE),
      TRUE ~ Fresh
    )
  )

# 5. Visualization After Outlier Treatment
p2 <- retail_data %>%
  ggplot(aes(x = Channel, y = Fresh, fill = Channel)) +
  geom_boxplot() +
  labs(title = "Fresh Spending by Channel (After Outlier Treatment)", 
       x = "Channel", y = "Fresh Spending") +
  theme_minimal()
print(p2)
ggsave("fresh_outliers_after.png", p2)

# 6. Exploratory Data Analysis (EDA)
summary_stats <- retail_data[, .(Mean = mean(Fresh), Median = median(Fresh), SD = sd(Fresh)), by = Channel]
cat("\nSummary Statistics of Fresh Spending by Channel:\n")
print(summary_stats)

p3 <- retail_data %>%
  group_by(Region) %>%
  summarise(Avg_Grocery = mean(Grocery)) %>%
  ggplot(aes(x = Region, y = Avg_Grocery, fill = Region)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Grocery Spending by Region", 
       x = "Region", y = "Average Grocery Spending") +
  theme_minimal()
print(p3)
ggsave("grocery_by_region.png", p3)

# 7. Predictive Analysis (Linear Regression with sparklyr)
# Load data into Spark
spark_retail <- copy_to(sc, retail_data, "spark_retail", overwrite = TRUE)

# Prepare data for modeling (select relevant columns)
model_data <- spark_retail %>%
  select(Grocery, Fresh, Detergents_Paper) %>%
  na.omit()

# Split into training and testing sets (80% train, 20% test)
splits <- model_data %>%
  sdf_random_split(training = 0.8, test = 0.2, seed = 123)
train_data <- splits$training
test_data <- splits$test

# Train linear regression model with sparklyr
model <- ml_linear_regression(train_data, Grocery ~ Fresh + Detergents_Paper)

# Predict on test data
predictions <- ml_predict(model, test_data) %>%
  collect()  # Bring predictions back to R

# Evaluate model
rmse <- sqrt(mean((predictions$Grocery - predictions$prediction)^2))
r_squared <- model$summary$r_squared
cat("\nSpark Linear Regression Model Evaluation:\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")
cat("Interpretation: The model explains", round(r_squared * 100, 2), "% of the variance in Grocery spending.\n")

# Disconnect from Spark
spark_disconnect(sc)

