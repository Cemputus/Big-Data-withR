
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
# Assuming data is in a CSV file named 'loan_approval.csv'
loan_data <- fread("loan_approval.csv", stringsAsFactors = FALSE)
cat("First 5 rows of Loan Approval Dataset:\n")
print(head(loan_data, 5))
cat("\nStructure of the dataset:\n")
str(loan_data)

# 2. Data Cleaning and Transformation
# Convert Loan_Status to factor
loan_data[, Loan_Status := factor(Loan_Status, levels = c("Y", "N"))]
# Impute missing LoanAmount with median by Education
loan_data[, LoanAmount := ifelse(is.na(LoanAmount), median(LoanAmount, na.rm = TRUE), LoanAmount), by = Education]
# Convert categorical variables to factors
loan_data[, Gender := factor(Gender)]
loan_data[, Married := factor(Married)]
loan_data[, Education := factor(Education)]
loan_data[, Self_Employed := factor(Self_Employed)]
loan_data[, Credit_History := factor(Credit_History, levels = c(0, 1))]

# 3. Outlier Detection and Visualization (Before Treatment)
# Boxplot for ApplicantIncome to visualize outliers
p1 <- ggplot(loan_data, aes(x = Loan_Status, y = ApplicantIncome, fill = Loan_Status)) +
  geom_boxplot() +
  labs(title = "Applicant Income by Loan Status (Before Outlier Treatment)", x = "Loan Status", y = "Applicant Income") +
  theme_minimal()
print(p1)
ggsave("income_outliers_before.png", p1)

# Identify outliers in ApplicantIncome using IQR
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  return(x < lower_bound | x > upper_bound)
}

outliers_income <- loan_data[, detect_outliers(ApplicantIncome)]
cat("\nNumber of outliers in ApplicantIncome:", sum(outliers_income, na.rm = TRUE), "\n")

# 4. Handling Outliers (Cap them at upper and lower bounds)
loan_data[, ApplicantIncome := ifelse(ApplicantIncome > quantile(ApplicantIncome, 0.75, na.rm = TRUE) + 1.5 * IQR(ApplicantIncome, na.rm = TRUE),
                                      quantile(ApplicantIncome, 0.75, na.rm = TRUE) + 1.5 * IQR(ApplicantIncome, na.rm = TRUE),
                                      ifelse(ApplicantIncome < quantile(ApplicantIncome, 0.25, na.rm = TRUE) - 1.5 * IQR(ApplicantIncome, na.rm = TRUE),
                                             quantile(ApplicantIncome, 0.25, na.rm = TRUE) - 1.5 * IQR(ApplicantIncome, na.rm = TRUE),
                                             ApplicantIncome))]

# 5. Visualization After Outlier Treatment
p2 <- ggplot(loan_data, aes(x = Loan_Status, y = ApplicantIncome, fill = Loan_Status)) +
  geom_boxplot() +
  labs(title = "Applicant Income by Loan Status (After Outlier Treatment)", x = "Loan Status", y = "Applicant Income") +
  theme_minimal()
print(p2)
ggsave("income_outliers_after.png", p2)

# 6. Exploratory Data Analysis (EDA)
# Approval rate by Credit_History
approval_rate <- loan_data[, .(Approval_Rate = mean(Loan_Status == "Y", na.rm = TRUE)), by = Credit_History]
cat("\nApproval Rate by Credit History:\n")
print(approval_rate)
cat("Interpretation: Applicants with a credit history (1) have a higher approval rate.\n")

# Scatterplot of ApplicantIncome vs LoanAmount
p3 <- ggplot(loan_data, aes(x = ApplicantIncome, y = LoanAmount)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Applicant Income vs Loan Amount", x = "Applicant Income", y = "Loan Amount") +
  theme_minimal()
print(p3)
ggsave("income_vs_loanamount.png", p3)

# Correlation
cor_income_loan <- cor(loan_data$ApplicantIncome, loan_data$LoanAmount, use = "complete.obs")
cat("\nCorrelation between ApplicantIncome and LoanAmount:", cor_income_loan, "\n")

# 7. Predictive Analysis (Logistic Regression)
# Prepare data for modeling
model_data <- loan_data[, .(Loan_Status, Gender, Married, ApplicantIncome, Credit_History)]
model_data <- na.omit(model_data)

# Split into training and testing sets (80% train, 20% test)
train_index <- createDataPartition(model_data$Loan_Status, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Train logistic regression model
model <- glm(Loan_Status ~ Gender + Married + ApplicantIncome + Credit_History, 
             data = train_data, family = "binomial")

# Predict on test data
predictions <- predict(model, newdata = test_data, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "Y", "N")

# Evaluate model
conf_matrix <- table(Predicted = predicted_classes, Actual = test_data$Loan_Status)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nLogistic Regression Model Evaluation:\n")
cat("Confusion Matrix:\n")
print(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Predict for the two individuals
new_data <- data.table(
  Gender = c("Female", "Male"),
  Married = c("Yes", "Divorced"),
  ApplicantIncome = c(1500, 2800),
  Credit_History = factor(c(1, 0), levels = c(0, 1))
)
predictions_new <- predict(model, newdata = new_data, type = "response")
predicted_status <- ifelse(predictions_new > 0.5, "Y", "N")
cat("\nPredictions for New Applicants:\n")
print(data.table(new_data, Predicted_Loan_Status = predicted_status))