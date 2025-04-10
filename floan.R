
# Libraries required
library(data.table)
library(tidyverse)
library(sparklyr)
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


# Install and configure Spark (run once if not already set up)
# spark_install(version = "3.4.0")  # Adjust version as needed
sc <- spark_connect(master = "local")  # Connect to local Spark instance

# Set seed for reproducibility
set.seed(123)

# 1. Data Reading
loan_data <- fread("loan_approval.csv", stringsAsFactors = FALSE)
cat("First 5 rows of Loan Approval Dataset:\n")
print(head(loan_data, 5))
cat("\nStructure of the dataset:\n")
str(loan_data)

# 2. Data Cleaning and Transformation
setDT(loan_data)
loan_data <- loan_data %>%
  mutate(
    Loan_Status = factor(Loan_Status, levels = c("Y", "N")),
    Gender = factor(Gender),
    Married = factor(Married),
    Education = factor(Education),
    Self_Employed = factor(Self_Employed),
    Credit_History = factor(Credit_History, levels = c(0, 1)),
    LoanAmount = if_else(is.na(LoanAmount), 
                         median(LoanAmount, na.rm = TRUE), 
                         LoanAmount, 
                         missing = median(LoanAmount, na.rm = TRUE)),
    ApplicantIncome = as.numeric(ApplicantIncome),
    CoapplicantIncome = as.numeric(CoapplicantIncome)
  )

# 3. Outlier Detection and Visualization (Before Treatment)
p1 <- loan_data %>%
  ggplot(aes(x = Loan_Status, y = ApplicantIncome, fill = Loan_Status)) +
  geom_boxplot() +
  labs(title = "Applicant Income by Loan Status (Before Outlier Treatment)", 
       x = "Loan Status", y = "Applicant Income") +
  theme_minimal()
print(p1)
ggsave("income_outliers_before.png", p1)

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

# 4. Handling Outliers
loan_data <- loan_data %>%
  mutate(
    ApplicantIncome = case_when(
      ApplicantIncome > quantile(ApplicantIncome, 0.75, na.rm = TRUE) + 1.5 * IQR(ApplicantIncome, na.rm = TRUE) ~ 
        quantile(ApplicantIncome, 0.75, na.rm = TRUE) + 1.5 * IQR(ApplicantIncome, na.rm = TRUE),
      ApplicantIncome < quantile(ApplicantIncome, 0.25, na.rm = TRUE) - 1.5 * IQR(ApplicantIncome, na.rm = TRUE) ~ 
        quantile(ApplicantIncome, 0.25, na.rm = TRUE) - 1.5 * IQR(ApplicantIncome, na.rm = TRUE),
      TRUE ~ ApplicantIncome
    )
  )

# 5. Visualization After Outlier Treatment
p2 <- loan_data %>%
  ggplot(aes(x = Loan_Status, y = ApplicantIncome, fill = Loan_Status)) +
  geom_boxplot() +
  labs(title = "Applicant Income by Loan Status (After Outlier Treatment)", 
       x = "Loan Status", y = "Applicant Income") +
  theme_minimal()
print(p2)
ggsave("income_outliers_after.png", p2)

# 6. Exploratory Data Analysis (EDA)
approval_rate <- loan_data %>%
  group_by(Credit_History) %>%
  summarise(Approval_Rate = mean(Loan_Status == "Y", na.rm = TRUE))
cat("\nApproval Rate by Credit History:\n")
print(approval_rate)
cat("Interpretation: Applicants with a credit history (1) have a higher approval rate.\n")

p3 <- loan_data %>%
  ggplot(aes(x = ApplicantIncome, y = LoanAmount)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Applicant Income vs Loan Amount", 
       x = "Applicant Income", y = "Loan Amount") +
  theme_minimal()
print(p3)
ggsave("income_vs_loanamount.png", p3)

cor_income_loan <- loan_data %>%
  summarise(cor = cor(ApplicantIncome, LoanAmount, use = "complete.obs")) %>%
  pull(cor)
cat("\nCorrelation between ApplicantIncome and LoanAmount:", cor_income_loan, "\n")

# 7. Predictive Analysis (Logistic Regression with sparklyr)
# Load data into Spark
spark_loan <- copy_to(sc, loan_data, "spark_loan", overwrite = TRUE)

# Prepare data for modeling
model_data <- spark_loan %>%
  select(Loan_Status, Gender, Married, ApplicantIncome, Credit_History) %>%
  na.omit() %>%
  mutate(Loan_Status = ifelse(Loan_Status == "Y", 1, 0))  # Convert to 0/1 for Spark ML

# Split into training and testing sets (80% train, 20% test)
splits <- model_data %>%
  sdf_random_split(training = 0.8, test = 0.2, seed = 123)
train_data <- splits$training
test_data <- splits$test

# Train logistic regression model with sparklyr
model <- ml_logistic_regression(train_data, Loan_Status ~ Gender + Married + ApplicantIncome + Credit_History)

# Predict on test data
predictions <- ml_predict(model, test_data) %>%
  collect()

# Evaluate model
conf_matrix <- table(Predicted = ifelse(predictions$prediction > 0.5, 1, 0), 
                     Actual = predictions$Loan_Status)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("\nSpark Logistic Regression Model Evaluation:\n")
cat("Confusion Matrix:\n")
print(conf_matrix)
cat("Accuracy:", accuracy, "\n")

# Predict for the two individuals
new_data <- tibble(
  Gender = c("Female", "Male"),
  Married = c("Yes", "Divorced"),
  ApplicantIncome = c(1500, 2800),
  Credit_History = c("1", "0")
) %>%
  copy_to(sc, ., "new_data", overwrite = TRUE)

predictions_new <- ml_predict(model, new_data) %>%
  collect()
predicted_status <- ifelse(predictions_new$prediction > 0.5, "Y", "N")
cat("\nPredictions for New Applicants:\n")
print(tibble(Gender = c("Female", "Male"), 
             Married = c("Yes", "Divorced"), 
             ApplicantIncome = c(1500, 2800), 
             Credit_History = c("1", "0"), 
             Predicted_Loan_Status = predicted_status))

# Disconnect from Spark
spark_disconnect(sc)