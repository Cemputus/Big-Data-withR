#### SUPERVISED LEARNING: REGRESSION ####

# ---------------------------
# 1. Set working directory
# ---------------------------
setwd("~/Data_Science/year2/semester 2/Big Data -R/Practicals in Rstudio and Using Sparklyr-20250225")

# ---------------------------
# 2. Install and load packages
# ---------------------------
packages <- c("corrplot", "nortest", "ISLR", "Hmisc", "ModelMetrics", "lmtest", "car",
              "olsrr", "moments", "bestNormalize", "magrittr", "rpart.plot", "tidyverse",
              "rpart", "caTools", "ggplot2", "caret", "Boruta", "cvms", "dplyr", "MASS",
              "ggcorrplot", "readxl", "data.table")

install_if_missing <- function(p) {
  if (!require(p, character.only = TRUE)) {
    install.packages(p, dependencies = TRUE)
    library(p, character.only = TRUE)
  }
}
invisible(lapply(packages, install_if_missing))

# ---------------------------
# 3. Load Data
# ---------------------------
Data <- read_xlsx("Bike_Sales_New.xlsx")
View(Data)

# ---------------------------
# 4. Convert categorical columns to factors
# ---------------------------
cols_to_factor <- c("Date", "Month", "Age_Group", "Customer_Gender", "Country", "State",
                    "Sub_Category", "Product_Category", "Product")
Data[cols_to_factor] <- lapply(Data[cols_to_factor], as.factor)

# ---------------------------
# 5. Subset variables
# ---------------------------
Cat <- Data[c("Date", "Month", "Age_Group", "Customer_Gender", "Sub_Category", 
              "Product", "Product_Category", "Country", "State")]

Cont <- Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", 
               "Unit_Price", "Profit", "Cost")]

# ---------------------------
# 6. ANOVA tests for categorical vs Revenue
# ---------------------------
anova_vars <- c("Date", "Month", "Age_Group", "Customer_Gender", "Country", 
                "State", "Product_Category", "Product", "Sub_Category")

for (var in anova_vars) {
  cat("\n\n--- ANOVA: Revenue ~", var, "---\n")
  aov_result <- aov(as.formula(paste("Revenue ~", var)), data = Data)
  print(summary(aov_result))
  print(TukeyHSD(aov_result))
}

# ---------------------------
# 7. Pearson Correlation for continuous vs Revenue
# ---------------------------
cont_vars <- names(Cont)
for (var in cont_vars) {
  cat("\n\n--- Correlation: Revenue vs", var, "---\n")
  print(cor.test(Data$Revenue, Data[[var]], method = "pearson", use = "complete.obs"))
}

# Correlation matrix
Cont2 <- Data[c("Day", "Year", "Customer_Age", "Order_Quantity", "Unit_Cost", 
                "Unit_Price", "Profit", "Cost", "Revenue")]
rcorr(as.matrix(Cont2))
corrplot(cor(Cont2))
ggcorrplot(cor(Cont2))

# ---------------------------
# 8. Subset Final Data for Model
# ---------------------------
Data_final <- Data[c("Unit_Cost", "Unit_Price", "Profit", "Cost", 
                     "Age_Group", "Customer_Gender", "Sub_Category", 
                     "Product", "Product_Category", "Revenue")]

# ---------------------------
# 9. Split Data into Train and Test
# ---------------------------
set.seed(123)
sample <- sample.split(Data_final$Revenue, SplitRatio = 0.7)
train_set <- subset(Data_final, sample == TRUE)
test_set <- subset(Data_final, sample == FALSE)

# ---------------------------
# 10. SIMPLE LINEAR REGRESSION
# ---------------------------
# Visualize relationship
plot(train_set$Unit_Cost, train_set$Revenue, main = "Unit_Cost vs Revenue")

# Fit model
linear_model <- lm(Revenue ~ Unit_Cost, data = train_set)
summary(linear_model)

# Plot with regression line
ggplot(data = train_set, aes(x = Unit_Cost, y = Revenue)) + 
  geom_point() + 
  stat_smooth(method = "lm", col = "blue") +
  ggtitle("Simple Linear Regression: Unit_Cost vs Revenue")

# ---------------------------
# 11. MULTIPLE LINEAR REGRESSION
# ---------------------------
multi_model <- lm(Revenue ~ ., data = train_set)
summary(multi_model)

# Predict on test set
predictions <- predict(multi_model, newdata = test_set)

# Evaluate performance
RMSE <- sqrt(mean((test_set$Revenue - predictions)^2))
cat("RMSE on test set: ", RMSE)

# Optionally visualize predicted vs actual
ggplot(data = test_set, aes(x = Revenue, y = predictions)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Predicted vs Actual Revenue", x = "Actual Revenue", y = "Predicted Revenue")

