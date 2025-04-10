## -------------------------
## Q12 ANALYSIS: BIKE SALES USING DATA.TABLE
## -------------------------

## Setting the working directory -----
setwd("C:/Users/STUDENTS/Documents/")

## Load required libraries -----
install.packages("data.table")
install.packages("psych")
library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(tidyselect)
library(readxl)
library(readr)
library(psych)
library(writexl)

## A. DATA TRANSFORMATION AND CLEANING ------------------------

## Read the Excel file -----
df <- as.data.table(read_excel("Bike_Sales_New.xlsx"))
view(df)

## Explore structure and dimension -----
names(df)
dim(df)
str(df)
glimpse(df)
class(df)

## Check for duplicates and remove them -----
sum(duplicated(df))
df <- df[!duplicated(df)]

## Check for missing values -----
missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values

## Impute missing values -----
impute_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
impute_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
impute_mode <- function(x) {
  mode_value <- names(sort(table(x), decreasing = TRUE))[1]
  replace(x, is.na(x), mode_value)
}

df$Month <- impute_mean(df$Month)
df$Country <- impute_mode(df$Country)

## Check for outliers using boxplots -----
outlier_boxplot <- function(df){
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  par(mfrow = c(2, 2))
  for (col in numeric_cols) {
    boxplot(df[[col]], main = paste("Boxplot of", col), xlab = col, col = "skyblue")
  }
  par(mfrow = c(1, 1))
}
outlier_boxplot(df)

## Remove outliers -----
removing_outliers <- function(df, cols){
  for (col in cols){
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- q3 - q1
    lower <- q1 - 1.5 * IQR
    upper <- q3 + 1.5 * IQR
    df <- df[df[[col]] >= lower & df[[col]] <= upper, ]
  }
  return(df)
}

numeric_cols <- names(df)[sapply(df, is.numeric)]
df_cleaned <- removing_outliers(df, numeric_cols)

## B. SAVE TRANSFORMED DATASET ------------------------
save(df_cleaned, file = "Q12.RData")
write_xlsx(df_cleaned, "Q12.xlsx")

## C. DESCRIPTIVE STATISTICS ------------------------

selected_vars <- df_cleaned[, .(Date, Month, Customer_Age, Customer_Gender, Age_Group, 
                                State, Sub_Category, Country, Cost, Unit_Cost, Unit_Price, Profit, Revenue)]

describe(selected_vars)
summary(selected_vars)

## D. RELATIONSHIP BETWEEN TWO CONTINUOUS VARIABLES ------------------------

# Correlation Test between Cost and Profit
cor_test <- cor.test(df_cleaned$Cost, df_cleaned$Profit)
print(cor_test)

# Visualization
ggplot(df_cleaned, aes(x = Cost , y = Profit)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Relationship between Cost and Profit")

## E. RELATIONSHIP BETWEEN TWO CATEGORICAL + ONE CONTINUOUS VARIABLE ------------------------

# Boxplot
ggplot(df_cleaned, aes(x = Product, y = Customer_Age, fill = Age_Group)) +
  geom_boxplot() +
  ggtitle("Product Purchase by Customer Age and Age Group")

# ANOVA Test
anova_test <- aov(Profit ~ Product * Country, data = df_cleaned)
summary(anova_test)

## F. SUPERVISED LEARNING ------------------------

## Simple Linear Regression (Profit ~ Revenue) -----
simple_model <- lm(Profit ~ Revenue, data = df_cleaned)
summary(simple_model)

# Visualization
ggplot(df_cleaned, aes(x = Revenue, y = Profit)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  ggtitle("Simple Linear Regression: Profit vs Revenue")

## Multiple Linear Regression -----
multi_model <- lm(Profit ~ Revenue + Unit_Cost + Customer_Age + Month, data = df_cleaned)
summary(multi_model)

# Diagnostics plot
par(mfrow = c(2, 2))
plot(multi_model)
par(mfrow = c(1, 1))

## -----------------------
## END OF SCRIPT
## -----------------------
