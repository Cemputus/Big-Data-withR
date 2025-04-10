## setting the working directory -----
setwd("C:/Users/STUDENTS/Documents/")




## A. Transform the dataset to by handling any anomalies [10 Marks].

## B. Save the transformed dataset both as an R dataframe and as an excel file labelled "Q12". [5 Marks]

## C. Explore the above dataframe and highlight key descriptive statistics about only 9 of the variables. [15 Marks]

## D. What is the relationship between two continuous variables? (Back this up with justifiable statistical results) [15 Marks]

## E. What is the relationship between two categorical and one continuous variable?  (Back this up with justifiable statistical results) [15 Marks]


install.packages("psych")

## importing the necessary  libraries  -----
library(tidyr)
library(tidyverse)
library(ggplot2)
library(data.table)
library(tidyselect)
library(readxl)
library(readr)
library(psych)
library(writexl)


df <- read_excel("Bike_Sales_New.xlsx")
view(df)

##checking for the columns of our datasets ----
names(df)


## 
dim(df)

## structure of the data ----
str(df)
glimpse(df)

class(df)

sum(duplicated(df))



# visualizing our dataset to check for the distribution ----
# Function to plot histograms for numeric columns ----
Histogram <- function(df){
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  par(mfrow = c(2,2))
  
  for (col in numeric_cols){
    hist(df[[col]], main = paste("Distribution of ", col),
         xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1,1))
}


## calling the function -----
Histogram(df)


## checking for missing values -----
missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values 

## Approach 2
missing_values2 <- df %>% 
  summarise(numeric_missing = sum(is.na(.)), categorical_missing = sum(is.na(as.character(.))))

missing_values2


## Approach 3
missing_values3 <- df %>% summarise_all(funs(sum(is.na(.))))
missing_values3




## handling missing values ----

## Dropping ----
new_data <- na.omit(df)
new_data



class(df$Country)

hist(df$Month, col = 'blue', main = "Histogram for Month")

## imputing for missing values 
impute_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE)) ### Normalized numerical data

impute_median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE)) ## skewed numerical data

impute_mode <- function(x) {
  model_value <- as.numeric(names(table(sort(x), decreasing = TRUE)[1])) ### categorical data
  replace(x, is.na(x), model_value)
}



df$Month <- impute_mean(df$Month)

df$Country <- impute_mode(df$Country)



# checking for outliers using boxplots ----
outlier_boxplot <- function(df){
  numeric_cols <- names(df)[sapply(df, is.numeric)]
  
  par(mfrow = c(2, 2)) 

  
  for (col in numeric_cols) {
    boxplot(df[[col]], main = paste("Boxplot of", col), 
            xlab = col, col = "skyblue", border = "black")
    
  }
  par(mfrow = c(1, 1))
}

outlier_boxplot(df)


boxplot(df$Revenue, col = "blue", main = "Boxplot of Revenue")


## Dealing with outliers ----
# removing outliers ----

removing_outliers <- function(df, cols){
  for (col in cols){
    q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
    IQR <- q3 - q1
    lower_bound <- q1 - 1.5 * IQR
    upper_bound <- q3 + 1.5 * IQR
    df <- df[df[[col]] >=lower_bound & df[[col]]<= upper_bound, ]
  }
  return(df)
}


## applying to numeric columns ----
numeric_cols <- names(df)[sapply(df, is.numeric)]

df_cleaned <- removing_outliers(df, numeric_cols)


## plotting again to check for outliers
outlier_boxplot <- function(df_cleaned) {
  numeric_cols <- names(df_cleaned)[sapply(df_cleaned, is.numeric)] 
  
  par(mfrow = c(2, 2)) 
  
  for (col in numeric_cols) {
    boxplot(df_cleaned[[col]], main = paste("Boxplot of", col), 
            xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  
}

# Calling the function ----
outlier_boxplot(df_cleaned)


## save transformed dataset
##  save as R dataframe 
save(df_cleaned, file = "cen.RData")


## save as excel file
write_xlsx(df_cleaned, "cen.xlsx")


names(df)


selected_vars <- df_cleaned %>% 
  select(Date, Month, Customer_Age, Customer_Gender,Age_Group, State, Sub_Category, Country, Cost, Unit_Cost, Unit_Price, Profit, Revenue) %>% 
  view()

# descriptive statistics ----- using psych libraries
describe(selected_vars$Profit)

describe(selected_vars$Revenue)

descriptive_summary <- describe(selected_vars) %>% 
  view()


## summary of descriptive statistics -----
summary(selected_vars$Country)


summary(selected_vars)

names(df_cleaned)


# D. Relationship between Two Continuous Variables (Cost vs Profit) -----
cor_test <- cor.test(df_cleaned$Cost, df_cleaned$Profit)
print(cor_test)  # 0.8469134- this implies that the relationship between Cost vs Profit is very  strong

# Visualization -----
ggplot(df_cleaned, aes(x = Cost , y = Profit)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Relationship between Cost vs Profit")  ## According to the output, the line of bestfit is has a strong positive correlation

sum(duplicated(df_cleaned))

# E. Relationship between Two Categorical and One Continuous Variable ----

# Create a categorical variable based on  and 
#df_cleaned$Channel <- as.factor(df_cleaned)
#df_cleaned$Region <- as.factor(df_cleaned$)

# Boxplot to visualize relationship
ggplot(df_cleaned, aes(x = Product  , y = Customer_Age, fill = Age_Group)) +
  geom_boxplot() +
  ggtitle("Product purchased  by Customer_Age and Age_Group")

# ANOVA Test
anova_test <- aov(Profit ~ Product * Country, data = df_cleaned) 
summary(anova_test)
  




  














