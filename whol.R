
# setting the working directory
setwd("C:/Users/STUDENTS/Documents/EMMANUEL NSUBUGA")


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



## A. Transform the dataset to by handling any anomalies [10 Marks].

## B. Save the transformed dataset both as an R dataframe and as an excel file labelled "Q12". [5 Marks]

## C. Explore the above dataframe and highlight key descriptive statistics about only four of the variables. [15 Marks]

## D. What is the relationship between two continuous variables? (Back this up with justifiable statistical results) [15 Marks]

## E. What is the relationship between two categorical and one continuous variable?  (Back this up with justifiable statistical results) [15 Marks]


### Answers

# A - using wholesale dataset ----

# reading the dataset ----

wholesale <- read_excel("Wholesale_customers_data.xls")

View(wholesale)

# checking for the columns of our datasets -----
names(wholesale)


## According to the structure of our dataset, all the columns are numerical

### Understanding the structure of my data set ----
## Shape of the data-----
dim(wholesale) 
#Rows -------> 440  columns --------> 8 ------


# structure of the dataset ----
str(wholesale)
glimpse(wholesale)


# visualizing our dataset to check for the distribution ----
# Function to plot histograms for numeric columns ----
Histogram <- function(wholesale) {
  numeric_cols <- names(wholesale)[sapply(wholesale, is.numeric)] 
  
  par(mfrow = c(2, 2)) 
  
  for (col in numeric_cols) {
    hist(wholesale[[col]], main = paste("Histogram of", col), 
         xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  
}

# Calling the function ----
Histogram(wholesale)

# Accoding to data distribution, we can see that the data is not normally distributed
# This implies that incase of any missing values, we cannot use mean to impute

## checking for missing values ----

missing_values <- sapply(wholesale, function(x) sum(is.na(x)))
missing_values 

# Basing on the results, we can see that there are no missing values 



# checking for outliers using boxplots

outlier_boxplot <- function(wholesale) {
  numeric_cols <- names(wholesale)[sapply(wholesale, is.numeric)] 
  
  par(mfrow = c(2, 2)) 
  
  for (col in numeric_cols) {
    boxplot(wholesale[[col]], main = paste("Boxplot of", col), 
            xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  
}

# Calling the function ----
outlier_boxplot(wholesale)


## Basing on the output, we notice that its only Channel and Region withon outliers
# this implies we can create  new dataframe use when removing outliers since Channel and Region are clean

## Dealing with the outliers ----

# Removing outliers 

remove_outliers <- function(wholesale, cols) {
  for (col in cols) {
    q1 <- quantile(wholesale[[col]], 0.25, na.rm = TRUE)
    q3 <- quantile(wholesale[[col]], 0.75, na.rm = TRUE)
    IQR <- q3 - q1
    lower_bound <- q1 - 1.5 * IQR
    upper_bound <- q3 + 1.5 * IQR
    wholesale <- wholesale[wholesale[[col]] >= lower_bound & wholesale[[col]] <= upper_bound, ]
  }
  return(wholesale)
}

# Apply to numeric columns
numeric_cols <- names(wholesale)[sapply(wholesale, is.numeric)]
wholesale_cleaned <- remove_outliers(wholesale, numeric_cols)


# ploting again to check for outliers

outlier_boxplot <- function(wholesale_cleaned) {
  numeric_cols <- names(wholesale_cleaned)[sapply(wholesale_cleaned, is.numeric)] 
  
  par(mfrow = c(2, 2)) 
  
  for (col in numeric_cols) {
    boxplot(wholesale_cleaned[[col]], main = paste("Boxplot of", col), 
            xlab = col, col = "skyblue", border = "black")
  }
  
  par(mfrow = c(1, 1))  
}

# Calling the function ----
outlier_boxplot(wholesale_cleaned)








# B. Save Transformed Dataset -----
# Save as R Dataframe
save(wholesale_cleaned, file = "Q12.RData")

# Save as Excel File
write_xlsx(wholesale_cleaned, "Q12.xlsx")


# C. Descriptive Statistics for 4 Variables ------
selected_vars <- wholesale_cleaned %>% 
  select(Fresh, Milk, Grocery, Detergents_Paper)

# Summary of descriptive statistics ----

describe(selected_vars$Fresh)
### vars   n    mean      sd median trimmed     mad min   max range skew kurtosis     se
### X1    1 318 9718.17 8200.12 7801.5 8676.46 7504.92   3 37036 37033 1.05     0.63 459.84


describe(selected_vars$Milk)
### vars   n    mean     sd median trimmed    mad min   max range skew kurtosis     se
## X1    1 318 3989.18 3260.6   3045 3560.89 2909.6  55 15729 15674 1.01     0.25 182.85

describe(selected_vars$Grocery)
# vars   n    mean      sd median trimmed     mad min   max range skew kurtosis     se
# X1    1 318 5563.68 4623.07 3815.5  4952.5 3529.33   3 20292 20289 1.07     0.34 259.25

describe(selected_vars$Detergents_Paper)
# vars   n   mean      sd median trimmed    mad min  max range skew kurtosis     se
# X1    1 318 1840.6 2206.61    675 1450.23 861.39   3 8077  8074 1.24     0.31 123.74


descriptive_summary <- describe(selected_vars) %>% 
  view()




## summary of the selected variables

summary(wholesale_cleaned$Fresh)
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 3    3044    7802    9718   14085   37036 



summary(wholesale_cleaned$Milk)

### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 55    1330    3045    3989    6188   15729 


summary(wholesale_cleaned$Grocery)

### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  3    2002    3816    5564    8436   20292 


summary(wholesale_cleaned$Detergents_Paper)
### Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
### 3.0   228.8   675.0  1840.6  3273.0  8077.0 


summary(selected_vars)

## Fresh            Milk          Grocery      Detergents_Paper
## Min.   :    3   Min.   :   55   Min.   :    3   Min.   :   3.0  
## 1st Qu.: 3044   1st Qu.: 1330   1st Qu.: 2002   1st Qu.: 228.8  
## Median : 7802   Median : 3045   Median : 3816   Median : 675.0  
## Mean   : 9718   Mean   : 3989   Mean   : 5564   Mean   :1840.6  
## 3rd Qu.:14085   3rd Qu.: 6188   3rd Qu.: 8436   3rd Qu.:3273.0  
## Max.   :37036   Max.   :15729   Max.   :20292   Max.   :8077.0  





# D. Relationship between Two Continuous Variables (Milk vs Grocery) -----
cor_test <- cor.test(wholesale_cleaned$Milk, wholesale_cleaned$Grocery)
print(cor_test)  # 0.7368725- this implies that the relationship between Milk and Grocery is high

# Visualization -----
ggplot(wholesale_cleaned, aes(x = Milk, y = Grocery)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red") +
  ggtitle("Relationship between Milk and Grocery")  ## According to the output, the line of bestfit is has a strong positive correlation



# E. Relationship between Two Categorical and One Continuous Variable ----

# Based on the dataset, we can see that Region and channel ore ordinal data which can be converted to categorical 
# Create a categorical variable based on Region  and Channel
wholesale_cleaned$Channel <- as.factor(wholesale_cleaned$Channel)
wholesale_cleaned$Region <- as.factor(wholesale_cleaned$Region)

# Boxplot to visualize relationship
ggplot(wholesale_cleaned, aes(x = Channel, y = Milk, fill = Region)) +
  geom_boxplot() +
  ggtitle("Milk Consumption by Channel and Region")

# ANOVA Test
anova_test <- aov(Milk ~ Channel * Region, data = wholesale_cleaned)
summary(anova_test)








  