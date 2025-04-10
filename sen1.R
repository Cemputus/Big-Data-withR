library(tidyselect)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(readxl)
library(readr)
library(tidyr)
library(zoo)
library(e1071)
# Load required package
library(data.table)
library(writexl)

df <- read_excel("D:/YEAR2SEM2/DATA VISUALIZATION AND STORYTELLING/DATA VIZ/Mag6PlusEarthquakes_1900-2013.xlsx")
view(df)
dim(df)
str(df)
head(df)
summary(df)

#missing values
sum(is.na(df))
sum(is.na(df$depth))
sum(is.na(df$mag))
sum(is.na(df$magType))
sum(is.na(df$...4))
sum(is.na(df$nst))
sum(is.na(df$gap))
sum(is.na(df$dmin))
sum(is.na(df$rms))
sum(is.na(df$net))
sum(is.na(df$id))
sum(is.na(df$updated))
sum(is.na(df$place))

#handling missing values ----
#1.dropping ----
df1=na.omit(df) #removes all rows that have missing values
df2=select(df,-...4) #removes a column with missing values

#2.filling
df3=na.locf(df2$mag,na.rm = FALSE) #only shows dataframe of one column

#3.imputing mean and median for continuous and mode for categorical
#visualization of continuous data using histogram and boxplot
ggplot(df2,aes(x=depth))+
  geom_histogram(bins=20,fill='beige',alpha=0.6,color='black')+
  theme_minimal()+
  ggtitle('Analysing depth')+
  xlab('Depth')+
  ylab('Count')

ggplot(df2,aes(x=mag))+
  geom_histogram(bins=20,fill='beige',alpha=0.6,color='black')+
  theme_minimal()+
  ggtitle('Analysing magnitude')+
  xlab('Magnitude')+
  ylab('Count')

ggplot(df2,aes(y=rms))+
  geom_boxplot(fill='blue',alpha=0.6,outlier.color ='red',outlier.shape =16)+
  theme_minimal()+
  ggtitle('Analsying rms ')+
  xlab('RMS')

ggplot(df2,aes(y=depth))+
  geom_boxplot(fill='blue',alpha=0.6,outlier.color ='red',outlier.shape =16)+
  theme_minimal()+
  ggtitle('Analsying depth')+
  xlab('depth')

ggplot(df2,aes(y=nst))+
  geom_boxplot(fill='blue',alpha=0.6,outlier.color ='red',outlier.shape =16)+
  theme_minimal()+
  ggtitle('Analsying nst')+
  xlab('nst')


#Visualizing categorical data using a bar plot
ggplot(df2,aes(x=magType,fill = magType))+
  geom_bar()+
  theme_minimal()+
  ggtitle('Analysing magnitude type')

df2$depth[is.na(df2$depth)] <- mean(df2$depth, na.rm = TRUE)
sum(is.na(df2$depth))
df2$rms[is.na(df2$rms)] <- median(df2$rms, na.rm = TRUE)
sum(is.na(df2$rms))
df2$nst[is.na(df2$nst)] <- median(df2$nst, na.rm = TRUE)
sum(is.na(df2$nst))

df2$place[is.na(df2$place)] <- names(which.max(table(df2$place)))
sum(is.na(df2$place))

#calculating skewness positive=right skew, negative=left skew
skewness(df2$latitude)
skewness(df2$longitude)
skewness(df2$depth)
skewness(df2$rms)

##OUTLIERS
#checking for them using iqr method or visualize using boxplot
q1=quantile(df2$depth,0.25)
q3=quantile(df2$depth,0.75)
iqr=q3-q1
lb=q1-2.5*iqr
ub=q3+2.5*iqr
df4=df2[(df2$depth<lb)|(df2$depth>ub),] #df4 are depth outliers
view(df4)

#removing outliers to remain with values only within the iqr
df5=df2[df2$depth<=ub,]
view(df5)

##method2
q1=quantile(df2$rms,0.25)
q3=quantile(df2$rms,0.75)
iqr=q3-q1
lb=q1-2.5*iqr
ub=q3+2.5*iqr
#replacing with the upper and lower bounds
df2$rms[df2$rms<lb]=lb
df2$rms[df2$rms>ub]=ub
ggplot(df2,aes(y=rms))+
  geom_boxplot(fill='blue',alpha=0.6,outlier.color ='red',outlier.shape =16)+
  theme_minimal()+
  ggtitle('Analsying rms ')+
  xlab('RMS')
view(df2)

q1=quantile(df2$nst,0.25)
q3=quantile(df2$nst,0.75)
iqr=q3-q1
lb=q1-2.5*iqr
ub=q3+2.5*iqr
#imputing with mean or median
df2$nst[df2$nst < lb | df2$nst > ub] <- median(df2$nst, na.rm = TRUE)  # Use median for skewed data
view(df2)

##bivariate analysis  for 2 categorical
ggplot(df2, aes(x = magType, fill = net)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  ggtitle("Relationship between magtype and network")

##for 2 continuous
ggplot(df2, aes(x = depth, y = nst)) +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal() +
  ggtitle("Scatter Plot of depth vs nst")

cor(df2$depth, df2$nst, method = "pearson")
cor(df2$depth, df2$nst, method = "spearman")

##continuous and categorical
ggplot(df, aes(x = net, y = depth, fill = net)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Distribution of depth by net")

##MULTIVARIATE ANALYSIS
install.packages('GGally')
library(GGally)
#using a pairplot
ggpairs(df2[, c("net", "depth", "nst")])
#using a correlation matrix
cor_matrix <- cor(df2[, c("rms", "depth", "nst")], use = "complete.obs")
print(cor_matrix)


####filtering and answering questions
##where net=atlas, place=vanuatu and magtype=mww
networktable=table(df2$net) #table to get mode for net
view(networktable)
#to filter
networks=filter(df2,df2$net=='atlas')
view(networks)
mag63=filter(df2,df2$mag==6.3)
view(mag63)
magmww=filter(df2,df2$magType=='Mww')
view(magmww)
placequake=filter(df2,df2$place=='Vanuatu')
view(placequake)

#####get highest and lowest magnitudes with their depth and places
max(df2$mag, na.rm = TRUE)
max(df2$depth)
highestdepth <- df2[max(df2$depth), c("depth","place")]
view(highestdepth)
lowestdepth <- df2[min(df2$depth), c("depth","place")]
view(lowestdepth)
#remember to use both which.max and max
higherdepth <- df2[which.max(df2$depth), c("depth","place")]
view(higherdepth)

###########creating a new column is mutating
df8 <- mutate(df2,mag_depth=df2$mag*df2$depth)
view(df8)
