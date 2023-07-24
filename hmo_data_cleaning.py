
''' DATA ANALYSIS AND CLEANING
In this section, we will figure out the data structure, it's attributes, the type of variables in the columns, how they look like statistically (mean, mode, median, etc.), if they have any irregularities or null values, and fix those null values.'''


#First, we have to import the libraries that we will be using for our initial
#reading of data, cleaning the data, and simple analysis.
library(tidyverse)

#We use the read_csv function to get the data and store it in the HMO_data variable.
HMO_data <- read_csv('https://intro-datascience.s3.us-east-2.amazonaws.com/HMO_data.csv')


#We will now have a look at the data to see the structures, identify any missing values.
dim(HMO_data)

#This dataset has 7582 rows (observations) and 14 columns (attributes).

#We will have a look at the data.
head(HMO_data)

#Let's look at the types of each variable.
str(HMO_data)

#There are 6 attributes that are of numerical type and 8 columns that are of type character (string).

#Next, we gather some statistical analysis of the variables.
summary(HMO_data)

#As we can see, each numerical variable has a mean, median, etc. Also, in this analysis, we see that bmi has 78 null values while hypertension has 80 null values.

#Let's see if the string columns have any null values.
nrow(HMO_data[is.na(HMO_data$smoker),])
nrow(HMO_data[is.na(HMO_data$location),])
nrow(HMO_data[is.na(HMO_data$location_type),])
nrow(HMO_data[is.na(HMO_data$education_level),])
nrow(HMO_data[is.na(HMO_data$yearly_physical),])
nrow(HMO_data[is.na(HMO_data$exercise),])
nrow(HMO_data[is.na(HMO_data$married),])
nrow(HMO_data[is.na(HMO_data$gender),])

#We see that there are no null values in any of the other columns.


#Let's remove the NA values from bmi and hypertension.
#We will use the na_interpolation() function to achieve this.
#It is in the imputeTS package.

install.packages('imputeTS')
library(imputeTS)

#Before
print('Number of NA\'s Before')
nrow(HMO_data[is.na(HMO_data$bmi),])
nrow(HMO_data[is.na(HMO_data$hypertension),])

#Using na_interpolation() to remove null values from the two columns.
HMO_data$bmi <- na_interpolation(HMO_data$bmi)
HMO_data$hypertension <- na_interpolation(HMO_data$hypertension)

#After
print('Number of NA\'s After')
nrow(HMO_data[is.na(HMO_data$bmi),])
nrow(HMO_data[is.na(HMO_data$hypertension),])

#In this section we will remove the possible outliers. Assuming that the bottom and top 0.5% data contained in the distribution curve contains possible outliers

lower_bound <- quantile(HMO_data$cost, 0.005)
upper_bound <- quantile(HMO_data$cost, 0.995)
lower_bound #0.5th percentile of all data
upper_bound #99.5th percentile of all data
outliers <- which(HMO_data$cost < lower_bound | HMO_data$cost > upper_bound)

nrow(HMO_data[outliers,]) #number of outliers

HMO_data_new <- HMO_data[-outliers,]

#We now look at the summary statistics of the new dataset.

summary(HMO_data_new)

