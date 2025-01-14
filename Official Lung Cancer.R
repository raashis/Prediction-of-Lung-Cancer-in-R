################################################################
#   Course        : BC2406 Analytics I Group Project           #
#   Team          : 04                                        #
#   Seminar Group : 06                                        #
################################################################

#---------------------------------------------Setting CSV Path--------------------------------------------------------#
setwd("~/Desktop/Accountancy AY 2024-2025 [Year 3]/Accountancy Year 3 Semester 1/BC2406 Analytics I/Team Project/Main Project/Official Code")

#========================================================================================================================
# Install Packages
#========================================================================================================================
list.of.packages <- c("data.table", "RCurl", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

install.packages("caTools")
install.packages("MASS")
install.packages("car") #to check for multicollinearity, to see if any variables are dependent on another
install.packages("data.table")
install.packages("caret")
install.packages("randomForest")
install.packages("plotly")

#========================================================================================================================
# Load Library
#========================================================================================================================
library(data.table)
library(RCurl)
library(tidyverse)
library(MASS) 
library(car) 
library(caTools)
library(randomForest)
library(dplyr)
library(caret) 
library(knitr)
library(readr)
library(ggplot2)
library(scales)
library(corrplot)
library(plotly)
library(nnet)
library(rpart)


#========================================================================================================================
# #Data Cleaning: Lung Cancer Patient Records.csv
#========================================================================================================================

#-----------------------------------Importing Dataset & Getting an overview of the dataset-----------------------------------#
patients <- fread("cancerpatients.csv", stringsAsFactors = T)
patients %>% glimpse()
patients %>% head()
patients %>% summary()

View(patients)

#Checking if dataset is a data.table or data frame
is.data.table(patients) #Results shows TRUE, meaning the dataset is already a data table.

#---------------------------------------------DATA CLEANING ------------------------------------------------------------------#

#This data set contains information on patients with lung cancer. The records consist of a sample size of 1000 patients and the levels of each variables towards each independent patient.The data identified the listed variables as potential risk factors for lung cancer.

#Age, Gender, and Levels of (Air Pollution, Alcohol Use, Dust Allergy, Occupational Hazards, Genetic Risk, Chronic Lung Disease, Balanced Diet, Obesity, Smoking, Passive Smoker, Chest Pain, Fatigue, Weight Loss, Shortness of Breath, Wheezing, Swallowing Difficulty and Clubbing of Fingernails).

#For efficiency of the dataset to our predictive models, we decided to adjust the variables and combine several variables together.

#The Records also indicate the Level of Risk towards Lung Cancer (Range: Low < Medium < High) for each patient.

#------------Foundation Data Cleaning (Missing Values, Duplicates, Outliers, Adjusting Column Names)--------------#

#Duplicates Check: with reference as "Patient ID", the unique() function should work as it the function removes columns that are duplicated acorss all columns (Index and Patient id)

patients=unique(patients)
any(duplicated(patients))

#Result: Identify all records to be unique by identifying and removing duplicate rows, No duplicates 

#------------------Finding NAs or Missing Records-----------------------------------------#

#Missing Values: Checking for missing values in dataset

na_count <-sapply(patients, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)
na_count

#Result: No NULL or Missing Values

#-------------Removing Unwanted Columns (e.g. Index & Patient ID)-------------------------#

patients <- patients[, !c("index", "Patient Id"), with = FALSE]
View(patients)

# We decided to remove Index and Patient ID coloumn as they are only unique keys for each patient, and not independent variables factors which are essential for the predictive analysis of Lung Cancer. 


#------------------------Releveling Ordinal Categorical Variables-------------------------#

#Releveling Levels Column - (Low - High) 
levels(patients$Level)
factor(patients$Level)

#Releveling the Ordinal Variable of Level of Risk for Lung Cancer to (Low < Medium < High)
patients$Level <- factor(patients$Level, levels = c("Low", "Medium", "High"), ordered = TRUE)

levels(patients$Level)

#-------------------------Factorizing Categorical Variable--------------------------------#

#Factorizing Gender to Nominal if still want use. (Assuming 1 = Male, 2 = Female)
nominal_vars <- c("Gender")

patients[, (nominal_vars) := lapply(.SD, factor), .SDcols = nominal_vars]

levels(patients$Gender)

#-----------------------------------------

#Releveling Independent Variables (Risk Factors) Columns 

#Columns on scale of (1 Low - 7 High) 7 factors: Genetic Risk, Chronic Lung Disease, Balanced Diet, Obesity, Frequent Cold, Dry Cough, Snoring

ordinal_vars1_7 <- c("Genetic Risk", "chronic Lung Disease", "Balanced Diet", "Obesity", "Frequent Cold", "Dry Cough", "Snoring")

patients[, (ordinal_vars1_7) := lapply(.SD, function(x) {
  factor(x, levels = 1:7, ordered = TRUE)
}), .SDcols = ordinal_vars1_7]

#Columns on scale of (1 Low - 8 High) 9 factors: Air Pollution, Alcohol Consumption, Dust Allergy, Occupational Hazards, Smoking, Passive Smoker, Weight Loss, Wheezing and Swallowing Difficulty

ordinal_vars1_8 <- c("Air Pollution", "Alcohol use", "Dust Allergy", "OccuPational Hazards", "Smoking", "Passive Smoker", "Weight Loss", "Wheezing", "Swallowing Difficulty")

patients[, (ordinal_vars1_8) := lapply(.SD, function(x) {
  factor(x, levels = 1:8, ordered = TRUE)
}), .SDcols = ordinal_vars1_8]

#Columns on scale of (1 Low - 9 High) 5 factors: Chest Pain, Coughing of Blood, Fatigue, Shortness of Breath, Clubbing of Finger Nails

ordinal_vars1_9 <- c("Chest Pain", "Coughing of Blood", "Fatigue", "Shortness of Breath", "Clubbing of Finger Nails")

patients[, (ordinal_vars1_9) := lapply(.SD, function(x) {
  factor(x, levels = 1:9, ordered = TRUE)
}), .SDcols = ordinal_vars1_9]

#-----------------------------------Outliers Check ---------------------------------------#

#Outliers: Checking for Outliers in Age (Numeric)

summary(patients$Age)

# Identify outliers using the IQR method for Age
Q1 <- quantile(patients$Age, 0.25, na.rm = TRUE)  # 1st quartile
Q3 <- quantile(patients$Age, 0.75, na.rm = TRUE)  # 3rd quartile
IQR <- Q3 - Q1  # Interquartile range

# Define lower and upper bounds for outliers
lower.limit <- Q1 - 1.5 * IQR #lower limit = 1.875. Thus, any age below 1.875 is considered outliers.
upper.limit <- Q3 + 1.5 * IQR #upper limit = 70.875 y/o. Thus, any age above 70.875 y/o are outliers.

#Thus, records above the upper limit consist only of people aged 73 y/o. 

# Find the rows where Age is an outlier
outliers.age <- patients[Age < lower.limit | Age > upper.limit, ]

# Displaying the outliers
print(outliers.age)
View(outliers.age) #Patient Records which R presumes as outliers (73 y/o).
boxplot(patients$Age, main = "Boxplot of Age", horizontal = TRUE)

#R assumes Records with Age = 73, are considered outliers. However, we identify that the records are relevant as Age of 73 is only 2.125 y/o and a small increase above the upper limit. Making it insignificant.


#Furthermore, there are no significant outliers as the outlier (73) in Age, is considered a realistic age number in this present day.

#-----------------------------------------

#Checking and Confirming Datatypes of all updated columns (table)
str(patients)
patients

#------------------------------RENAMING Columns for Typo Errors---------------------------#

colnames(patients) <- c("Age", "Gender", "Air Pollution", "Alcohol Use", "Dust Allergy", "Occupational Hazards", "Genetic Risk", "Chronic Lung Disease", "Balanced Diet", "Obesity", "Smoking Frequency", "Passive Smoker Frequency", "Chest Pain", "Coughing of Blood", "Fatigue", "Weight Loss", "Shortness of Breath", "Wheezing", "Swallowing Difficulty", "Clubbing of Finger Nails", "Frequent Cold", "Dry Cough", "Snoring", "Risk Level")

View(patients)

#------------------------------Creating Age.Group Column---------------------------#

#Create the AgeGroup column based on intervals
patients$AgeGroup <- cut(
  patients$Age,
  breaks = c(0, 25, 35, 45, 55, 65, Inf),
  labels = c("0-25", "26-35", "36-45", "46-55", "56-65", "65+"),
  right = FALSE
)

#Remove the old Age column
patients$Age <- NULL

#Rename AgeGroup to Age
names(patients)[names(patients) == "AgeGroup"] <- "Age"

summary(patients) # Check to see whether variables are converted correctly
View(patients)

### Editing column names
# Remove spaces and special characters from column names, and replacing them with fullstop.
colnames(patients) <- make.names(colnames(patients), unique = TRUE)
View(patients)

### Refer to new csv file: "patients_cleaned.csv" for data analysis ###



#========================================================================================================================
# Lung Cancer Data Visualisation
#========================================================================================================================
# Load Data
patients <- fread("patients_cleaned.csv")

# Ensure 'Gender' is a factor with appropriate labels
patients$Gender <- factor(patients$Gender, levels = c(1, 2), labels = c("Male", "Female"))

gender_count <- table(patients$Gender)
print(gender_count)

# Plot with Gender split by Risk Level
ggplot(patients, aes(x = Gender, fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  labs(title = "Gender Split by Risk Level", x = "Gender", y = "Count", fill = "Risk Level") +
  theme_minimal()

# Air Pollution vs Risk Level
ggplot(patients, aes(x = as.factor(`Air.Pollution`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Air Pollution by Risk Level", x = "Air Pollution Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Alcohol Use vs Risk Level
ggplot(patients, aes(x = as.factor(`Alcohol.Use`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Alcohol Use by Risk Level", x = "Alcohol Use Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Dust Allergy vs Risk Level
ggplot(patients, aes(x = as.factor(`Dust.Allergy`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Dust Allergy by Risk Level", x = "Dust Allergy Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Occupational Hazards vs Risk Level
ggplot(patients, aes(x = as.factor(`Occupational.Hazards`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Occupational Hazards by Risk Level", x = "Occupational Hazards Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Genetic Risk vs Risk Level
ggplot(patients, aes(x = as.factor(`Genetic.Risk`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Genetic Risk by Risk Level", x = "Genetic Risk Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Chronic Lung Disease vs Risk Level
ggplot(patients, aes(x = as.factor(`Chronic.Lung.Disease`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Chronic Lung Disease by Risk Level", x = "Chronic Lung Disease Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Balanced Diet vs Risk Level
ggplot(patients, aes(x = as.factor(`Balanced.Diet`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Balanced Diet by Risk Level", x = "Balanced Diet Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Obesity vs Risk Level
ggplot(patients, aes(x = as.factor(`Obesity`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Obesity by Risk Level", x = "Obesity Level", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Smoking Frequency vs Risk Level
ggplot(patients, aes(x = as.factor(`Smoking.Frequency`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Smoking Frequency by Risk Level", x = "Smoking Frequency", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Passive Smoker Frequency vs Risk Level
ggplot(patients, aes(x = as.factor(`Passive.Smoker.Frequency`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Passive Smoker Frequency by Risk Level", x = "Passive Smoker Frequency", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Chest Pain vs Risk Level
ggplot(patients, aes(x = as.factor(`Chest.Pain`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Chest Pain by Risk Level", x = "Chest Pain", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Coughing of Blood vs Risk Level
ggplot(patients, aes(x = as.factor(`Coughing.of.Blood`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Coughing of Blood by Risk Level", x = "Coughing of Blood", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Fatigue vs Risk Level
ggplot(patients, aes(x = as.factor(`Fatigue`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Fatigue by Risk Level", x = "Fatigue", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Weight Loss vs Risk Level
ggplot(patients, aes(x = as.factor(`Weight.Loss`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Weight Loss by Risk Level", x = "Weight Loss", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Shortness of Breath vs Risk Level
ggplot(patients, aes(x = as.factor(`Shortness.of.Breath`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Shortness of Breath by Risk Level", x = "Shortness of Breath", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Wheezing vs Risk Level
ggplot(patients, aes(x = as.factor(`Wheezing`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Wheezing by Risk Level", x = "Wheezing", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Swallowing Difficulty vs Risk Level
ggplot(patients, aes(x = as.factor(`Swallowing.Difficulty`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Swallowing Difficulty by Risk Level", x = "Swallowing Difficulty", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Clubbing of Finger Nails vs Risk Level
ggplot(patients, aes(x = as.factor(`Clubbing.of.Finger.Nails`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Clubbing of Finger Nails by Risk Level", x = "Clubbing of Finger Nails", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Frequent Cold vs Risk Level
ggplot(patients, aes(x = as.factor(`Frequent.Cold`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Frequent Cold by Risk Level", x = "Frequent Cold", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Dry Cough vs Risk Level
ggplot(patients, aes(x = as.factor(`Dry.Cough`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Dry Cough by Risk Level", x = "Dry Cough", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Snoring vs Risk Level
ggplot(patients, aes(x = as.factor(`Snoring`), fill = `Risk.Level`)) +
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = percent_format(scale = 1)) +
  labs(title = "Distribution of Snoring by Risk Level", x = "Snoring", y = "Proportion", fill = "Risk Level") +
  theme_minimal()

# Bar plot of age group distribution
ggplot(patients, aes(x = Age)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Age Group Distribution", x = "Age Group", y = "Count") +
  theme_minimal()       

# Ensure Age_Group and Risk_Level are factors
patients$Age <- factor(patients$Age, levels = c("0-25", "26-35", "36-45", "46-55", "56-65", "65+"))
patients$Risk_Level <- factor(patients$Risk.Level, levels = c("Low", "Medium", "High"))

# Create a contingency table of Age_Group and Risk_Level
age_risk_table <- table(patients$Age, patients$Risk.Level)

# Print the contingency table
print(age_risk_table)

# Calculate the proportions to see the distribution within each age group
age_risk_prop <- prop.table(age_risk_table, margin = 1) # Proportions within age groups
print(age_risk_prop)     



# Box plot of Age distribution by Risk Level
ggplot(patients, aes(x = `Risk.Level`, y = Age)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Risk Level", x = "Risk Level", y = "Age") +
  theme_minimal()

# Box plot to analyze Alcohol Use across different Risk Levels
ggplot(patients, aes(x = `Risk.Level`, y = `Alcohol.Use`)) +
  geom_boxplot() +
  labs(title = "Alcohol Use by Risk Level", x = "Risk Level", y = "Alcohol Use Level") +
  theme_minimal()

# Scatter plot of Air Pollution vs Smoking Frequency, colored by Risk Level
ggplot(patients, aes(x = `Air.Pollution`, y = `Smoking.Frequency`, color = `Risk.Level`)) +
  geom_jitter(alpha = 0.6) +
  labs(title = "Air Pollution vs Smoking Frequency by Risk Level", x = "Air Pollution Level", y = "Smoking Frequency") +
  theme_minimal()

# Heat map of Risk Level by Obesity and Chronic Lung Disease levels
ggplot(patients, aes(x = as.factor(`Obesity`), y = as.factor(`Chronic.Lung.Disease`), fill = `Risk.Level`)) +
  geom_tile() +
  labs(title = "Obesity and Chronic Lung Disease vs Risk Level", x = "Obesity Level", y = "Chronic Lung Disease Level", fill = "Risk Level") +
  theme_minimal()



# 3D scatter plot for Air Pollution, Smoking Frequency, and Risk Level (requires plotly package)
library(plotly)
plot_ly(patients, x = ~`Air.Pollution`, y = ~`Smoking.Frequency`, z = ~`Risk.Level`, color = ~`Risk.Level`, colors = c("red", "green", "blue")) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Air Pollution'),
                      yaxis = list(title = 'Smoking Frequency'),
                      zaxis = list(title = 'Risk Level')),
         title = "3D Scatter Plot of Air Pollution, Smoking Frequency, and Risk Level")

# Convert Risk Level to a factor if it's not already
patients$`Risk.Level` <- as.factor(patients$`Risk.Level`)

# Multinomial logistic regression model
library(nnet)
model_multinomial <- multinom(`Risk.Level` ~ `Air.Pollution` + `Alcohol.Use` + `Smoking.Frequency` + `Obesity` + `Chronic.Lung.Disease`,
                              data = patients)
summary(model_multinomial)



# Box Plot of Alcohol Use by Gender and Risk Level
ggplot(patients, aes(x = `Risk.Level`, y = `Alcohol.Use`, fill = Gender)) +
  geom_boxplot() +
  labs(title = "Alcohol Use by Gender and Risk Level", x = "Risk Level", y = "Alcohol Use Level", fill = "Gender") +
  theme_minimal()

# Contingency table for Obesity, Smoking Frequency, and Risk Level
contingency_patients <- as.data.frame.table(table(patients$`Obesity`, patients$`Smoking.Frequency`, patients$`Risk.Level`))
names(contingency_patients) <- c("Obesity", "Smoking Frequency", "Risk Level", "Count")
contingency_patients

# PCA (Principal Component Analysis) for dimensionality reduction on continuous variables
library(dplyr)
continuous_vars <- patients %>% select(`Air.Pollution`, `Alcohol.Use`, `Smoking.Frequency`, `Obesity`)
pca <- prcomp(continuous_vars, scale = TRUE)
summary(pca)

# Decision Tree for Predicting Risk Level
library(rpart)
tree_model <- rpart(`Risk.Level` ~ `Air.Pollution` + `Alcohol.Use` + `Smoking.Frequency` + `Obesity` + `Chronic.Lung.Disease`, data = patients)
plot(tree_model)
text(tree_model, use.n = TRUE)



#=======================================================================================================================
# Logistics Regression
#=======================================================================================================================
patients <- read.csv("patients_cleaned.csv")
View(patients)
set.seed(4260) #setting seed for reproducability

split <- sample.split(patients$Risk.Level, SplitRatio = 0.7) #splitting the data into our train sets and test sets
train_set <- subset(patients, split == TRUE)
test_set <- subset(patients, split == FALSE)

train_set$Risk.Level <- factor(train_set$Risk.Level, ordered = TRUE, levels = c("Low", "Medium", "High"))
test_set$Risk.Level <- factor(test_set$Risk.Level, ordered = TRUE, levels = c("Low", "Medium", "High"))
#making sure both our train and test sets are categorical

train_set$Age <- as.factor(train_set$Age)

#only runs with these variables, any other variables and an error comes where the algorithm cannot converge 
m1 <- polr(Risk.Level ~ Gender + Air.Pollution + Alcohol.Use + Dust.Allergy + Occupational.Hazards + Genetic.Risk + Chronic.Lung.Disease + Balanced.Diet + Obesity + Smoking.Frequency + Passive.Smoker.Frequency + Chest.Pain + Coughing.of.Blood + Weight.Loss + Shortness.of.Breath, data = train_set, Hess = TRUE)

#checking multicollinearity
vif(m1) 

m1_new <- polr(Risk.Level ~ Gender + Chronic.Lung.Disease + Air.Pollution + Alcohol.Use + Dust.Allergy + Genetic.Risk + Balanced.Diet + Obesity + Smoking.Frequency + Passive.Smoker.Frequency + Chest.Pain + Coughing.of.Blood + Weight.Loss + Shortness.of.Breath, data = train_set, Hess = TRUE) #removed occupational hazards because its vif value is greater than 10 

odds_ratios <- exp(coef(m1_new)) #looking at the odds ratio for all the variables
odds_ratios_patients <- data.frame(Variable = names(odds_ratios),OddsRatio = odds_ratios) #here we create a new datafrmae with Variable in one column and Odds Ratio in the other
odds_ratios_patients <- odds_ratios_patients[order(odds_ratios_patients$OddsRatio, decreasing = TRUE),] #here we are sorting the odds ratio in desc order, so we see the most significant predictors on the top
odds_ratios_patients

#hence, remove 'Genetic Risk'

m1_new1 <- polr(Risk.Level ~ Gender + Air.Pollution + Dust.Allergy + Alcohol.Use + Balanced.Diet + Obesity + Smoking.Frequency + Passive.Smoker.Frequency + Chest.Pain + Coughing.of.Blood + Weight.Loss + Shortness.of.Breath + Chronic.Lung.Disease, data = train_set, Hess = TRUE)

best_m1 <- stepAIC(m1_new1, direction = "both") #checking the model with the lowest AIC value

best_m1 <- polr(Risk.Level ~ Dust.Allergy + Obesity + Passive.Smoker.Frequency + Chest.Pain + Coughing.of.Blood + Weight.Loss + Shortness.of.Breath + Chronic.Lung.Disease, data = train_set, Hess = TRUE) #this was the model with the lowest AIC value which is 496.08

predictions <- predict(best_m1, newdata = test_set, type = "class") #predicting on test set

conf_matrix <- confusionMatrix(predictions, test_set$Risk.Level)
conf_matrix




#=======================================================================================================================
# CART - Random Forest 
#=======================================================================================================================
patients <- fread("patients_cleaned.csv")
View(patients)

# Convert all variables in the data frame to factors
patients[] <- lapply(patients, as.factor)
summary(patients)


### CART: Using Random Forest - Since our data set consist of about 1,000 data, we need to use random forest for a detailed analysis on our project: 

# Set the seed for reproducibility
set.seed(2024)


### Test - Train Set 70-30 ### ------------------------------------------------

# Split data into training and testing sets (70% training, 30% testing)
train_index <- createDataPartition(patients$Risk.Level, p = 0.7, list = FALSE)
train_data <- patients[train_index, ]
test_data <- patients[-train_index, ]

# Train the Random Forest model on the training data
rf_model <- randomForest(Risk.Level ~ ., data = train_data, ntree = 30, importance = TRUE)
rf_model


# OOB = 0%: This error rate indicates that the model perfectly classified the training data with no errors using out-of-bag (OOB) samples. In Random Forest, each tree is built using a random subset of the data, and the remaining samples (OOB samples) are used to estimate the model’s error rate. A 0% OOB error rate suggests the model is highly accurate on this dataset, though it might be overfitting if the dataset is too small or simple.

# class.error: Each class (High, Low, Medium) has a class error of 0, indicating perfect classification for each class in the training data.


### 10-fold cross-validation to check overfitting
train_control <- trainControl(method = "cv", number = 30) 

# Train the Random Forest model with cross-validation
cv_model <- train(Risk.Level ~ ., data = patients, method = "rf", trControl = train_control, ntree = 30, importance = TRUE)
cv_model

# The cross-validation results confirm that the model is highly accurate and generalizes well to different subsets of the data. The chosen mtry value of 92, which yielded 100% accuracy, seems optimal for the dataset. Overall, the results indicate a very reliable model that performs well across various data splits, minimizing the risk of overfitting.


# Predict on the test set
predictions <- predict(rf_model, newdata = test_data)

# Evaluate model performance
confusion_matrix <- confusionMatrix(predictions, test_data$Risk.Level)
confusion_matrix

# Evaluate Predictive Accuracy
accuracy <- confusion_matrix$overall["Accuracy"]
accuracy

# Show the importance of each variable
importance(rf_model)
varImpPlot(rf_model)

### Create a table showing the Number of Occurence Through Trees (NOTT) and the Degree of Importance of each variables:
# Calculate variable importance (Degree of Importance)
var_importance <- importance(rf_model, type = 2)  # Mean Decrease Gini

# Get Degree of Importance (DOI) and approximate NOTT
importance_patients <- data.frame(Risk_Factor = rownames(var_importance), Degree_of_Importance = var_importance[,"MeanDecreaseGini"])

# Sort by Degree of Importance for presentation
importance_patients <- importance_patients %>% arrange(desc(Degree_of_Importance))

# Approximate Number of Occurrences Through Trees (NOTT)
# This is an approximation since actual split counts are not provided by randomForest in R studio.
importance_patients$Number_of_Occurrence_Through_Trees <- round(scales::rescale(importance_patients$Degree_of_Importance, to = c(1, 10)))

# Display the final table
kable(importance_patients, col.names = c("Risk Factor", "Number of Occurrence Through Trees", "Degree of Importance"))








