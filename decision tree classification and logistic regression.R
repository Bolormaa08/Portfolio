#https://catalog.data.gov/dataset/heart-disease-mortality-data-among-us-adults-35-by-state-territory-and-county?fbclid=IwAR1GMrbyJaxzeq8nIFd5m-nA3cuhdnF0mFhQu0sM-rzLbHBDjUHH0PZ-5W4
install.packages("rpart.plot")
install.packages("pROC")
install.packages("rms")
install.packages("olsrr")
install.packages("effects")
detach("package:car", unload = TRUE)
install.packages("car")
install.packages("ggeffects")  # Install the package if you haven't already
install.packages("rms")



library(stats)
library(rpart)
library(MLmetrics)
library(rpart.plot)
library(pROC)
library(caret)
library(rms)
library(pROC)  # For ROC curve
library(rms)   # For calibration curve
library(olsrr) # For diagnostic plots
library(car)   # For leverage-residual plot and added variable plot
library(effects)
library(ggeffects)  # Load the package


# Load data from a CSV file
heart_data <- read.csv("C:\\Users\\35387\\OneDrive - National College of Ireland\\NCI data analyst\\DMML\\PROJECT\\x23176725\\Dataset 3\\Heart_Disease_Mortality_Data_Among_US_Adults__35___by_State_Territory_and_County.csv", header = TRUE, stringsAsFactors = T)

# View data 
names(heart_data)
str(heart_data)

# Descriptive Statistics
summary(heart_data)

sapply(heart_data, class)

#create histogram for original distribution
hist(heart_data$Data_Value, col='steelblue', main='Data value')
hist(heart_data$LocationID, col='steelblue', main='Location ID')


boxplot(heart_data$Data_Value, col='red', main='Data Value', ylab="Data value")
boxplot(heart_data$LocationID, col='red', main='Lot Frontage', ylab="Location Id")

####################Data Cleaning###################################
#Drop a level
heart_data$Stratification1 <- droplevels(heart_data$Stratification1, exclude = "Overall")

# Identify missing values in the entire dataset
sum(is.na(heart_data))

#Identify missing values for all columns and store the result in a data frame
missing_data <- data.frame(
  Column = names(heart_data),
  Missing_Values = sapply(heart_data, function(x) sum(is.na(x))))
print(missing_data)
# Remove rows with missing values
heart_data <- na.omit(heart_data)
# Remove duplicates
heart_data <- unique(heart_data) 
summary(heart_data)



heart_data <- heart_data[, !colnames(heart_data) %in% c("Year","DataSource","LocationAbbr","LocationDesc",
                                                        "StratificationCategory2" ,"TopicID","Data_Value_Footnote_Symbol",
                                                        "Data_Value_Footnote","Data_Value_Unit","GeographicLevel","StratificationCategory1")]
summary(heart_data)
sapply(heart_data, class)

# Columns to encode
cols_to_encode <- c( "Stratification2","Data_Value_Type","Class ","Topic")


# Use log_model.matrix to create dummy variables (one-hot encoding)
encoded_df <- cbind(heart_data[, !grepl("Stratification2|Data_Value_Type", names(heart_data))],
                    model.matrix(~Stratification2 +Data_Value_Type  - 1, data = heart_data))

print(encoded_df)
sapply(encoded_df, class)
summary(encoded_df)
sapply(heart_data, class)

#Outliers
clean_data <- encoded_df
numerical_columns <- sapply(clean_data, is.numeric)
summary(clean_data)

# Identify and remove outliers for all numeric variables
for (col in names(clean_data)[numerical_columns]) {
  # Calculate IQR for each column
  Q1 <- quantile(clean_data[[col]], 0.25)
  Q3 <- quantile(clean_data[[col]], 0.75)
  IQR_value <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identify outliers
  outliers <- clean_data[[col]][clean_data[[col]] < lower_bound | clean_data[[col]] > upper_bound]
  
  # Remove outliers
  clean_data <- clean_data[!(clean_data[[col]] < lower_bound | clean_data[[col]] > upper_bound), ]
  
  # Display summary statistics after removing outliers
  cat("Potential outliers for", col, ":", outliers, "\n")
}

# Display summary statistics after removing outliers
summary(clean_data)


boxplot(clean_data$Data_Value, col='red', main='After removing outliers data value', ylab="")
boxplot(clean_data$LocationID, col='red', main='After removing outliers Location ID', ylab="")


####################Correlation matrix########################
sapply(clean_data, class)

Class<- as.numeric(clean_data$Class)
topic<- as.numeric(clean_data$Topic)

sapply(clean_data, class)
summary(clean_data)

clean_data <- clean_data[, !colnames(clean_data) %in% c("Stratification2Asian and Pacific Islander","Stratification2Black","Stratification2American Indian and Alaskan Native",
                                                        "Stratification2Hispanic", "Data_Value_TypeAge-adjusted, Spatially Smoothed, 3-year Average Rate","Location.1"
                                                        ,"Class","Topic")]
summary(clean_data)

# Identify numerical and categorical variables
numerical_vars <- sapply(clean_data, is.numeric)
categorical_vars <- setdiff(names(clean_data), names(clean_data)[numerical_vars])


# Calculate correlations for numerical variables
numerical_correlations <- cor(clean_data[, numerical_vars])
print(numerical_correlations)

#Create a heatmap
corrplot::corrplot(numerical_correlations, method = "number")
summary(clean_data)

clean_data <- clean_data[, !colnames(clean_data) %in% c("Stratification2Overall")]


summary(clean_data)

#Data Transformation
#Standardize numeric features for consistency
numeric_features <- c("Data_Value","LocationID", "Stratification2White")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

sapply(clean_data, class)
#Apply min-max scaling function to relevant columns
normalized_data <- lapply(clean_data[numeric_features], min_max_transform)
clean_data[numeric_features] <- normalized_data

summary(clean_data)

################## Decision tree classification###########
set.seed(123)
partition <- createDataPartition(y = clean_data$Stratification1, p = 0.8, list = FALSE)
train_data <- clean_data[partition, ]
test_data <- clean_data[-partition, ]

# Fit a decision tree model
tree_model <- rpart(Stratification1 ~ Data_Value + LocationID  +Stratification2White, data = train_data, method = "class")

rpart.plot(tree_model)

# Display the tree
plot(tree_model)
text(tree_model)

predictions <- predict(tree_model, newdata = test_data, type = "class")

test_data$Stratification1 <- as.factor(test_data$Stratification1)

#Confusion matrix
conf_matrix <- table(Actual = test_data$Stratification1, predicted = predictions)

#Calculate accuracy, precision, recall, and F1-score
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
f1_score <- 2 * (precision * recall) / (precision + recall)

#Print the confusion matrix
print("Confusion Matrix:")
print(conf_matrix)
#        predicted
#Actual   Female Male
#Female    966  301
#Male      133 1127

#Print evaluation metrics
cat(paste("Accuracy: ", accuracy, "\n")) #Accuracy:  0.828254847645429
cat(paste("Precision: ", precision, "\n")) #Precision:  0.78921568627451
cat(paste("Recall: ", recall, "\n")) #Recall:  0.894444444444444 
cat(paste("F1 Score: ", f1_score, "\n")) #F1 Score:  0.838541666666667

# Calculate accuracy
correct_predictions <- sum(predictions == test_data$Stratification1)
total_instances <- nrow(test_data)
accuracy <- correct_predictions / total_instances

print(paste("Accuracy:", accuracy))
#0.828254847645429

test_data$Stratification1 <- as.factor(test_data$Stratification1)
contrasts(test_data$Stratification1)

#Cross-validation
#Define parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 10)  #10-fold cross-validation

#Define model
cvmodel_results <- train(Stratification1 ~ ., data = train_data, method = "rpart", trControl = ctrl)

#Print the results
print(cvmodel_results)
# cp           Accuracy   Kappa    
#0.005277778  0.8673737  0.7347617
#0.008829365  0.8412617  0.6826202
#0.681547619  0.7342728  0.4676194



###########################################################################################
################################ Logistic regression#######################################
###########################################################################################
# Build logistic regression model
log_model <- glm(Stratification1 ~ ., data = train_data, family = "binomial")

print(summary(log_model))
#AIC: 7375.7

# Predict on test data
predicted_probs <- predict(log_model, newdata = test_data, type = "response")

# Confusion Matrix
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) # Assuming a threshold of 0.5

conf_matrix <- table(Actual = test_data$Stratification1, Predicted = predicted_classes)
print(conf_matrix)
#Predicted
#Actual      0    1
#Female 1082  185
#Male    223 1037

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)
#0.8385437

# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(precision)
#0.8486088

# Recall (Sensitivity)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(recall)
#0.8230159

# F1-score
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
#0.8356164

# ROC Curve and AUC
roc_obj <- roc(test_data$Stratification1, predicted_probs)
auc_result <- auc(roc_obj)
print(paste("AUC:", auc_result))
#[1] "AUC: 0.921642174365142"

#1. Plot ROC curve
plot(roc_obj, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)
text(0.8, 0.2, paste("AUC =", round(auc(roc_obj), 4)), col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 4)), col = "blue", lty = 1, cex = 0.8)


#2. Calibration curve
# Creating a formula with the predicted probabilities and the actual outcomes
calib_formula <- as.formula(paste("Stratification1~ ", predicted_probs))
# Creating a calibration object
calib_obj <- calibration(calib_formula, data = test_data, bw = TRUE)
# Plotting the calibration curve
plot(calib_obj, main = "Calibration Curve")


# 3. Create partial effect plots for logistic regression
partial_plot <- ggpredict(log_model, terms = "Stratification2White")
plot(partial_plot)

partial_plot <- ggpredict(log_model, terms = "Data_Value")
plot(partial_plot)

partial_plot <- ggpredict(log_model, terms = "LocationID")
plot(partial_plot)

# 4. Leverage-Residual Plot
influencePlot(log_model)

# 5. Cook's Distance Plot
cooksd <- cooks.distance(log_model)
plot(cooksd, pch = "24", main = "Cook's Distance Plot")
abline(h = 4/length(log_model$residuals), col = "red")

# 6. Added Variable Plot (for individual predictors)
avPlots(log_model)
# Residual Analysis (if applicable)
residuals <- residuals(log_model)

# Cross-Validation
fitControl <- trainControl(method = "cv", number = 10)
model <- train(Stratification1 ~ ., data = train_data, method = "glm", trControl = fitControl, family = "binomial")

# Information Criteria
AIC_value <- AIC(log_model)
BIC_value <- BIC(log_model)
print(paste("AIC:", AIC_value, "BIC:", BIC_value))
#[1] "AIC: 7375.71055931515 BIC: 7404.59607619152"


# Cross-validation
cv_log_model <- train(Stratification1 ~ ., data = train_data, method = "glm", family = "binomial")
print(cv_log_model)
#Accuracy   Kappa    
#0.8381012  0.6761353

# Validation Set Performance
test_predictions <- predict(cv_log_model, newdata = test_data)
conf_matrix <- confusionMatrix(test_predictions, test_data$Stratification1)
print(conf_matrix)
#Accuracy:0.8385
#Kappa: 0.6771


