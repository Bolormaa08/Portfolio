#https://www.kaggle.com/datasets/kaggle/sf-salaries

install.packages("naniar")
install.packages("caret")
install.packages("class")


library(naniar)
library(dplyr)
library(ggplot2)
library(lattice)
library(caret)
library(readr)
library(class)

#Read the dataset
data <- read.csv("C:\\Users\\35387\\OneDrive - National College of Ireland\\NCI data analyst\\DMML\\PROJECT\\x23176725\\Dataset 2\\Salaries.csv")

table(data$TotalPay)
# Randomly sample 10% of the rows

# Assuming 'data' is your dataset
salaries_data <- data[sample(nrow(data), nrow(data) * 0.1), ]


# Write the sampled data to a new CSV file
write_csv(salaries_data, 'salaries_data.csv')

 sapply(salaries_data, class)
# View data 
names(salaries_data)
str(salaries_data)

# Descriptive Statistics
summary(salaries_data)
# Levels of Measurement
sapply(salaries_data, class)

salaries_data$BasePay <- as.numeric(as.factor(salaries_data$BasePay))
salaries_data$OvertimePay <- as.numeric(as.factor(salaries_data$OvertimePay))
salaries_data$OtherPay <- as.numeric(as.factor(salaries_data$OtherPay))
salaries_data$Benefits <- as.numeric(as.factor(salaries_data$Benefits))

# Levels of Measurement
sapply(salaries_data, class)

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(salaries_data$TotalPay)
psych::describe(salaries_data$TotalPayBenefits)
psych::describe(salaries_data$BasePay)
psych::describe(salaries_data$OvertimePay)
psych::describe(salaries_data$OtherPay)
psych::describe(salaries_data$Benefits)




#create histogram for original distribution
hist(salaries_data$TotalPay, col='steelblue', main="Total pay annually")
hist(salaries_data$TotalPayBenefits, col='steelblue', main='Total pay benefits annually')
hist(salaries_data$BasePay, col='steelblue', main='Base pay annually')
hist(salaries_data$OvertimePay, col='steelblue', main=' Overtime pay annually')
hist(salaries_data$OtherPay, col='steelblue', main=' Other pay annually')
hist(salaries_data$Benefits, col='steelblue', main=' Benefits annually')



boxplot(salaries_data$TotalPay, col='red', main='Outliers Total pay', ylab="Total pay")
boxplot(salaries_data$TotalPayBenefits, col='red', main='Outliers Totalpay Benefits', ylab="Total pay Benefits")
boxplot(salaries_data$BasePay, col='red', main='Outliers Base pay benefits', ylab="Base pay")
boxplot(salaries_data$OvertimePay, col='red', main=' Outliers Overtime pay', ylab="Overtime pay")
boxplot(salaries_data$OtherPay, col='red', main=' Outliers Other pay', ylab="Other pay")
boxplot(salaries_data$Benefits, col='red', main=' Outliers Benefits ', ylab="Benefits")

# Scatter plot for BasePay vs TotalPay
ggplot(salaries_data, aes(x = BasePay, y = TotalPay)) +
  geom_point() +
  labs(x = "Base Pay", y = "Total Pay") +
  ggtitle("Scatter Plot of Base Pay vs Total Pay")
# Scatter plot for OvertimePay vs TotalPay
ggplot(salaries_data, aes(x = OvertimePay, y = TotalPay)) +
  geom_point() +
  labs(x = "Base Pay", y = "Total Pay") +
  ggtitle("Scatter Plot of OvertimePay vs Total Pay")
# Scatter plot for OtherPay vs TotalPay
ggplot(salaries_data, aes(x = OtherPay, y = TotalPay)) +
  geom_point() +
  labs(x = "Base Pay", y = "Total Pay") +
  ggtitle("Scatter Plot of OtherPay vs Total Pay")

# Creating the bar chart for job titles
ggplot(salaries_data, aes(x = JobTitle)) +
  geom_bar(fill = "skyblue") +
  labs(x = "Job Title", y = "Count") +
  ggtitle("Count of Each Job Title") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotating x-axis labels for better readability


####################Data Cleaning###################################

salaries_data <- salaries_data[, !colnames(salaries_data) %in% c("Notes", "Agency", "Status")]

# Identify missing values in the entire dataset
sum(is.na(salaries_data))

#Identify missing values for all columns and store the result in a data frame
missing_data <- data.frame(
  Column = names(salaries_data),
  Missing_Values = sapply(salaries_data, function(x) sum(is.na(x))))
print(missing_data)
# Remove rows with missing values
salaries_data <- na.omit(salaries_data)
# Remove duplicates
salaries_data <- unique(salaries_data) 
summary(salaries_data)
# 
# #Columns to encode
# cols_to_encode <- c("JobTitle" )
# # Use model.matrix to create dummy variables (one-hot encoding)
# salaries_data <- cbind(salaries_data[, !grepl("JobTitle", names(salaries_data))],
#                     model.matrix(~ JobTitle - 1, data = salaries_data))
# print(salaries_data)
# sapply(salaries_data, class)
# summary(salaries_data)



#Outliers
numerical_columns <- sapply(salaries_data, is.numeric)
summary(salaries_data)
# Identify and remove outliers for all numeric variables
for (col in names(salaries_data)[numerical_columns]) {
  # Calculate IQR for each column
  Q1 <- quantile(salaries_data[[col]], 0.25)
  Q3 <- quantile(salaries_data[[col]], 0.75)
  IQR_value <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identify outliers
  outliers <- salaries_data[[col]][salaries_data[[col]] < lower_bound | salaries_data[[col]] > upper_bound]
  
  # Remove outliers
  salaries_data <- salaries_data[!(salaries_data[[col]] < lower_bound | salaries_data[[col]] > upper_bound), ]
  
  # Display summary statistics after removing outliers
  
  #cat("Potential outliers for", col, ":", outliers, "\n")
}

# Display summary statistics after removing outliers
summary(salaries_data)

boxplot(salaries_data$TotalPay, col='red', main='After removing Outliers Total pay', ylab="Total pay")
boxplot(salaries_data$TotalPayBenefits, col='red', main='After removing Outliers Totalpay Benefits', ylab="Total pay Benefits")
boxplot(salaries_data$BasePay, col='red', main='After removing Outliers Base pay benefits', ylab="Base pay")
boxplot(salaries_data$OvertimePay, col='red', main='After removing Outliers Overtime pay', ylab="Overtime pay")
boxplot(salaries_data$OtherPay, col='red', main='After removing Outliers Other pay', ylab="Other pay")
boxplot(salaries_data$Benefits, col='red', main=' After removing Outliers Benefits ', ylab="Benefits")

####################Correlation matrix########################


# Identify numerical and categorical variables
numerical_vars <- sapply(salaries_data, is.numeric)

sapply(salaries_data, class)
# Identify columns with constant or duplicated values
constant_cols <- sapply(salaries_data[, numerical_vars], function(x) {
  all(duplicated(x)[-1L])
})

# Identify columns with zero variance for non-factor columns
zero_var_cols <- sapply(salaries_data[, numerical_vars], function(x) {
  if (!is.factor(x)) {
    var(x) == 0
  } else {
    FALSE
  }
})

summary(salaries_data)

# Combine both methods to find columns with zero variance or constant values
problematic_cols <- zero_var_cols | constant_cols

# Remove columns with zero variance or constant values
salaries_data <- salaries_data[, !problematic_cols]

# Identify numerical and categorical variables
numerical_vars <- sapply(salaries_data, is.numeric)

cor_matrix <- cor(salaries_data[, numerical_vars])
print(cor_matrix)

#Create a heatmap
corrplot::corrplot(cor_matrix, method = "number")
summary(salaries_data)

salaries_data <- salaries_data[, !colnames(salaries_data) %in% c("Id","Year", "TotalPayBenefits","EmployeeName", "JobTitle")]
summary(salaries_data)





#Data Transformation
#Standardize numeric predictors for consistency
numeric_predictors <- c("BasePay", "OvertimePay","OtherPay","Benefits")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#Apply min-max scaling function to relevant columns
salaries_data[numeric_predictors] <- lapply(salaries_data[numeric_predictors], min_max_transform)


summary(salaries_data)
sapply(salaries_data,class)

####################### Build KNN model############

# Assuming your_data is your dataset and target is your target column
# Splitting the data into training and testing sets
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(salaries_data$TotalPay, p = 0.8, list = FALSE)
data_train <- salaries_data[trainIndex, ]
data_test <- salaries_data[-trainIndex, ]

# Setting up the control parameters for the model
ctrl <- trainControl(method = "cv", number = 10)  # 10-fold cross-validation

################Model 1###################

# Perform ANOVA
anova_result <- aov(TotalPay ~ BasePay +OvertimePay + OtherPay + Benefits , data = data_train)

# Summary of ANOVA results
summary(anova_result)

set.seed(124)
# Training the KNN regression model
knn_model_1 <- train(TotalPay ~ ., data = data_train, method = "knn")
print(knn_model_1)
summary(knn_model_1)
#  k  RMSE      Rsquared   MAE     
#5  31056.41  0.6016150  16925.91
#7  30487.27  0.6109612  17045.91
#9  30252.62  0.6140501  17206.20

# Predictions on test data
predictions_1 <- predict(knn_model_1, newdata = data_test)

# Calculate performance metrics
mse <- mean(( data_test$TotalPay - predictions_1)^2)
rmse <- sqrt(mse)
# Printing the metrics
cat("Mean Squared Error (MSE): ", mse, "\n")
#905872291


pairs(data_train[, c("BasePay", "OvertimePay", "OtherPay", "Benefits")])

actual_values<- data_test$TotalPay

# Assuming 'predictions' contains the predicted values and 'actual_values' contains the actual values
plot(predictions_1, actual_values, 
     xlab = "Predicted Values", ylab = "Actual Values",
     main = "Actual vs. Predicted Values")
abline(0, 1, col = "red")  # Adding a diagonal line for reference

residuals <- actual_values - predictions_1
plot(predictions_1, residuals, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")  # Adding a horizontal line at 0 for reference




#####################Model2###################

ctrl <- trainControl(method = "cv", number = 10)

# Define the tuning grid (e.g., K values from 1 to 20)
k_values <- data.frame(k = seq(1, 20, by = 1))

# Initialize a vector to store R-squared values
rsquared_values <- numeric(nrow(k_values))

# Train the KNN model with cross-validation and collect R-squared values
for (i in 1:nrow(k_values)) {
  tuning_grid <- data.frame(k = k_values$k[i])
  set.seed(125)
  knn_model_2 <- train(TotalPay ~ ., data = data_train, method = "knn",
                     trControl = ctrl, tuneGrid = tuning_grid)
  
  # Get predictions on the test set
  predictions_2 <- predict(knn_model_2, newdata = data_test)
  
  # Calculate R-squared on the test set
  rsquared <- cor(data_test$TotalPay, predictions_2)^2
  
  # Store R-squared value for this fold
  rsquared_values[i] <- rsquared
}

# Find the best K value and its corresponding R-squared
best_k <- k_values$k[which.max(rsquared_values)]
best_rsquared <- max(rsquared_values)

# Print the best K value and its corresponding R-squared
cat("Best K value:", best_k, "\n")
#Best K value: 7
cat("Best R-squared:", best_rsquared, "\n")
#Best R-squared:0.6294305 

# Train the KNN model with the best K value
best_tuning_grid <- data.frame(k = best_k)
best_knn_model <- train(TotalPay ~ ., data = data_train, method = "knn",
                        trControl = ctrl, tuneGrid = best_tuning_grid)

# Get predictions on the test set using the best model
best_predictions <- predict(best_knn_model, newdata = data_test)

# Calculate evaluation metrics (MSE, RMSE, MAE) for the best model
best_mse <- mean((data_test$TotalPay - best_predictions)^2)
best_rmse <- sqrt(mean((data_test$TotalPay - best_predictions)^2))
best_mae <- mean(abs(data_test$TotalPay - best_predictions))

# Print the evaluation metrics for the best K value
cat("Best K value:", best_k, "\n")
#7
cat("Best R-squared:", best_rsquared, "\n")
#0.6294305
cat("MSE for Best K:", best_mse, "\n")
#896677489 
cat("RMSE for Best K:", best_rmse, "\n")
#29944.57
cat("MAE for Best K:", best_mae, "\n")
#17170.6  


# Convert data to data frame for plotting
data_for_plot <- data.frame(K_Values = k_values, R_Squared = rsquared_values)

# Plotting Model Performance with Different K Values
plot(data_for_plot$k, data_for_plot$R_Squared, 
     type = "b", 
     xlab = "K Values", ylab = "R-squared",
     main = "Model Performance with Different K Values")


# Assuming 'predictions' contains the predicted values and 'actual_values' contains the actual values
plot(best_predictions, actual_values, 
     xlab = "Predicted Values", ylab = "Actual Values",
     main = "Actual vs. Predicted Values")
abline(0, 1, col = "red")  # Adding a diagonal line for reference

best_residuals <- actual_values - best_predictions
plot(best_predictions, best_residuals, 
     xlab = "Predicted Values", ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")  # Adding a horizontal line at 0 for reference
