## Global air pollution dateset
#https://www.kaggle.com/datasets/hasibalmuzdadid/global-air-pollution-dataset

install.packages("lattice")
install.packages("psych")
install.packages("ggqqplot")
install.packages("caret")
install.packages("lmtest")
install.packages(c("Metrics", "MLmetrics"))
install.packages("naniar")
install.packages("leaflet")
install.packages("spdep")
install.packages("gstat")
install.packages("performance")
install.packages("car")
install.packages("rsample")
install.packages("rsample", repos='http://cran.us.r-project.org')
install.packages("sfsmisc")


library(dplyr)
library(car)
library(ggplot2)
library(lattice)
library(caret)
library(lmtest)
library(Metrics)
library(naniar)
library(gplots)
library(leaflet)
library(spdep)
library(gstat)
library(performance)
library(caret)
library(randomForest)



# Load data from a CSV file
aqi_data <- read.csv("C:/Users/35387/OneDrive - National College of Ireland/NCI data analyst/DMML/PROJECT/DMML project 1/global air pollution dataset.csv", header = TRUE, stringsAsFactors = T)

# View data 
names(aqi_data)
str(aqi_data)

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(aqi_data$AQI.Value)
psych::describe(aqi_data$NO2.AQI.Value)
psych::describe(aqi_data$CO.AQI.Value)
psych::describe(aqi_data$PM2.5.AQI.Value)
psych::describe(aqi_data$Ozone.AQI.Value)

# Descriptive Statistics
summary(aqi_data)

# Levels of Measurement
sapply(aqi_data, class)



#create histogram for original distribution
hist(aqi_data$AQI.Value, col='steelblue', main="Overall AQI value of the city")
hist(aqi_data$CO.AQI.Value, col='steelblue', main='AQI value of Carbon Monoxide of the city')
hist(aqi_data$Ozone.AQI.Value, col='steelblue', main='AQI value of Ozone of the city')
hist(aqi_data$NO2.AQI.Value, col='steelblue', main=' AQI value of Nitrogen Dioxide of the city')
hist(aqi_data$PM2.5.AQI.Value, col='steelblue', main=' AQI value of Particulate Matter with a diameter of 2.5 micrometers or less of the city')




boxplot(aqi_data$AQI.Value, col='red', main='Outliers AQI value of the city', ylab="AQI value")
boxplot(aqi_data$CO.AQI.Value, col='red', main='Outliers AQI value of Carbon Monoxide of the city', ylab="CO AQI value")
boxplot(aqi_data$NO2.AQI.Value, col='red', main='Outliers AQI value of Nitrogen Dioxide of the city', ylab="NO2 AQI value")
boxplot(aqi_data$PM2.5.AQI.Value, col='red', main=' Outliers AQI value of Particulate Matter with a diameter of 2.5 micrometers or less of the city', ylab="PM2.5 AQI value")
boxplot(aqi_data$Ozone.AQI.Value, col='red', main=' Outliers AQI value of Ozone of the city', ylab="Ozone AQI value")



####################Data Cleaning###################################
# Identify missing values in the entire dataset
sum(is.na(aqi_data))

#Identify missing values for all columns and store the result in a data frame
missing_data <- data.frame(
  Column = names(aqi_data),
  Missing_Values = sapply(aqi_data, function(x) sum(is.na(x))))
print(missing_data)
# Remove rows with missing values
aqi_data <- na.omit(aqi_data)
# Remove duplicates
aqi_data <- unique(aqi_data) 
summary(aqi_data)
#Create a heatmap or bar plot to visualize the extent and pattern of missing values
vis_miss(aqi_data)



#Outliers

numerical_columns <- sapply(aqi_data, is.numeric)
summary(aqi_data)
# Identify and remove outliers for all numeric variables
for (col in names(aqi_data)[numerical_columns]) {
  # Calculate IQR for each column
  Q1 <- quantile(aqi_data[[col]], 0.25)
  Q3 <- quantile(aqi_data[[col]], 0.75)
  IQR_value <- Q3 - Q1
  
  # Define lower and upper bounds
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Identify outliers
  outliers <- aqi_data[[col]][aqi_data[[col]] < lower_bound | aqi_data[[col]] > upper_bound]
  
  # Remove outliers
  aqi_data <- aqi_data[!(aqi_data[[col]] < lower_bound | aqi_data[[col]] > upper_bound), ]
  
  # Display summary statistics after removing outliers
  cat("Potential outliers for", col, ":", outliers, "\n")
}

# Display summary statistics after removing outliers
summary(aqi_data)

boxplot(aqi_data$AQI.Value, col='red', main='After removing outliers AQI value of the city', ylab="AQI value")
boxplot(aqi_data$CO.AQI.Value, col='red', main='After removing outliers AQI value of Carbon Monoxide of the city', ylab="CO AQI value")
boxplot(aqi_data$NO2.AQI.Value, col='red', main='After removing outliers AQI value of Nitrogen Dioxide of the city', ylab="NO2 AQI value")
boxplot(aqi_data$PM2.5.AQI.Value, col='red', main=' After removing outliers AQI value of Particulate Matter with a diameter of 2.5 micrometers or less of the city', ylab="PM2.5 AQI value")
boxplot(aqi_data$Ozone.AQI.Value, col='red', main=' After removing outliers AQI value of Ozone of the city', ylab="Ozone AQI value")



####################Correlation matrix########################
# Calculate correlations for numerical variables
aqi_data <- aqi_data[, !colnames(aqi_data) %in% c("Country", "City", "AQI.Category",
                                                  "NO2.AQI.Category", "PM2.5.AQI.Category","Ozone.AQI.Category",
                                                  "CO.AQI.Category")]
summary(aqi_data)
# Identify numerical and categorical variables
numerical_vars <- sapply(aqi_data, is.numeric)

sapply(aqi_data, class)
# Identify columns with constant or duplicated values
constant_cols <- sapply(aqi_data[, numerical_vars], function(x) {
  all(duplicated(x)[-1L])
})

# Identify columns with zero variance for non-factor columns
zero_var_cols <- sapply(aqi_data[, numerical_vars], function(x) {
  if (!is.factor(x)) {
    var(x) == 0
  } else {
    FALSE
  }
})

# Combine both methods to find columns with zero variance or constant values
problematic_cols <- zero_var_cols | constant_cols

# Remove columns with zero variance or constant values
aqi_data <- aqi_data[, !problematic_cols]

# Identify numerical and categorical variables
numerical_vars <- sapply(aqi_data, is.numeric)

correlations <- cor(aqi_data[, numerical_vars])
print(correlations)

#Create a heatmap
corrplot::corrplot(correlations, method = "number")
summary(aqi_data)





#Data Transformation
#Standardize numeric features for consistency
numeric_features <- c("Ozone.AQI.Value", "NO2.AQI.Value","PM2.5.AQI.Value")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#Apply min-max scaling function to relevant columns
aqi_data[numeric_features] <- lapply(aqi_data[numeric_features], min_max_transform)


summary(aqi_data)

##############################################################################
####################### Build a regression model##############################
##############################################################################

### Model1 ###
set.seed(123)
partition <- createDataPartition(y = aqi_data$AQI.Value, p = 0.8, list = FALSE)
train_data <- aqi_data[partition, ]
test_data <- aqi_data[-partition, ]

model_1 <- lm(AQI.Value~ Ozone.AQI.Value + NO2.AQI.Value + PM2.5.AQI.Value
              , data = train_data)

summary(model_1)
#Adjusted R-squared:  0.898 , Residual standard error: 4.918 on 10192 degrees of freedom
check_model(model_1)
ncvTest(model_1)
#Chisquare = 1327.127, Df = 1, p = < 2.22e-16
check_heteroscedasticity(model_1)

vif(model_1)
check_collinearity(model_1)

durbinWatsonTest(model_1)
#D-W Statistic: 2.002122
summary(train_data$AQI.Value)

plot(model_1$fitted.values, model_1$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")

qqnorm(model_1$residuals)
qqline(model_1$residuals)

sqrt_abs_resid <- sqrt(abs(model_1$residuals))
plot(model_1$fitted.values, sqrt_abs_resid,
     xlab = "Fitted values",
     ylab = "Square root of standardized residuals",
     main = "Scale-Location Plot")

avPlots(model_1)
# Assuming 'model_1' is the linear regression model
residuals_1 <- residuals(model_1)

# Plot histogram with density curve using ggplot2
ggplot(train_data, aes(x = residuals_1)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", lwd=2.5) +
  labs(title = "Histogram with Density Curve for Model-1", x = "Residuals", y = "Density") +
  theme_minimal()

cooksd_1 <- cooks.distance(model_1)
summary(cooksd_1)

###### Model2 #########
#Log transformation
model_2_fit <- log(AQI.Value)~ Ozone.AQI.Value + NO2.AQI.Value + PM2.5.AQI.Value
model_2 <- lm(model_2_fit, train_data)  


summary(train_data$AQI.Value)
summary(model_2)
#Adjusted R-squared:   0.8612 , 
#Residual standard error: 0.119  on 10192 degrees of freedom

check_model(model_2)
ncvTest(model_2)
#Chisquare = 4852.689, Df = 1, p = < 2.22e-16
check_heteroscedasticity(model_2)

vif(model_2)
durbinWatsonTest(model_2)
#D-W Statistic: 2.012291

plot(model_2$fitted.values, model_2$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")

qqnorm(model_2$residuals)
qqline(model_2$residuals)

sqrt_abs_resid <- sqrt(abs(model_2$residuals))
plot(model_2$fitted.values, sqrt_abs_resid,
     xlab = "Fitted values",
     ylab = "Square root of standardized residuals",
     main = "Scale-Location Plot")

avPlots(model_2)
# Assuming 'model_2' is the linear regression model
residuals_2 <- residuals(model_2)

# Plot histogram with density curve using ggplot2
ggplot(train_data, aes(x = residuals_2)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", lwd=2.5) +
  labs(title = "Histogram with Density Curve for Model-2", x = "Residuals", y = "Density") +
  theme_minimal()

cooksd_2 <- cooks.distance(model_2)
summary(cooksd_2)

###### Model3 #########

model_3 <- lm(sqrt(AQI.Value)~ Ozone.AQI.Value + NO2.AQI.Value + PM2.5.AQI.Value
                           , data = train_data)

summary(model_3)
#Adjusted R-squared:    0.8915 , 
#Residual standard error: 0.3585 on 10192 degrees of freedom
check_model(model_3)
ncvTest(model_3)
#Chisquare = 3290.072, Df = 1, p = < 2.22e-16

check_heteroscedasticity(model_3)

vif(model_3)
durbinWatsonTest(model_3)
#D-W Statistic: 2.010097

plot(model_3$fitted.values, model_3$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")

qqnorm(model_3$residuals)
qqline(model_3$residuals)

sqrt_abs_resid <- sqrt(abs(model_3$residuals))
plot(model_3$fitted.values, sqrt_abs_resid,
     xlab = "Fitted values",
     ylab = "Square root of standardized residuals",
     main = "Scale-Location Plot")

avPlots(model_3)

residuals_3 <- residuals(model_3)


# Plot histogram with density curve using ggplot2
ggplot(train_data, aes(x = residuals_3)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", lwd=2.5) +
  labs(title = "Histogram with Density Curve for Model-3", x = "Residuals", y = "Density") +
  theme_minimal()

cooksd_3 <- cooks.distance(model_3)
summary(cooksd_3)
############# Backward model##########

# Backward elimination
model_4 <- step(model_3, direction = "backward")

# Summary of the final model
summary(model_4)
#Adjusted R-squared:  0.8915
#Residual standard error: 0.3585 on 10192 degrees of freedom

check_model(model_4)
ncvTest(model_4)
#Chisquare = 3290.072, Df = 1, p = < 2.22e-16

vif(model_4)
durbinWatsonTest(model_4)
#D-W Statistic: 2.01009

##########Evaluation###############################3

test_data$AQI.Value<- sqrt(test_data$AQI.Value)
summary(test_data$AQI.Value)

predictions <- predict(model_3, newdata = test_data)

# Assess model performance using metrics (e.g., RMSE, R-squared)
# For example, calculating Root Mean Squared Error (RMSE)
actual_values <- test_data$AQI.Value
rmse <- sqrt(mean((predictions - actual_values)^2))

# Or, calculate R-squared
r_squared <- cor(predictions, actual_values)^2
#Root Mean Squared Error (RMSE): 0.3553542 

# Print evaluation metrics
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")
#R-squared: 0.8932925

#Define your control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

#Define your model
cvmodel_results <- train(AQI.Value ~ ., data = train_data, method = "lm", trControl = ctrl)

#Print the results
print(cvmodel_results)
# RMSE      Rsquared   MAE     
#4.919524  0.8979597  3.789789

##########################################################################################################################################################
################### Create a Random Forest regression model######################################################################################################
##########################################################################################################################################################

#########model1#######
test_data$AQI.Value<- (test_data$AQI.Value)^2
summary(test_data$AQI.Value)

set.seed(456)
# Build a random forest regression model
model_rf_1 <- randomForest(AQI.Value ~ Ozone.AQI.Value + NO2.AQI.Value + PM2.5.AQI.Value, data = train_data)
# Get OOB scores
oob_scores <- model_rf_1$predicted - train_data$AQI.Value
# Set the threshold value
threshold <- 5
# Identify influential observations based on OOB scores
influential_observations <- which(abs(oob_scores) > threshold)
# Print the indices of influential observations
cat("Influential observations: ", influential_observations, "\n")

# View model details
print(model_rf_1)
#Mean of squared residuals:  7.019123
#% Var explained: 97.04


#################### Evaluation#############################
# Predict on the test set
predictions_1 <- predict(model_rf_1, newdata = test_data)


mse <- mean((test_data$AQI.Value - predictions_1)^2)
r_squared <- 1 - (mse / var(test_data$AQI.Value))
mae <- mean(abs(test_data$AQI.Value - predictions_1))
rmse <- sqrt(mse)


# Importance plot 
cat('\nFeature Importance:\n')
importance(model_rf_1) 
# Variable importance plot 
varImpPlot(model_rf_1) 
#          IncNodePurity
#Ozone.AQI.Value     183035.98
#NO2.AQI.Value        23724.48
#PM2.5.AQI.Value    1893780.98

cat("Mean Squared Error:", mse, "\n")
#Mean Squared Error: 7.040406 
cat("R-squared:", r_squared, "\n")
#R-squared: 0.970423
cat("Mean Absolute Error:", mae, "\n")
#Mean Absolute Error: 1.612065 
cat("Root Mean Squared Error:", rmse, "\n")
#Root Mean Squared Error: 2.653376 



# Create residual plots
par(mfrow=c(1, 2)) # Set up a 1x2 grid for plotting
residuals <- resid(model_rf_1)
# Residuals vs. Fitted Values
plot(predict(model_rf_1), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")

# Calculate residuals
residuals <- train_data$AQI.Value - predict(model_rf_1)
head(residuals)
# Create the Q-Q plot
qqnorm(residuals, main = "Normal Q-Q Plot")
qqline(residuals)

# Create a data frame with actual and predicted values
plot_data <- data.frame(Actual = train_data$AQI.Value, Predicted = predict(model_rf_1))

# Create the scatterplot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual AQI Value", y = "Predicted AQI Value") +
  ggtitle("Actual vs. Predicted AQI Values")

# Output to be present as PNG file 
png(file = "randomForestRegression.png") 

# Plot the error vs the number of trees graph 
plot(model_rf_1) 

# Saving the file 
dev.off() 

# number of trees with lowest MSE
which.min(model_rf_1$mse)
## [1] 146

# RMSE of this optimal random forest
sqrt(model_rf_1$mse[which.min(model_rf_1$mse)])
## [1] 2.566516

#####################Model 2 #############################

set.seed(789)
# Build a random forest regression model
model_rf_2 <- randomForest(AQI.Value ~ Ozone.AQI.Value + NO2.AQI.Value + PM2.5.AQI.Value,ntree=600, data = train_data)
# Get OOB scores
oob_scores <- model_rf_2$predicted - train_data$AQI.Value
# Set the threshold value
threshold <- 5
# Identify influential observations based on OOB scores
influential_observations <- which(abs(oob_scores) > threshold)
# Print the indices of influential observations
cat("Influential observations: ", influential_observations, "\n")

# View model details
print(model_rf_2)
#Mean of squared residuals: 6.775712
#% Var explained: 97.14



#################### Evaluation#############################
# Predict on the test set
predictions_2 <- predict(model_rf_2, newdata = test_data)


mse <- mean((test_data$AQI.Value - predictions_2)^2)
r_squared <- 1 - (mse / var(test_data$AQI.Value))
mae <- mean(abs(test_data$AQI.Value - predictions_2))
rmse <- sqrt(mse)


# Importance plot 
cat('\nFeature Importance:\n')
importance(model_rf_2) 
# Variable importance plot 
varImpPlot(model_rf_2) 
#             IncNodePurity
#Ozone.AQI.Value     180453.47
#NO2.AQI.Value        23191.22
#PM2.5.AQI.Value    1903321.48

cat("Mean Squared Error:", mse, "\n")
#Mean Squared Error: 6.744044 
cat("R-squared:", r_squared, "\n")
#R-squared: 0.971668
cat("Mean Absolute Error:", mae, "\n")
#Mean Absolute Error: 1.580779 
cat("Root Mean Squared Error:", rmse, "\n")
#Root Mean Squared Error: 2.59693 



# Create residual plots
par(mfrow=c(1, 2)) # Set up a 1x2 grid for plotting
residuals <- resid(model_rf_2)
# Residuals vs. Fitted Values
plot(predict(model_rf_2), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")

# Calculate residuals
residuals <- train_data$AQI.Value - predict(model_rf_2)
head(residuals)
# Create the Q-Q plot
qqnorm(residuals, main = "Normal Q-Q Plot")
qqline(residuals)

# Create a data frame with actual and predicted values
plot_data <- data.frame(Actual = train_data$AQI.Value, Predicted = predict(model_rf_2))

# Create the scatterplot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual AQI Value", y = "Predicted AQI Value") +
  ggtitle("Actual vs. Predicted AQI Values")

# Output to be present as PNG file 
png(file = "randomForestRegression.png") 

# Plot the error vs the number of trees graph 
plot(model_rf_2) 

# Saving the file 
dev.off() 


#####################Model 3 #############################

set.seed(012)
# Build a random forest regression model
model_rf_3 <- randomForest(AQI.Value ~ Ozone.AQI.Value + NO2.AQI.Value + PM2.5.AQI.Value, ntree=146, data = train_data)
# Get OOB scores
oob_scores <- model_rf_3$predicted - train_data$AQI.Value
# Set the threshold value
threshold <- 5
# Identify influential observations based on OOB scores
influential_observations <- which(abs(oob_scores) > threshold)
# Print the indices of influential observations
cat("Influential observations: ", influential_observations, "\n")

# View model details
print(model_rf_3)
#Mean of squared residuals: 6.422902
#% Var explained: 97.29





#################### Evaluation#############################



# Predict on the test set
predictions_3 <- predict(model_rf_3, newdata = test_data)


mse <- mean((test_data$AQI.Value - predictions_3)^2)
r_squared <- 1 - (mse / var(test_data$AQI.Value))
mae <- mean(abs(test_data$AQI.Value - predictions_3))
rmse <- sqrt(mse)


# Importance plot 
cat('\nFeature Importance:\n')
importance(model_rf_3) 
# Variable importance plot 
varImpPlot(model_rf_3) 
#         IncNodePurity
#Ozone.AQI.Value      179316.4
#NO2.AQI.Value         22264.5
#PM2.5.AQI.Value     1927919.9

cat("Mean Squared Error:", mse, "\n")
#Mean Squared Error: 6.148997 
cat("R-squared:", r_squared, "\n")
#R-squared: 0.9741678
cat("Mean Absolute Error:", mae, "\n")
#Mean Absolute Error: 1.556248 
cat("Root Mean Squared Error:", rmse, "\n")
#Root Mean Squared Error: 2.479717 



# Create residual plots
par(mfrow=c(1, 2)) # Set up a 1x2 grid for plotting
residuals <- resid(model_rf_3)
# Residuals vs. Fitted Values
plot(predict(model_rf_3), residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs. Fitted")

# Calculate residuals
residuals <- train_data$AQI.Value - predict(model_rf_3)
head(residuals)
# Create the Q-Q plot
qqnorm(residuals, main = "Normal Q-Q Plot")
qqline(residuals)

# Create a data frame with actual and predicted values
plot_data <- data.frame(Actual = train_data$AQI.Value, Predicted = predict(model_rf_3))

# Create the scatterplot
ggplot(plot_data, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(x = "Actual AQI Value", y = "Predicted AQI Value") +
  ggtitle("Actual vs. Predicted AQI Values")

# Output to be present as PNG file 
png(file = "randomForestRegression.png") 

# Plot the error vs the number of trees graph 
plot(model_rf_3) 

# Saving the file 
dev.off() 

