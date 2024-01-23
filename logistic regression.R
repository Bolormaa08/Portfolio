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
library(ROSE)


# Load data from a CSV file
cardiac_data <- read.csv("C:/Users/35387/OneDrive - National College of Ireland/NCI data analyst/STATISTIC/TABA/Data set-20231227/cardiac.csv")
# View data 
names(cardiac_data)
str(cardiac_data)

# Descriptive Statistics
summary(cardiac_data)

sapply(cardiac_data, class)

#create histogram for original distribution
hist(cardiac_data$age, col='steelblue', main='Age')
hist(cardiac_data$weight, col='steelblue', main='Weight')
hist(cardiac_data$fitness_score, col='steelblue', main='Fitness score')


boxplot(cardiac_data$age, col='red', main='Boxplot of Age', ylab="Age")
boxplot(cardiac_data$weight, col='red', main='Boxplot of Weight', ylab="Weight")
boxplot(cardiac_data$fitness_score, col='red', main='Boxplot of fitness score', ylab="fitness score")

# Cross-tabulation of 'gender' and 'target_variable'
cross_tab <- table(cardiac_data$gender, cardiac_data$cardiac_condition)
print(cross_tab)

# Visualize distribution using bar plot or pie chart
gender_target_plot <- ggplot(cardiac_data, aes(x = gender, fill = cardiac_condition)) +
  geom_bar(position = "fill", stat = "count") +
  labs(title = "Distribution of Cardiac condition across Gender") +
  theme_minimal()
# Display the plot
print(gender_target_plot)

cardiac_counts <- table(cardiac_data$cardiac_condition)
print(cardiac_counts)
#Absent Present 
#65      35

#####################Balancing######################################

# Apply SMOTE to balance the classes
balanced_data <- ovun.sample(cardiac_condition ~ ., data = cardiac_data, method = "over", p = 0.5, seed = 123)

# Sample the majority class to match the minority class size
minority_class_size <- sum(cardiac_data$cardiac_condition == "Present")

majority_class <- cardiac_data[cardiac_data$cardiac_condition == "Absent", ]
undersampled_majority <- majority_class[sample(nrow(majority_class), minority_class_size), ]

# Combine the balanced dataset
balanced_data <- rbind(undersampled_majority, cardiac_data[cardiac_data$cardiac_condition == "Present", ])

cardiac_counts <- table(balanced_data$cardiac_condition)
print(cardiac_counts)
#Absent Present 
#35      35 

####################Data Cleaning###################################

balanced_data <- balanced_data[, !colnames(balanced_data) %in% c("caseno")]

# Identify missing values in the entire dataset
sum(is.na(balanced_data))

# Check the class of 'gender' column
class(balanced_data$gender)
# If it's not a factor, convert it to a factor
balanced_data$gender <- as.factor(balanced_data$gender)
# Now apply relevel
balanced_data$gender <- relevel(balanced_data$gender, ref = "Male")
contrasts(balanced_data$gender) <- contr.sum(2)  # Effect coding for 2 levels (e.g., Male/Female)
summary(balanced_data)


#Outliers
clean_data <- balanced_data
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


boxplot(clean_data$age, col='red', main='After removing outliers Boxplot of Age', ylab="Age")
boxplot(clean_data$weight, col='red', main='After removing outliers Boxplot of Weight', ylab="Weight")
boxplot(clean_data$fitness_score, col='red', main='After removing outliers Boxplot of fitness score', ylab="fitness score")

####################Correlation matrix########################
# Identify numerical and categorical variables
numerical_vars <- sapply(clean_data, is.numeric)
categorical_vars <- setdiff(names(clean_data), names(clean_data)[numerical_vars])

# Calculate correlations for numerical variables
numerical_correlations <- cor(clean_data[, numerical_vars])
print(numerical_correlations)

#Create a heatmap
corrplot::corrplot(numerical_correlations, method = "number")
summary(clean_data)

# Check for zero variance variables
apply(clean_data[, c('age', 'weight', 'fitness_score')], 2, var)

#Data Transformation
#Standardize numeric features for consistency
numeric_features <- c("age","weight", "fitness_score")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


#Apply min-max scaling function to relevant columns
normalized_data <- lapply(clean_data[numeric_features], min_max_transform)
clean_data[numeric_features] <- normalized_data

summary(clean_data)

# Check the structure to ensure the conversion
str(clean_data$cardiac_condition)
clean_data$cardiac_condition <- as.factor(clean_data$cardiac_condition)
summary(clean_data)
###########################################################################################
################################ Logistic regression#######################################
###########################################################################################
set.seed(23176725)
partition <- createDataPartition(y = clean_data$cardiac_condition, p = 0.8, list = FALSE)
train_data <- clean_data[partition, ]
test_data <- clean_data[-partition, ]

# Build logistic regression model
log_model <- glm(cardiac_condition ~ ., data = train_data, family = "binomial")

print(summary(log_model))
#AIC: 62.937

#Assumption Checks
# Check for multicollinearity
vif_results <- vif(log_model)
print(vif_results)
#  age        weight        gender fitness_score 
#1.159838      1.966979      2.772866      2.632635 
# Predict on test data
predicted_probs <- predict(log_model, newdata = test_data, type = "response")

# Confusion Matrix
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) # Assuming a threshold of 0.5

conf_matrix <- table(Actual = test_data$cardiac_condition, Predicted = predicted_classes)
print(conf_matrix)
#            Predicted
#Actual    0 1
#Absent  6 0
#Present 2 4

# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)
#0.8333333

# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(precision)
#1

# Recall (Sensitivity)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(recall)
#0.6666667

# F1-score
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
#0.8

# ROC Curve and AUC
roc_obj <- roc(test_data$cardiac_condition, predicted_probs)
auc_result <- auc(roc_obj)
print(paste("AUC:", auc_result))
#"AUC: 0.944444444444444"

#Plot ROC curve
plot(roc_obj, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)
text(0.8, 0.2, paste("AUC =", round(auc(roc_obj), 4)), col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 4)), col = "blue", lty = 1, cex = 0.8)

# Information Criteria
AIC_value <- AIC(log_model)
BIC_value <- BIC(log_model)
print(paste("AIC:", AIC_value, "BIC:", BIC_value))
# "AIC: 62.9371713829256 BIC: 72.973837309088"

set.seed(23176725)
# Cross-Validation
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
cv_log_model <- train(cardiac_condition ~ ., data = train_data, method = "glm", trControl = fitControl, family = "binomial")
print(cv_log_model)
#Accuracy  Kappa
#0.705     0.4

# Assuming log_model is your fitted model
plot(log_model, which = 1)  # Partial residual plots

# Check for autocorrelation in residuals
acf(residuals(log_model))

# Assuming log_model is your fitted model
plot(log_model, which = 3)  # Residuals vs Fitted plot


############### Backward model######################################
backwards <- step(log_model, direction = "backward")
#Start:  AIC=62.94
#cardiac_condition ~ age + weight + gender + fitness_score

#Df Deviance    AIC
#- weight         1   53.888 61.888
#<none>               52.937 62.937
# fitness_score  1   58.922 66.922
#- gender         1   59.742 67.742
#- age            1   65.336 73.336

#Step:  AIC=61.89
#cardiac_condition ~ age + gender + fitness_score

#Df Deviance    AIC
#<none>               53.888 61.888
#- fitness_score  1   59.685 65.685
#- gender         1   60.897 66.897
#- age            1   66.012 72.012

summary(backwards)

backward2 <- glm(cardiac_condition ~ age + gender + fitness_score, data = train_data, family = "binomial")
summary(backward2)
#AIC: 61.888
#######################Forward model###############################
forwards <- step(log_model, direction = "forward")
summary(forwards)
#Start:  AIC=62.94

###################################################################

#Assumption Checks
# Check for multicollinearity
vif_results <- vif(backward2)
print(vif_results)
#   age        gender      fitness_score 
#1.097408      1.549571      1.576085 
# Predict on test data
predicted_probs <- predict(backward2, newdata = test_data, type = "response")

# Confusion Matrix
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0) # Assuming a threshold of 0.5

conf_matrix <- table(Actual = test_data$cardiac_condition, Predicted = predicted_classes)
print(conf_matrix)
#    Predicted
#Actual    0 1
#Absent  5 1
#Present 2 4
# Calculate accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)
#0.75

# Precision
precision <- conf_matrix[2, 2] / sum(conf_matrix[, 2])
print(precision)
#0.8

# Recall (Sensitivity)
recall <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
print(recall)
#0.6666667

# F1-score
f1_score <- 2 * precision * recall / (precision + recall)
print(f1_score)
#0.7272727

# ROC Curve and AUC
roc_obj <- roc(test_data$cardiac_condition, predicted_probs)
auc_result <- auc(roc_obj)
print(paste("AUC:", auc_result))
#"AUC: 0.916666666666667"

#1. Plot ROC curve
plot(roc_obj, main = "ROC Curve - Logistic Regression", col = "blue", lwd = 2)
text(0.8, 0.2, paste("AUC =", round(auc(roc_obj), 4)), col = "blue")
legend("bottomright", legend = paste("AUC =", round(auc(roc_obj), 4)), col = "blue", lty = 1, cex = 0.8)


#2. Calibration curve
# Creating a formula with the predicted probabilities and the actual outcomes
calib_formula <- as.formula(paste("cardiac_condition~ ", predicted_probs))
# Creating a calibration object
calib_obj <- calibration(calib_formula, data = test_data, bw = TRUE)
# Plotting the calibration curve
plot(calib_obj, main = "Calibration Curve")



# 3. Create partial effect plots for logistic regression
partial_plot <- ggpredict(backward2, terms = "age")
plot(partial_plot)

partial_plot <- ggpredict(backward2, terms = "fitness_score")
plot(partial_plot)

partial_plot <- ggpredict(backward2, terms = "gender")
plot(partial_plot)

# 4. Leverage-Residual Plot
influencePlot(backward2)

# 5. Cook's Distance Plot
cooksd <- cooks.distance(backward2)
plot(cooksd, pch = "24", main = "Cook's Distance Plot")
abline(h = 4/length(backward2$residuals), col = "red")

# 6. Added Variable Plot (for individual predictors)
avPlots(backward2)
# Residual Analysis (if applicable)
residuals <- residuals(backward2)


# Information Criteria
AIC_value <- AIC(backward2)
BIC_value <- BIC(backward2)
print(paste("AIC:", AIC_value, "BIC:", BIC_value))
# AIC: 61.8878936519089 BIC: 69.9172263928387"

set.seed(23176725)
# Cross-Validation
fitControl <- trainControl(method = "cv", number = 10, verboseIter = TRUE)
cv_backward2 <- train(cardiac_condition ~ ., data = train_data, method = "glm", trControl = fitControl, family = "binomial")
print(cv_backward2)
#Accuracy  Kappa
#0.705     0.4


# Visualizing the model
# Example of plotting predicted probabilities against predictor_variable
ggplot(clean_data, aes(x = age, y = cardiac_condition)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
ggplot(clean_data, aes(x = weight, y = cardiac_condition)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
ggplot(clean_data, aes(x = gender, y = cardiac_condition)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)
ggplot(clean_data, aes(x = fitness_score, y = cardiac_condition)) +
  geom_point() +
  stat_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE)

# Assuming backward2 is your fitted model
plot(backward2, which = 1)  # Partial residual plots

# Check for autocorrelation in residuals
acf(residuals(backward2))

# Assuming backward2 is your fitted model
plot(backward2, which = 3)  # Residuals vs Fitted plot

