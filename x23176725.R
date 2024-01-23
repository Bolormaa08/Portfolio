## Here's the data
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
library(stats)

# Load data from a CSV file
stat_data <- read.csv("C:/Users/35387/OneDrive - National College of Ireland/NCI data analyst/STATISTIC/CA1/housing.csv", header = TRUE, stringsAsFactors = T)

# View data 
names(stat_data)
str(stat_data)

# Mean, SD, Median, Min, Max, Skewness, and Kurtosis for y
psych::describe(stat_data$Sale_Price)
psych::describe(stat_data$Lot_Frontage)
psych::describe(stat_data$Year_Built)
psych::describe(stat_data$Total_Bsmt_SF)
psych::describe(stat_data$First_Flr_SF)
psych::describe(stat_data$Second_Flr_SF)
psych::describe(stat_data$Full_Bath)
psych::describe(stat_data$Bedroom_AbvGr)
psych::describe(stat_data$Kitchen_AbvGr)
psych::describe(stat_data$Fireplaces)
psych::describe(stat_data$Longitude)
psych::describe(stat_data$Latitude)

# Descriptive Statistics
summary(stat_data)

# Levels of Measurement
sapply(stat_data, class)



#create histogram for original distribution
hist(stat_data$Sale_Price, col='steelblue', main='The sale price of the house')
hist(stat_data$Lot_Frontage, col='steelblue', main='Lot Frontage')
hist(stat_data$Lot_Area, col='steelblue', main='Lot Area')
hist(stat_data$Year_Built, col='steelblue', main='The year the house was constructed')
hist(stat_data$Total_Bsmt_SF, col='steelblue', main='Total basement area in square feet')
hist(stat_data$First_Flr_SF, col='steelblue', main='Ground floor area in square feet')
hist(stat_data$Second_Flr_SF, col='steelblue', main='First floor area in square feet')
hist(stat_data$Full_Bath, col='steelblue', main='Number of full bathrooms')
hist(stat_data$Half_Bath, col='steelblue', main='Number of half bathrooms')
hist(stat_data$Bedroom_AbvGr, col='steelblue', main=' Number of bedrooms on or above ground floor')
hist(stat_data$Kitchen_AbvGr, col='steelblue', main=' Number of kitches on or above ground floor')
hist(stat_data$Fireplaces, col='steelblue', main='Number of fireplaces')
hist(stat_data$Longitude, col='steelblue', main='Longitude of plot')
hist(stat_data$Latitude, col='steelblue', main='Latitude of plot')


plot(stat_data$Longitude, stat_data$Latitude, 
     xlab = "Longitude", ylab = "Latitude", 
     main = "Locations Scatter Plot")


ggplot(stat_data, aes(x = Longitude, y = Latitude)) +
  geom_density_2d() +
  labs(x = "Longitude", y = "Latitude", title = "Density Heatmap")


#Spatial Clustering:
coordinates <- stat_data[, c("Longitude", "Latitude")]
# You may need to scale the coordinates for K-means
scaled_coordinates <- scale(coordinates)
# Determine the number of clusters (e.g., 5 clusters)
num_clusters <- 5
# Perform K-means clustering
kmeans_result <- kmeans(scaled_coordinates, centers = num_clusters)
# Visualize clusters on a scatter plot
plot(coordinates, col = kmeans_result$cluster, 
     main = "K-means Clustering of Locations")
# Assuming 'kmeans_result' contains the clustering result with 'clean_data'
# 'clean_data' should contain 'Longitude' and 'Latitude' columns
# Assign distinct colors to clusters
cluster_colors <- rainbow(length(unique(kmeans_result$cluster)))
# Visualize clusters on a scatter plot with colors
plot(stat_data$Longitude, stat_data$Latitude, col = cluster_colors[kmeans_result$cluster],
     pch = 19, main = "Spatial Clustering with Colors",
     xlab = "Longitude", ylab = "Latitude", cex = 1.5)
legend("topright", legend = unique(kmeans_result$cluster), fill = cluster_colors)





#Spatial Autocorrelation:
coordinates <- stat_data[, c("Longitude", "Latitude")]
# Create a spatial weights matrix based on k-nearest neighbors
W <- knn2nb(knearneigh(coordinates, k = 5))
# Convert to a listw object
W_listw <- nb2listw(W, style = "W")
# Check the created spatial weights matrix
summary(W_listw)





boxplot(stat_data$Sale_Price, col='red', main='The sale price of the house', ylab="Sale Price")
boxplot(stat_data$Lot_Frontage, col='red', main='Lot Frontage', ylab="Lot Frontage")
boxplot(stat_data$Lot_Area, col='red', main='Lot Area', ylab="Lot Area")
boxplot(stat_data$Year_Built, col='red', main='The year the house was constructed', ylab="Year Built")
boxplot(stat_data$Total_Bsmt_SF, col='red', main='Total basement area in square feet', ylab="total basement area sqrft")
boxplot(stat_data$First_Flr_SF, col='red', main='Ground floor area in square feet', ylab="Ground floor area sqrft")
boxplot(stat_data$Second_Flr_SF, col='red', main='First floor area in square feet', ylab="First floor area sqrft")
boxplot(stat_data$Full_Bath, col='red', main='Number of full bathrooms', ylab="Full Bathrooms")
boxplot(stat_data$Half_Bath, col='red', main='Number of half bathrooms', ylab="Half bathrooms")
boxplot(stat_data$Bedroom_AbvGr, col='red', main=' Number of bedrooms on or above ground floor', ylab="Bedrooms")
boxplot(stat_data$Kitchen_AbvGr, col='red', main=' Number of kitches on or above ground floor', ylab="Kitchens")
boxplot(stat_data$Fireplaces, col='red', main='Number of fireplaces', ylab="Fireplaces")
boxplot(stat_data$Longitude, col='red', main='Longitude of plot', ylab="Longtitude")
boxplot(stat_data$Latitude, col='red', main='Latitude of plot', ylab="Latitude")

####################Data Cleaning###################################
# Identify missing values in the entire dataset
sum(is.na(stat_data))

#Identify missing values for all columns and store the result in a data frame
missing_data <- data.frame(
  Column = names(stat_data),
  Missing_Values = sapply(stat_data, function(x) sum(is.na(x))))
print(missing_data)
# Remove rows with missing values
stat_data <- na.omit(stat_data)
# Remove duplicates
stat_data <- unique(stat_data) 
summary(stat_data)
#Create a heatmap or bar plot to visualize the extent and pattern of missing values
vis_miss(stat_data)





# Columns to encode
cols_to_encode <- c("Bldg_Type", "House_Style", "Overall_Cond", 'Exter_Cond' )
# Use model.matrix to create dummy variables (one-hot encoding)
encoded_df <- cbind(stat_data[, !grepl("Bldg_Type|House_Style|Overall_Cond|Exter_Cond", names(stat_data))],
                model.matrix(~ Bldg_Type + House_Style + Overall_Cond+ Exter_Cond - 1, data = stat_data))
print(encoded_df)
sapply(encoded_df, class)
summary(encoded_df)
 sapply(stat_data, class)

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


boxplot(clean_data$Sale_Price, col='red', main='After removing outliers the sale price', ylab="Sale Price")
boxplot(clean_data$Lot_Frontage, col='red', main='After removing outliers Lot Frontage', ylab="Lot Frontage")
boxplot(clean_data$Lot_Area, col='red', main='After removing outliers Lot Area', ylab="Lot Area")
boxplot(clean_data$Year_Built, col='red', main='After removing outliers 
        year the house was constructed', ylab="Year Built")
boxplot(clean_data$Total_Bsmt_SF, col='red', main='After removing outliers Total basement area in square feetl', ylab="total basement area sqrft")
boxplot(clean_data$First_Flr_SF, col='red', main='After removing outliers Ground floor area in square feet', ylab="Ground floor area sqrft")
boxplot(clean_data$Second_Flr_SF, col='red', main='After removing outliers First floor area in square feet', ylab="First floor area sqrft")
boxplot(clean_data$Full_Bath, col='red', main='After removing outliers Number of full bathrooms', ylab="Full Bathrooms")
boxplot(clean_data$Half_Bath, col='red', main='After removing outliers Number of half bathrooms', ylab="Half bathrooms")
boxplot(clean_data$Bedroom_AbvGr, col='red', main=' After removing outliers Number of bedrooms on or above ground floor', ylab="Bedrooms")
boxplot(clean_data$Kitchen_AbvGr, col='red', main=' After removing outliers Number of kitches on or above ground floor', ylab="Kitchens")
boxplot(clean_data$Fireplaces, col='red', main='After removing outliers Number of fireplaces', ylab="Fireplaces")
boxplot(clean_data$Longitude, col='red', main='After removing outliers Longitude of plot', ylab="Longtitude")
boxplot(clean_data$Latitude, col='red', main='After removing outliers Latitude of plot', ylab="Latitude")



####################Correlation matrix########################
clean_data <- clean_data[, !colnames(clean_data) %in% c("Exter_CondPoor","Exter_CondTypical","Overall_CondGood", "Overall_CondPoor",
                                                        "Overall_CondVery_Good" ,"Overall_CondVery_Poor", "Exter_CondFair", "Exter_CondGood",
                                                        "Overall_CondBelow_Average" ,"Overall_CondExcellent", "Overall_CondFair",
                                                        "House_StyleSFoyer", "House_StyleSLvl", "House_StyleTwo_and_Half_Fin", "House_StyleTwo_and_Half_Unf",
                                                        " Bldg_TypeDuplex", "Bldg_TypeOneFam", "Bldg_TypeTwnhs", "Bldg_TypeTwnhsE" ,"Bldg_TypeTwoFmCon", "House_StyleOne_and_Half_Unf",
                                                        "Kitchen_AbvGr","Bldg_TypeDuplex")]
summary(clean_data)
binary_numeric_vars <- sapply(clean_data, function(x) is.numeric(x) && length(unique(x)) == 2)
binary_numeric_vars_names <- names(binary_numeric_vars[binary_numeric_vars])

# Display the names of binary numeric variables
print(binary_numeric_vars_names)
summary(clean_data)

# Exclude binary variables from the correlation calculation
correlation_matrix_exc <- cor(clean_data[, !binary_numeric_vars])
print(correlation_matrix_exc)



# Identify numerical and categorical variables
numerical_vars <- sapply(clean_data, is.numeric)
categorical_vars <- setdiff(names(clean_data), names(clean_data)[numerical_vars])


# Calculate correlations for numerical variables
numerical_correlations <- cor(clean_data[, numerical_vars])
print(numerical_correlations)

#Create a heatmap
corrplot::corrplot(numerical_correlations, method = "number")
summary(clean_data)

#Data Transformation
#Standardize numeric features for consistency
numeric_features <- c("Lot_Frontage","Lot_Area", "Year_Built", "Total_Bsmt_SF",
                      "First_Flr_SF", "Second_Flr_SF", "Full_Bath", "Half_Bath",
                      "Bedroom_AbvGr", "Fireplaces", "Longitude", "Latitude", 
                      "House_StyleOne_Story", "House_StyleTwo_Story", "Overall_CondAverage")

#Normalize the data using min-max normalization
min_max_transform <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

#Apply min-max scaling function to relevant columns
clean_data[numeric_features] <- lapply(clean_data[numeric_features], min_max_transform)


summary(clean_data)



####################### Build a model##############################
### Model1 ###
set.seed(23176725)
partition <- createDataPartition(y = clean_data$Sale_Price, p = 0.8, list = FALSE)
train_data <- clean_data[partition, ]
test_data <- clean_data[-partition, ]


model_1 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area+          
                         Year_Built + Total_Bsmt_SF+      
                         First_Flr_SF + Second_Flr_SF+      
                         Full_Bath + Half_Bath+        
                         Bedroom_AbvGr + Fireplaces+       
                         Longitude + Latitude+           
                          House_StyleOne_Story+
                          House_StyleTwo_Story + Overall_CondAverage
                        , data = train_data)

summary(model_1)
#Adjusted R-squared:  0.857, Residual standard error: 19650 on 780 degrees of freedom
check_model(model_1)
ncvTest(model_1)
check_heteroscedasticity(model_1)
#Chisquare = 47.5007, Df = 1, p = 5.4984e-12

vif(model_1)
check_collinearity(model_1)

durbinWatsonTest(model_1)
check_autocorrelation(model_1)
#D-W Statistic: 1.570801

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
train_data$Sale_Price <- log(train_data$Sale_Price)
summary(train_data$Sale_Price)
model_2 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area+          
                Year_Built + Total_Bsmt_SF+      
                First_Flr_SF + Second_Flr_SF+      
                Full_Bath + Half_Bath+        
                Bedroom_AbvGr + Fireplaces+       
                Longitude + Latitude+           
                House_StyleOne_Story+
                House_StyleTwo_Story + Overall_CondAverage
              , data = train_data)
summary(train_data)

summary(model_2)
#Adjusted R-squared:   0.8812 , 
#Residual standard error: 0.1059 on 780 degrees of freedom

check_model(model_2)
ncvTest(model_2)
#Chisquare = 30.83635, Df = 1, p = 2.8073e-08
vif(model_2)
durbinWatsonTest(model_2)
#D-W Statistic: 1.660495

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

model_3 <- lm(Sale_Price ~ Lot_Frontage + Lot_Area+          
                Year_Built + Full_Bath + Half_Bath+        
                Bedroom_AbvGr + Fireplaces+       
                Longitude + Latitude+Overall_CondAverage
              , data = train_data)

summary(model_3)
#Adjusted R-squared:   0.7595 , 
#Residual standard error: 0.1507  on 785 degrees of freedom
check_model(model_3)
ncvTest(model_3)
#Chisquare = 12.57032, Df = 1, p = 0.00039192

vif(model_3)
durbinWatsonTest(model_3)
#D-W Statistic: 1.591189

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

#############Model 4##########

# Backward elimination
model_4 <- step(model_3, direction = "backward")

# Summary of the final model
summary(model_4)
#Adjusted R-squared:   0.76 
#Residual standard error: 0.1506 on 787 degrees of freedom

check_model(model_4)
ncvTest(model_4)
#Chisquare = 12.62043, Df = 1, p = 0.00038155

vif(model_4)
durbinWatsonTest(model_4)
#D-W Statistic: 1.58938

plot(model_4$fitted.values, model_4$residuals,
     xlab = "Fitted values",
     ylab = "Residuals",
     main = "Residuals vs. Fitted")

qqnorm(model_4$residuals)
qqline(model_4$residuals)

sqrt_abs_resid <- sqrt(abs(model_4$residuals))
plot(model_4$fitted.values, sqrt_abs_resid,
     xlab = "Fitted values",
     ylab = "Square root of standardized residuals",
     main = "Scale-Location Plot")

avPlots(model_4)

# Assuming 'model_2' is the linear regression model
residuals_4 <- residuals(model_4)


# Plot histogram with density curve using ggplot2
ggplot(train_data, aes(x = residuals_3)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "black") +
  geom_density(color = "red", lwd=2.5) +
  labs(title = "Histogram with Density Curve for Model-3", x = "Residuals", y = "Density") +
  theme_minimal()

cooksd_3 <- cooks.distance(model_4)
summary(cooksd_3)


########Evaluation######################

test_data$Sale_Price<- log(test_data$Sale_Price)
# Predict Sale_Price using the model on the test data
predictions <- predict(model_4, newdata = test_data)
summary(predictions)

plot(predictions, test_data$Sale_Price, xlab = "Predicted Values", ylab = "Observed Values",
     main = "Observed vs. Predicted Values")
abline(0, 1, col = "red", lty = 2)  # Adding a diagonal line for reference


# Assess model performance using metrics (e.g., RMSE, R-squared)
# For example, calculating Root Mean Squared Error (RMSE)
actual_values <- test_data$Sale_Price
rmse <- sqrt(mean((predictions - actual_values)^2))

# Or, calculate R-squared
r_squared <- cor(predictions, actual_values)^2

# Print evaluation metrics
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared:", r_squared, "\n")
#R-squared: 0.7697999 

#Define your control parameters for cross-validation
ctrl <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

#Define your model
cvmodel_results <- train(log(Sale_Price) ~ ., data = train_data, method = "lm", trControl = ctrl)
summary(cvmodel_results)

#Print the results
print(cvmodel_results)
# RMSE         Rsquared   MAE        
#0.009003949  0.8789365  0.006680532

