install.packages("lubridate")


library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(forecast) # For time series analysis
library(tseries)  # For stationarity tests
library(fpp2)
library(forecast)
library(Metrics)


# Load data from a CSV file
setwd("C:\\Users\\35387\\OneDrive - National College of Ireland\\NCI data analyst\\STATISTIC\\TABA\\x23176725") #Set the working directory

#Read the dataset
weather_data <- read.csv("weather_revised.csv", header = TRUE, na.string=c(""), stringsAsFactors = T)
summary(weather_data)
length(weather_data)

# View data 
names(weather_data)
summary(weather_data$gmin.Grass.Minimum.Temperature...degrees.C.)


#Convert date column to date format
weather_data$date <- dmy(weather_data$date)
summary(weather_data$date)
str(weather_data$date)

#Verify date is sitable format
print(min(weather_data$date))
print(max(weather_data$date))
summary(weather_data)

hist(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., col='steelblue', main='Grass minimum temperature')
boxplot(weather_data$gmin.Grass.Minimum.Temperature...degrees.C, col='red', main='Grass minimum temperature')

# Plotting the time series
plot(ts, xlab='Year', ylab='Grass minimum temperature', main='Time Series Plot')

####################Data Cleaning###################################
# Identify missing values in the entire dataset
sum(is.na(weather_data))

#Identify missing values for all columns and store the result in a data frame
missing_data <- data.frame(
  Column = names(weather_data),
  Missing_Values = sapply(weather_data, function(x) sum(is.na(x))))
print(missing_data)
# Remove rows with missing values
weather_data <- na.omit(weather_data)
# Remove duplicates
weather_data <- unique(weather_data) 
summary(weather_data)


#Outliers
# Calculate Z-Score
clean_data<-weather_data
z_scores <- scale(weather_data$gmin.Grass.Minimum.Temperature...degrees.C.)
# Identify outliers based on threshold (e.g., >3 or <-3)
outliers_z_score <- which(abs(z_scores) > 3)
print(outliers_z_score)

# Calculate IQR
Q1 <- quantile(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., 0.25)
Q3 <- quantile(weather_data$gmin.Grass.Minimum.Temperature...degrees.C., 0.75)
IQR <- Q3 - Q1

# Identify outliers using IQR method
outliers_iqr <- which(weather_data$gmin.Grass.Minimum.Temperature...degrees.C. < (Q1 - 1.5 * IQR) | 
                        weather_data$gmin.Grass.Minimum.Temperature...degrees.C. > (Q3 + 1.5 * IQR))


# Remove outliers from weather_data using negative indexing
clean_data <- weather_data[-outliers_iqr, ]
summary(clean_data)
num_rows <- nrow(clean_data)
print(num_rows)

boxplot(clean_data$gmin.Grass.Minimum.Temperature...degrees.C, col='red', main='After removing outliers Grass minimum temperature')

# Remove duplicates
clean_data <- distinct(clean_data)


summary(clean_data)


##########################Visualizations###############################

#Store data in time-series object
ts <- ts(clean_data$gmin.Grass.Minimum.Temperature...degrees.C., 
         start = c(1942, 01),
         end=c(2023,10),
         frequency = 12)
summary(ts)
class(ts)
length(ts)


ggseasonplot(ts, year.labels = TRUE, year.labels.left = TRUE)+
  ylab("Grass minimum temperature") +
  ggtitle("Seasonal plot: Grass minimum temperature monthly")
ggsubseriesplot(ts) + 
  ylab("Grass minimum temperature") +
  ggtitle("Grass minimum temperature monthly")

#Decomposition:
decomposed <- decompose(ts)
plot(decomposed)

#Summary Statistics:
summary(ts)
hist(ts, breaks = 20, main = "Histogram of Time Series Data", xlab = "Value")

# Autocorrelation
acf(ts, main = "Autocorrelation Function")

# Seasonal Decomposition of a Time Series using stl()
stl_decomposed <- stl(ts, s.window = "periodic")
plot(stl_decomposed, main = "STL Decomposition Plot")

# Seasonal decomposition using decompose() - additive
fit.decadd <- decompose(ts, type = "additive")
plot(fit.decadd)




##########################################################################################
#################Simple Time series model#################################################
##########################################################################################

#Subsetting the series with the window function
train_data <- window(ts, start = c(2019, 1), end = c(2022, 12))
test_data <- window(ts, start=c(2023, 1), end=c(2023, 10))
autoplot(train_data)
autoplot(test_data)

length(train_data)
length(test_data)

####### Simple moving average###########################
# Generate a moving average with different window sizes
ma_5 <- ma(train_data, order = 5)
ma_10 <- ma(train_data, order = 10)
ma_20 <- ma(train_data, order = 20)

# Create an empty plot to set up the axes and labels
plot(train_data, type = "n", main = "Smoothed Series Comparison", xlab = "Time", ylab = "Value")
# Plot smoothed series with increased line width
lines(ma_5, col = "red", lwd = 3)  # Adjust 'lwd' for wider line
lines(ma_10, col = "black", lwd = 3)  # Adjust 'lwd' for wider line
lines(ma_20, col = "green", lwd = 3)  # Adjust 'lwd' for wider line
# Add legend
legend("topright", legend = c("MA(5)", "MA(10)", "MA(20)"), col = c("red", "black", "green"), lty = 1)


autoplot(train_data) +
  autolayer(ma(train_data, 5), series = "MA(5)", size = 1.5) +
  autolayer(ma(train_data, 10), series = "MA(10)", size = 1.5) +
  autolayer(ma(train_data, 20), series = "MA(20)", size = 1.5)

ggtsdisplay(train_data)

#Fitting and comparing some simple time series models
#Comparing with 'Basic' models
#### Model 1: Average Method
fcast.mean<-meanf(train_data,h=7)
length(fcast.mean)
summary(fcast.mean)
plot(fcast.mean)
#ME              RMSE        MAE          MPE    MAPE     MASE      ACF1
#Training set -2.775558e-16 3.053129 2.461111 -11.65846 29.62345 0.6656649 0.2391976
# Residual analysis 
checkresiduals(fcast.mean)
autoplot(fcast.mean)
autoplot(fcast.mean)+autolayer(fitted(fcast.mean), series = "Fitted")
forecast((fcast.mean),2)

#### Model 2: Naive Method (Random Walk)
fcast.naive<-naive(train_data,h=7)
summary(fcast.naive)
plot(fcast.naive)
#                   ME       RMSE      MAE    MPE   MAPE    MASE       ACF1
#Training set -0.1468085 3.737988 3.010638 -10.90369 36.006 0.8142974 -0.2745408
checkresiduals(fcast.naive)
autoplot(fcast.naive)
autoplot(fcast.naive)+autolayer(fitted(fcast.naive), series = "Fitted")
forecast((fcast.naive),2)

### Model 3: Season Naive Method
fcast.seasonalnaive<-snaive(train_data,h=7)
summary(fcast.seasonalnaive)
plot(fcast.seasonalnaive)
#                   ME    RMSE      MAE    MPE   MAPE    MASE      ACF1
#Training set -0.3194444 4.460599 3.697222 -14.16926 41.04109    1 0.3419272
checkresiduals(fcast.seasonalnaive)
autoplot(fcast.seasonalnaive)
autoplot(fcast.seasonalnaive)+autolayer(fitted(fcast.seasonalnaive), series = "Fitted")
forecast((fcast.seasonalnaive),2)

### Model 4: Drift Method
fcast.drift <- rwf(train_data, h = 7, drift = TRUE)
summary(fcast.drift)
plot(fcast.drift)
#                   ME       RMSE      MAE     MPE    MAPE   MASE    ACF1
#Training set 3.784096e-17 3.735104 3.007515 -9.209577 35.65459 0.8134525 -0.2745408
checkresiduals(fcast.drift)
autoplot(fcast.drift)
autoplot(fcast.drift)+autolayer(fitted(fcast.drift), series = "Fitted")
forecast((fcast.drift),2)

####Now use CrossValidation and compare RMSE
train_data %>% tsCV(forecastfunction = rwf, drift = TRUE, h=7)->e
e^2 %>% mean(na.rm= TRUE) %>% sqrt()
#RMSE is 5.677955

############################Evaluate performance on 2023 data
#Forecast using mean model on the 2023 data
fcast.mean_2023 <- meanf(test_data, h = 7)
summary(fcast.mean_2023)
#ME     RMSE      MAE        MPE     MAPE MASE       ACF1
#Training set 4.436555e-17 2.480645 2.08 -13.26969 33.70439  NaN -0.160107


autoplot(fcast.mean_2023)
autoplot(fcast.mean_2023)+autolayer(fitted(fcast.mean_2023), series = "Fitted")
forecast((fcast.mean_2023),2)

#################################################################################
##############Exponential Smoothing##############################################
#################################################################################

#####Model1:   Holt's linear trend
holt1<- ets(train_data, model= "ANN")
holt1
#AIC 298.9745
summary(holt1)
#               ME RMSE  MAE MPE MAPE MASE ACF1
#Training set -0.0009765971 3.053282 2.461475 -11.67029 29.63036 0.6657633 0.23919892
autoplot(holt1)
autoplot(holt1)+autolayer(fitted(holt1), series = "Fitted")
forecast((holt1),2)


#####Model2:
holt2<- ets(train_data, model= "AAN")
holt2
#AIC 301.3425
summary(holt2)
#               ME RMSE  MAE MPE MAPE MASE ACF1
#Training set -0.0556731 3.001815 2.372607 -11.73673 28.72959 0.6417269 0.22817091
autoplot(holt2)
autoplot(holt2)+autolayer(fitted(holt2), series = "Fitted")
forecast((holt2),2)

#####Model3:
holt3<- ets(train_data, model= "AAA")
holt3
#AIC 314.33
summary(holt3)
#               ME RMSE  MAE MPE MAPE MASE ACF1
#Training set -0.006546976 2.676513 2.195246 -8.341091 25.40056 0.5937555 0.2438328
autoplot(holt3)
autoplot(holt3)+autolayer(fitted(holt3), series = "Fitted")
forecast((holt3),2)

#####Model4:
holt4<- holt(train_data, h=7)
holt4
#AIC 301.3424
summary(holt4)
#               ME RMSE  MAE MPE MAPE MASE ACF1
#Training set -0.05585018 3.001811 2.372595 -11.73877 28.72999 0.6417236 0.2281692
autoplot(holt4)
autoplot(holt4)+autolayer(fitted(holt4), series = "Fitted")
forecast((holt4),2)

window(train_data)%>%
  holt(damped = TRUE, h=7, PI=FALSE)%>%
  autoplot()

#####Model5:
holt5<- ets(train_data, model= "ZZZ")
holt5
#ETS(M,N,N) 
#AIC 298.9744
summary(holt5)
#               ME RMSE  MAE MPE MAPE MASE ACF1
#Training set0.0005991618 3.053282 2.461213 -11.65223 29.62288 0.6656924 0.239199
autoplot(holt5)
autoplot(holt5)+autolayer(fitted(holt5), series = "Fitted")
forecast((holt5),2)

####Model 6:  Mean annual
mean<- ses(train_data, h=7)
summary(mean)
#AIC 298.9745
#                     ME      RMSE    MAE     MPE    MAPE    ASE      ACF1
#Training set -0.0009765971 3.053282 2.461475 -11.67029 29.63036 0.6657633 0.2391989
autoplot(mean)
autoplot(mean)+autolayer(fitted(mean), series = "Fitted")
forecast((mean),2)

######Model7: Holt-winter seasonal method
fit1 <- hw(train_data, seasonal = "additive")
fit2 <- hw(train_data, seasonal = "multiplicative")
autoplot(train_data) +
  autolayer(fit1, series = "HW additive forecasts", PI = FALSE) + 
  autolayer(fit2, series = "HW multiplicative forecasts", PI = FALSE) +
  xlab("Year") +
  ylab("Grass minimum temperature") +
  ggtitle("Grass minimum temperature") +
  guides(colour = guide_legend(title = "Forecast"))
summary(fit1)
#AIC: 314.3311
#ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.006546976 2.676513 2.195246 -8.341091 25.40056 0.5937555 0.2438328
summary(fit2)
#AIC 311.5881 
#                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.02523336 2.627484 2.166948 -8.695745 25.14455 0.5861015 0.2525877


#######Evaluation on 2023 dataset
#Forecast using ANN, ZZZ, mean model on the 2023 data
#ANN
fcast.holt1_2023 <-ets(test_data, model= "ANN")
summary(fcast.holt1_2023)
#ME     RMSE      MAE        MPE     MAPE MASE       ACF1
#Training set 1.217879e-05 2.480769 2.080072 -13.27012 33.70556  NaN -0.1601051

#ZZZ
fcast.holt4_2023 <-ets(test_data, model= "ZZZ")
summary(fcast.holt4_2023)
#ME     RMSE      MAE        MPE     MAPE MASE       ACF1
#Training set 1.217879e-05 2.480769 2.080072 -13.27012 33.70556  NaN -0.1601051

#mean
fcast.mean_2023 <-ses(test_data, h=7)
summary(fcast.mean_2023)
#ME     RMSE      MAE        MPE     MAPE MASE       ACF1
#Training set -0.000156166 2.480769 2.080072 -13.27282 33.70636  NaN -0.1601051

ses(train_data)
holt(train_data)
hw(train_data)


autoplot(fcast.holt1_2023)
autoplot(fcast.holt1_2023)+autolayer(fitted(fcast.holt1_2023), series = "Fitted")
forecast((fcast.holt1_2023),2)

#########################################################################
############# Arima/Sarima ################################################
#########################################################################
#Visual inspection
ggplot(clean_data, aes(date, gmin.Grass.Minimum.Temperature...degrees.C.)) +
  geom_line() +
  labs(title = "Time Series Data", x = "Date", y = "grass minimum temperature")

# Determine the number of differences needed for stationarity
d <- nsdiffs(train_data)
print(d)

ggtsdisplay (train_data, main="Train data")

# Check for stationarity
# Perform Dickey-Fuller test
adf.test(train_data)
#Dickey-Fuller = -5.2605, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

#ACF/PACF plots. Choosing p and q
acf(train_data)
acf(train_data, lag.max = 5)

Pacf(train_data)
Pacf(train_data, lag.max = 5)

###############The PACF plot indicates that the most suitable model is AR(1).
#Model 1: order(1,0,0)
fit100<-Arima(train_data,order=c(1,0,0))
summary(fit100)
#AIC=246.44
#ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
#Training set -0.0177729 2.959413 2.418511 -11.26205 29.11246 0.6541428 0.03327146

#Model 2: order(0,0,1)
fit001<-Arima(train_data,order=c(0,0,1))
summary(fit001)
#AIC=246.19
#ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.01213682 2.951642 2.383968 -11.19933 28.77595 0.6447998 0.0111627

#Model 3: order(0,0,0)
fit000<-Arima(train_data,order=c(0,0,0))
summary(fit000)
#AIC=247.37
#ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -2.775558e-16 3.053129 2.461111 -11.65846 29.62345 0.6656649 0.2391976

#Model 4: order(1,0,1)
fit101<-Arima(train_data,order=c(1,0,1))
summary(fit101)
#AIC=AIC=248.15
# ME     RMSE     MAE       MPE     MAPE      MASE        ACF1
#Training set -0.01422834 2.950225 2.39287 -11.17987 28.85218 0.6472075 0.004218034


############Diagnostic
checkresiduals(fit001$residuals)
#	Ljung-Box test
#data:  Residuals
#Q* = 13.296, df = 10, p-value = 0.2076
#Model df: 0.   Total lags used: 10

predictions001<-  forecast::forecast(fit001,h=length(test_data))
autoplot(predictions001)
print(predictions001$mean)

qqnorm(fit001$residuals)
qqline(fit001$residuals)

Box.test(fit001$residuals,type="Ljung-Box")

#Forecast for 
forecast001=forecast(fit001, 7)
print(forecast001)

#########Automatic arima
model_auto_arima=auto.arima(train_data)
summary(model_auto_arima)
#ARIMA(0,0,1) with non-zero mean
#AIC=246.19
#ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
#Training set -0.01213682 2.951642 2.383968 -11.19933 28.77595 0.6447998 0.0111627

qqnorm(model_auto_arima$residuals)
qqline(model_auto_arima$residuals)

Box.test(model_auto_arima$residuals,type="Ljung-Box")

fit001 %>%forecast(h=7) %>% autoplot()
forecast(fit001,h=7)

###########################Evaluation 
evaluate001<-Arima(test_data,order=c(0,0,1))
summary(evaluate001)
#ME     RMSE      MAE       MPE     MAPE MASE         ACF1
#Training set -0.009705334 2.439061 2.123986 -13.27715 34.54006  NaN -0.003528683

# Calculate MAE, RMSE, and MAPE using the Metrics package
mae001 <- round(mae(predictions001$mean, test_data), 3)
rmse001 <- round(rmse(predictions001$mean, test_data), 3)
mape001 <- round(mape(predictions001$mean, test_data), 3)

# Display the rounded metrics values
print(mae001)
#3.019
print(rmse001)
#3.573
print(mape001)
#0.312

