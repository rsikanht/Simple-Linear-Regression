# 2) Delivery_time -> Predict delivery time using sorting time 

dt <- read.csv("delivery_time.csv")
View(dt)
library(psych)
describe(dt)
attach(dt)

# EDA - Exploratory Data Analysis.
qqnorm(Delivery.Time)
qqnorm(log(Sorting.Time))
qqnorm(sqrt(Sorting.Time))
qqline(sqrt(Sorting.Time))
hist(Delivery.Time)
hist(Sorting.Time)
boxplot(Delivery.Time, horizontal = T)
boxplot(Sorting.Time, horizontal = T)
plot(density(Delivery.Time))
plot(density(Sorting.Time)
     
# Scatter Plot
plot(Delivery.Time, Sorting.Time)
# Graph shows moderate positive correlation between both the variables
     
cor(Delivery.Time, Sorting.Time)
# Correlation value is 0.83 which shows strong correlation between the variables
     
# Regression check
reg_DT <- lm(Delivery.Time ~ Sorting.Time)
summary(reg_DT) 
# R^2 value is 0.68 which is a good fit model.
#We can write eq. as SH=6.58+1.65*Sorting.Time
     
# Check fitted values of predicted
reg_DT$fitted.values 
     
# check for associated errors
reg_DT$residuals
sum(reg_DT$residuals)
     
# Confidence interval for 95%
confint(reg_DT, level = 0.95)
     
# List down the predicted values along with lsl and usl
predict(reg_DT, interval = "predict") # See predicted values with confidence interval.

# Calculation on Y = B0 + B1*x for prediction check
6.58+1.64*10
6.58+1.64*6

     
# From the provided data set, the correlation value is 0.83 and hence both variables are strong and positively correlated.
# Co-ef. are significant and co-ef.of Determination value. i.e R^2 is 0.68 which is good.
# sum of errors is 9.992007e-16 i.e approx 0 and errors are normally distributed.
# Hence model is best fit and Sorting time is an appropriate input to determine delivery time.
     