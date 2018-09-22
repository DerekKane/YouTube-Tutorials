# Price Elasticity and optimiziation in Regression


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set a Working Directory
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

setwd("C:/Users/Derek/Documents/RPackages/Regression Tutorial")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

mydata <- read.csv("grapeJuice.csv")
attach(mydata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load the libraries
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(s20x)
library(car)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Basic statistics
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary(mydata)


# We can further explore the distribution 
# of the data of sales by visualizing the data in graphical form as follows.

#set the 1 by 2 layout plot window

par(mfrow = c(1,2))

# boxplot to check if there are outliers
boxplot(mydata$sales,horizontal = TRUE, xlab="sales")

# histogram to explore the data distribution shape

hist(mydata$sales,main="",xlab="sales",prob=T)
lines(density(mydata$sales),lty="dashed",lwd=2.5,col="red")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analysis of Ad Effectiveness
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# The marketing team wants to find out the ad with better effectiveness for sales 
# between the two types of ads, one is with natural production theme; the other is with family health caring theme. So 
# they can place the better one into all of ABC's stores after the pilot period.

# To find out the better ad, we can calculate and compare the mean of sales with 
# the two different ad types at the first step.

#divide the dataset into two sub dataset by ad_type

sales_ad_nature = subset(mydata,ad_type==0)
sales_ad_family = subset(mydata,ad_type==1)

#calculate the mean of sales with different ad_type

mean(sales_ad_nature$sales)
mean(sales_ad_family$sales)

# The mean of sales with nature product theme is about 187; the mean of sales with family
# health caring theme is about 247. It looks like that the latter one is better. 
# However, this is only the conclusion based on the sample with only 30 observations 
# randomly selected. To find out how likely the conclusion is correct for the
# whole population, it is necessary to do statistical testing - two-sample t-test.

#set the 1 by 2 layout plot window
par(mfrow = c(1,2))

# histogram to explore the data distribution shapes
hist(sales_ad_nature$sales,main="",xlab="sales with nature production theme",prob=T)
lines(density(sales_ad_nature$sales),lty="dashed",lwd=2.5,col="red")

hist(sales_ad_family$sales,main="",xlab="sales with family health caring theme",prob=T)
lines(density(sales_ad_family$sales),lty="dashed",lwd=2.5,col="red")

# We can see that the shapes are roughly normally distributed. 
# We can also check the normality by Shapiro-Wilk test as follows.

shapiro.test(sales_ad_nature$sales)
shapiro.test(sales_ad_family$sales)

# The p-values of the Shapiro-Wilk tests are larger than 0.05, so there is no strong evidence to reject 
# the null hypothesis that the two groups of sales data are normally distributed.

# Now we can conduct the t-test since the t-test assumptions are met.

t.test(sales_ad_nature$sales,sales_ad_family$sales)

# From the output of t-test above, we can say that:-

# We have strong evidence to say that the population means of the sales with 
# the two different ad types are different because the p-value of the t-test is very small;

# With 95% confidence, we can estimate that the mean of the sales with natural production 
# theme ad is somewhere in 27 to 93 units less than that of the sales with family health caring theme ad.

# So the conclusion is that the ad with the theme of family health caring is BETTER.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sales Driver Analysis and Price Elasticity Analysis 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# With the information given in the data set, we can explore how grape juice price, 
# ad type, apple juice price, cookies price influence the sales of 
# grape juice in a store by multiple linear regression analysis. Here, "sales" 
# is the dependent variable and the others are independent variables.

pairs(mydata,col="blue",pch=20)
pairs20x(mydata)

# The correlation coefficients between sales and price, ad_type, price_apple, 
# and price_cookies are 0.85, 0.58, 0.37, and 0.37 respectively, 
# that means they all might have some influences to the sales, so we can try 
# to add all of the independent variables into the regression model as follows.

sales.reg <-lm(sales~price+ad_type+price_apple+price_cookies,mydata)

summary(sales.reg)

# The p-value for price, ad_type, and price_cookies in the last column of the 
# above output is much less than 0.05. They are significant in explaining
# the sales. We are confident to include these three variables into the model.

# The p-value of price_apple is a bit larger than 0.05, seems there are no strong evidence for apple juice price to explain the sales. However, according to our real-life 
# experience, we know when apple juice price is lower, consumers likely to buy 
# more apple juice, and then the sales of other fruit juice will decrease. 
# So we can also add it into the model to explain the grape juice sales.

# Please note the R-squared is very high here because the dataset 
# were made up rather than from real world data sources.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OLS Assumption Check
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# plotting the residuals vs. other key model metrics

par(mfrow=c(2,2))
plot(sales.reg)

# The Residuals vs Fitted graph above shows that the residuals scatter around the fitted line with no obvious pattern, and the Normal Q-Q graph shows that
# basically the residuals are normally distributed. The assumptions are met.

#check multicollinearity
vif(sales.reg)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Discussion
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Based on the above analysis, we can accept the
# regression result and construct the multi-linear model of sales as follows.

# Sales = 774.81 - 51.24 * price + 29.74 * ad_type + 22.1 * price_apple - 25.28 * price_cookies

# With model established, we can analysis the Price Elasticity(PE) and Cross-price Elasticity(CPE) to predict the 
# reactions of sales quantity to price. "Price elasticity is defined as %??Q/%??P, 
# which indicates the percent change in quantity divided by the
# percent change in price; Cross-price Elasticity is the percent change 
# in quantity divided by the change in the price of some other product."

# PE = (??Q/Q) / (??P/P) = (??Q/??P) * (P/Q) = -51.24 * 0.045 = -2.3

# P is price, Q is sales quantity

# ??Q/??P = -51.24 , the parameter before the variable "price" in the above model

# P/Q = 9.738 / 216.7 = 0.045,  P is the mean of prices in the dataset, so does Q

# The PE indicates that 10% decrease in price will increase the sales by 23%, and vice verse.

# Let's further calculate the CPE on apple juice and cookies to analyze the how the change 
# of apple juice price and cookies price influence the sales of grape juice.

# CPEapple = (??Q/??Papple) * (Papple/Q) = 22.1 * ( 7.659 / 216.7) = 0.78
# CPEcookies = (??Q/??Pcookies) * (Pcookies/Q) = -25.28 * ( 9.622 / 216.7) = - 1.12

# The CPEapple indicates that 10% decrease in apple juice price will DECREASE the sales by 7.8%, and vice verse. 
# So the grape juice and apple juice are substitutes.

# The CPEcookies indicates that 10% decrease in cookies price will INCREASE the 
# sales by 11.2%, and vice verse. So the grape juice and cookies are compliments. 

# Place the two products together will likely increase the sales for both.

# We can also know that the sales increase 29.74 units 
# when using the ad with the family health caring theme (ad_type = 1).


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optimal Pricing and Sales Prediction
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Usually companies want to get higher profit rather than just higher sales quantity. So, how to 
# set the optimal price for the new grape juice to get the maximum profit based 
# on the dataset collected in the pilot period and the regression model above?

# To simplify the question, we can let the ad_type = 1, the price_apple = 7.659 (mean value), 
# and the price_cookies = 9.738 (mean value).

# The model is simplified as follows:-
  
# Sales = 774.81 - 51.24 * price + 29.74 * 1 + 22.1 * 7.659 - 25.28 * 9.738
# or
# Sales = 772.64 - 51.24*price

# Assume the marginal cost(C) per unit of grape juice is 5. 
# We can calculate the profit (Y) by the following formula.

# Y = (price - C) * Sales Quantity = (price - 5) * (772.64 - 51.24*price)

# Y = - 51.24 * price2 + 1028.84 * price - 3863.2

# To get the optimal price to maximize Y, we can use the following R function.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Optimization Function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

f <- function(x) {
  -51.24*x^2 + 1028.84*x - 3863.2}

optimize(f,lower=0,upper=20,maximum=TRUE)

# The optimal price is 10.04; the maximum profit will be 1301 according to the above output. 
# In reality, we can reasonably set the price to be 10 or 9.99.

# We can further use the model to predict the sales while the price is 10.

# predict the sales qty

inputData <- data.frame(price=10,ad_type=1,price_apple=7.659,price_cookies=9.738)

predict(sales.reg,inputData,interval="p")

# The sales forecast will be 215 units with a variable range of 176 ~ 254 
# with 95% confidence in a store in one week on average. Based on the forecast 
# and other factors, ABC Company can prepare the inventory for all of its stores 
# after the pilot period.

