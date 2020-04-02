### Empirical Method in Finance HW4 ###

library(tidyverse)
library(Rmisc)
library(readxl)
library(sandwich)

## Question 1
# Importing and Cleaning up the data
stock = read.csv("sp500.csv")
ff = read.csv("fedfund.csv")

df1 = data.frame(matrix(ncol = 0, nrow = length(stock$DATE)))
df1$Date = as.character(stock$DATE)
df1$Date = as.Date(df1$Date, "%Y%m%d")
df1$ret.ex.div = stock$vwretx
df1$ret.cum.div = stock$vwretd
df1$val = stock$usdval
df1$div = (df1$ret.cum.div - df1$ret.ex.div) * df1$val # Dividend amount
df1$div.yield = double(length(df1$Date))

# Calculating the dividend yield
for (i in 13:length(df1$Date)){
  df1$div.yield[i] = (sum(df1$div[(i-12) : (i-1)]) / df1$val[i])
}

df1 = df1[-(1:12),] # Adjusting the date
df1$ff = ff$TMYTM / 100 / 12 # Risk free rates

# Calculating the Adjusted Dividend Yield
mean_1st = mean(df1$div.yield[1:348]) 
mean_2nd = mean(df1$div.yield[349:length(df1$Date)])
df1$div.yield[1:348] = df1$div.yield[1:348] - mean_1st
df1$div.yield[349:length(df1$Date)] = df1$div.yield[349:length(df1$Date)] - mean_2nd
df1$excess.ret = log(df1$ret.cum.div + 1) - log(df1$ff +1) # Excess return
ty_ff = read.csv("T10YFFM.csv")
Aaa = read.csv("AAAFFM.csv")
Baa = read.csv("BAAFFM.csv")
df1$terms = ty_ff$T10YFF / 100 # Term spreads
df1$default = (Baa$BAAFF - Aaa$AAAFF) / 100 # Default spreads

## Question 2
# Plotting the predictable variables
plot1 = ggplot(df1, aes(x = Date, y = excess.ret, group = 1)) + geom_line() + labs(title = "Excess Return")
plot2 = ggplot(df1, aes(x = Date, y = div.yield, group = 1)) + geom_line(colour = "Blue") + labs(title = "Dividend Yield")
plot3 = ggplot(df1, aes(x = Date, y = terms, group = 1)) + geom_line(colour = "Red")  + labs(title = "Term Spread")
plot4 = ggplot(df1, aes(x = Date, y = default, group = 1)) + geom_line(colour = "Purple") + labs(title = "Default Spread")
mul.plot1 = multiplot(plot1, plot2, plot3, plot4, cols = 2)


## Question 3
# Function to calculate the future returns
sum_function = function(data, periods) {
  ret.data = rep(NA, length(data))
  for(i in 1:(length(data) - periods + 1)) {
    ret.data[i] = sum(data[(i+1):(i + periods)])
  }
  return (ret.data)
}

# Storing the future returns of 1m, 3m, 12m, 24m, and 60m
df1$ret1m = sum_function(df1$excess.ret, 1)
df1$ret3m = sum_function(df1$excess.ret, 3)
df1$ret12m = sum_function(df1$excess.ret, 12)
df1$ret24m = sum_function(df1$excess.ret, 24)
df1$ret60m = sum_function(df1$excess.ret, 60)

# Function to regress the model and store the values 
reg_fun = function(data, num) {

# regressing the models
out1m = lm(data = data, data[,11] ~ data[,num])
out3m = lm(data = data, data[,12] ~ data[,num])
out12m = lm(data = data, data[,13] ~ data[,num])
out24m = lm(data = data, data[,14] ~ data[,num])
out60m = lm(data = data, data[,15] ~ data[,num])
df.result = data.frame(out1m$coef, out3m$coef, out12m$coef, out24m$coef, out60m$coef)

out1m.1 = summary(out1m)
out3m.1 = summary(out3m)
out12m.1 = summary(out12m)
out24m.1 = summary(out24m)
out60m.1 = summary(out60m)

# R-square
r2.1m = out1m.1$r.squared
r2.3m = out3m.1$r.squared
r2.12m = out12m.1$r.squared
r2.24m = out24m.1$r.squared
r2.60m = out60m.1$r.squared
r2.result = c(r2.1m, r2.3m, r2.12m, r2.24m, r2.60m)

# OLS standard error
se.1m = out1m.1$sigma
se.3m = out3m.1$sigma
se.12m = out12m.1$sigma
se.24m = out24m.1$sigma
se.60m = out60m.1$sigma
se.result = c(se.1m, se.3m, se.12m, se.24m, se.60m)

# Newey-west covariance matrix
nwvcov.1m = NeweyWest(out1m, lag = (0 * 1.5), prewhite = F, adjust = T)
nwvcov.3m = NeweyWest(out3m, lag = (2 * 1.5), prewhite = F, adjust = T)
nwvcov.12m = NeweyWest(out12m, lag = (11 * 1.5), prewhite = F, adjust = T)
nwvcov.24m = NeweyWest(out24m, lag = (23 * 1.5), prewhite = F, adjust = T)
nwvcov.60m = NeweyWest(out60m, lag = (59 * 1.5), prewhite = F, adjust = T)

# Newey-west standard error
nwse.1m = sqrt(diag(nwvcov.1m))
nwse.3m = sqrt(diag(nwvcov.3m))
nwse.12m = sqrt(diag(nwvcov.12m))
nwse.24m = sqrt(diag(nwvcov.24m))
nwse.60m = sqrt(diag(nwvcov.60m))
nwse.result = c(nwse.1m, nwse.3m, nwse.12m, nwse.24m, nwse.60m)

# Summarizing the result
df.result = rbind(df.result, se.result, nwse.result, r2.result)
rownames(df.result) = c("Intercept", "Slope", "OLS-StdErr", "NW-StdErr", "R-Squared")
colnames(df.result) = c("1M", "3M", "12M", "24M", "60M")
return(df.result)
}

# Runinng the functions
div.result = reg_fun(df1, 6)
terms.result = reg_fun(df1, 9)
default.result = reg_fun(df1,10)


## Question 4
# Regressing the 12m model
div12m = lm(data = df1, ret12m ~ div.yield)
terms12m = lm(data = df1, ret12m ~ terms)
default12m = lm(data = df1, ret12m ~ default)

# Estimating the expectef 12-month excess returns
df2 = data.frame(matrix(ncol = 0, nrow = (length(df1$Date)-12)))
df2$Date = df1$Date[1:(length(df1$Date)-12)]
df2$fit.div12m = div12m$fitted.values
df2$fit.terms12m = terms12m$fitted.values
df2$fit.default12m = default12m$fitted.values
df2$Actual = df1$ret12m[1:(length(df1$Date)-12)]

# Plotting the result
plot1.4 = ggplot(data = df2, aes(x = Date, y = Actual, group = 1)) + geom_line() + labs(title = "Actual")
plot2.4 = ggplot(data = df2, aes(x = Date, y = fit.div12m, group = 1)) + geom_line(colour = "Blue") + labs(title = "Dividend")
plot3.4 = ggplot(data = df2, aes(x = Date, y = fit.terms12m, group = 1)) + geom_line(colour = "Red") + labs(title = "Term Spread")
plot4.4 = ggplot(data = df2, aes(x = Date, y = fit.default12m, group = 1)) + geom_line(colour = "Purple") + labs(title = "Default Spread")
mul.plot2 = multiplot(plot1.4, plot2.4, plot3.4, plot4.4, cols = 2)
