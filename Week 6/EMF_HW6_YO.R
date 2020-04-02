### Empirical Methods in Finance

library(fGarch)
library(tidyverse)
library(readxl)
library(xts)

# Impoting and organizing the data
ddf = read.csv("F-F_Research_Data_5_Factors_2x3_daily.CSV", skip = 3 ,stringsAsFactors=FALSE)
mdf = read.csv("F-F_Research_Data_5_Factors_2x3.CSV", skip = 3, stringsAsFactors=FALSE)
mdf = mdf[1:678,]
ddf$X = as.Date(as.character(ddf$X), "%Y%m%d")
mdf$X = as.numeric(mdf$X)*100 + 1
mdf$X = as.Date(as.character(mdf$X), "%Y%m%d")
mdf$X = as.Date(as.character(mdf$X), "%Y%m%d")
dCMA = as.ts(as.numeric(ddf$CMA))
mCMA = as.ts(as.numeric(mdf$CMA))

## Problem 1-1
# Run the ARMA(1,1) model
mout1 = arima(mCMA, c(1,0,1))
mout1

# Store the parameters
phi1 = as.numeric(mout1$coef[1])
theta1 = as.numeric(mout1$coef[2])
sigma = sqrt(var(as.numeric(mout1$residuals)))

# Caluculating the covariance with lag 0 (= Variance of yt)
gamma0 = sigma^2 * (1 + theta1^2 - 2 * phi1 * theta1) / (1 - phi1^2)
gamma0

# Using gamma above, compute the first order autocorrelation
rho = double()
rho[1] = phi1 - theta1 * (sigma^2 / gamma0)
for(i in 2:20){
  rho[i] = rho[i-1] * phi1
}
rho = as.data.frame(rho)
rho$lag = seq(1,20,1)
plot1 = ggplot(rho, aes(x = lag, y = rho)) + geom_bar(stat = "identity")

# Calculating the half life
mhl = log(0.5)/log(as.numeric(mout1$coef[1]))


## Problem 1-2 and 1-3
# Run the arch(12) and garch(1,1) model
m.arch.12 = garchFit(~garch(12,0), data = mCMA, trace = F)
m.garch.1.1 = garchFit(~garch(1,1), data = mCMA, trace = F)

# Summarizing the results
result.m = data.frame(matrix(ncol = 0, nrow = length(mdf$X))) %>%
  mutate(Date = mdf$X) %>%
  mutate(A.CV = m.arch.12@h.t) %>%
  mutate(G.CV = m.garch.1.1@h.t) %>%
  mutate(A.eta = abs(residuals(m.arch.12, standardize = T))) %>%
  mutate(G.eta = abs(residuals(m.garch.1.1, standardize = T)))
  
# Plot the conditional variance and the standadized residuals
m.base = ggplot(data = result.m, aes(x = Date))
plot2 = m.base + geom_line(aes(y = A.CV), colour = "blue") + geom_line(aes(y = G.CV), colour = "red", linetype = "dashed", size = 1.2)
plot3 = m.base + geom_line(aes(y = A.eta), colour = "blue")
plot4 = m.base + geom_line(aes(y = G.eta), colour = "blue")

acf(result.m$A.eta)
acf(result.m$G.eta)


## Problem 1-4
# Subset monthly data and calculate monthly variance
ddf.xts = xts(ddf, order.by = ddf$X)
month.end = endpoints(ddf.xts, on = "month")
month.beg = head(endpoints(ddf.xts, on='month')+1, -1)
month.var = double(length(month.beg))
ddf$sq.ret = ddf$CMA^2
for (i in (1:length(month.beg))){
  month.var[i] = sum(ddf$sq.ret[month.beg[i]:month.end[i+1]])
}

# Store the value and plot the graph
result.d = data.frame(matrix(ncol = 0, nrow = length(month.var))) %>%
  mutate(Date = mdf$X) %>%
  mutate(month.Var = month.var)

d.base = ggplot(data = result.d, aes(x = Date))
plot5 = d.base + geom_line(aes(y = month.var), colour = "blue")

## Problem 1-5
# Compute the first order autocorrelations
acf.arch.1st = acf(m.arch.12@residuals)[1]
acf.garch.1st = acf(m.garch.1.1@residuals)[1]
acf.RV.1st = acf(result.d$month.Var)[1]

# Compute the correlations
corr.RV_archRes = cor(m.arch.12@residuals, result.d$month.Var)
corr.RV_garchRes = cor(m.garch.1.1@residuals, result.d$month.Var)
corr.RV_garchFitted = cor(m.garch.1.1@h.t, result.d$month.Var)
corr.garchRes_garchFitted = cor(m.garch.1.1@residuals, m.garch.1.1@h.t)


## Problem 1-6
# Run the arma(1,1) model
RV_arma1.1 = arima(result.d$month.Var, order = c(1,0,1))
RV_arma1.1
par = as.numeric(RV_arma1.1$coef)
epsilon = RV_arma1.1$residuals
mu = par[3]/(1 - par[1])

# Compute the time series of vt
vt = double(length(result.d$month.Var-1))
for(j in 1:length(vt)){
  vt[j] = par[1]*result.d$month.Var[j] + par[2]*epsilon[j]
}

# Compute correlation and plot the result
corr.RV_arma = cor(result.d$month.Var, vt)
result.d = result.d %>%
  mutate(Vt = vt)

d.base = ggplot(data = result.d, aes(x = Date))
plot6 = d.base + geom_line(aes(y = month.var), colour = "blue") + geom_line(aes(y = Vt), colour = "red", linetype = "dashed", size = 1.2)
