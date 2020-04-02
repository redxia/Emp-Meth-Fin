library(tidyverse)
library(readxl)
library(ggplot2)
library(sandwich)


# 1
df <- read_xlsx('MktRet_DP_TermSpread.xlsx') %>% 
  rename(er = MktExRet) %>% 
  rename(dp = Mkt_DP) %>% 
  rename(ts = y10minFedFunds) 
plot(df$er ~ df$Date, type='l')
plot(df$dp ~ df$Date, type='l')
plot(df$ts ~ df$Date, type='l')

mean(df$er)
sd(df$er)
fac1 <- as.numeric(lm(df$er ~ lag(df$er))[[1]][2])
hl1 <- log(0.5) / log(fac1)

mean(df$dp)
sd(df$dp)
fac2 <- as.numeric(lm(df$dp ~ lag(df$dp))[[1]][2])
hl2 <- log(0.5) / log(fac2)

mean(df$ts)
sd(df$ts)
fac3 <- as.numeric(lm(df$ts ~ lag(df$ts))[[1]][2])
hl3 <- log(0.5) / log(fac3)


# 2
phi0 <- matrix(0, 3, 1)
phi1 <- matrix(0, 3, 3)
lm1 <- lm(data=df, er ~ lag(er) + lag(dp) + lag(ts))
phi0[1] <- lm1$coefficients[1]
phi1[1,] <- lm1$coefficients[2:4]
sqrt(diag(vcovHC(lm1, type='HC0')))
summary(lm1)$r.squared

lm2 <- lm(data=df, dp ~ lag(er) + lag(dp) + lag(ts))
phi0[2] <- lm2$coefficients[1]
phi1[2,] <- lm2$coefficients[2:4]
sqrt(diag(vcovHC(lm2, type='HC0')))
summary(lm2)$r.squared

lm3 <- lm(data=df, ts ~ lag(er) + lag(dp) + lag(ts))
phi0[3] <- lm3$coefficients[1]
phi1[3,] <- lm3$coefficients[2:4]
sqrt(diag(vcovHC(lm3, type='HC0')))
summary(lm3)$r.squared

# 3
eigen(phi1) # stationary

# 4
et <- t(phi1 %*% t(as.matrix(df[2:4])))
sd(et[,1])

# 5
one <- one <- matrix(1,3,3)
mu <- t(t(one-phi1) %*% phi0)
t <- nrow(df)
mu_m <- matrix(rep(mu,3), nrow=t, ncol=3, byrow=T)
zt <- df[,2:4] - mu_m
et1 <- t(phi1 %*% t(zt)) + mu_m
et1_df <- as.data.frame(et1) %>% 
  mutate(Date = df$Date) %>% 
  rename(Excess_Return = V1) %>% 
  rename(Dividend_Yield = V2) %>% 
  rename(Term_Spread = V3)
  
gather(et1_df, Excess_Return:Term_Spread, key='yeah', value='V') %>% 
  ggplot(aes(x=Date, y=V, col=yeah)) +
  geom_line() +
  labs(x = 'Date', y= 'Variables',
       title = 'One Quarter Ahead Plots') +
  theme(plot.title = element_text(hjust = 0.5))

et4 <- t(phi1^4 %*% t(zt)) + mu_m
et4_df <- as.data.frame(et4) %>% 
  mutate(Date = df$Date) %>% 
  rename(Excess_Return = V1) %>% 
  rename(Dividend_Yield = V2) %>% 
  rename(Term_Spread = V3)

gather(et4_df, Excess_Return:Term_Spread, key='yeah', value='V') %>% 
  ggplot(aes(x=Date, y=V, col=yeah)) +
  geom_line() +
  labs(x = 'Date', y= 'Variables',
       title = 'Four Quarters Ahead Plots') +
  theme(plot.title = element_text(hjust = 0.5))

et20 <- t(phi1^20 %*% t(zt)) + mu_m
et20_df <- as.data.frame(et20) %>% 
  mutate(Date = df$Date) %>% 
  rename(Excess_Return = V1) %>% 
  rename(Dividend_Yield = V2) %>% 
  rename(Term_Spread = V3)

gather(et20_df, Excess_Return:Term_Spread, key='yeah', value='V') %>% 
  ggplot(aes(x=Date, y=V, col=yeah)) +
  geom_line() +
  labs(x = 'Date', y= 'Variables',
       title = 'Four Quarters Ahead Plots') +
  theme(plot.title = element_text(hjust = 0.5))

# 7
train <- round(t*4/5)
pred <- double(t-train-1)
actual <- zt[(train+1):t, 1]

for (i in train:(t-1)){
  out = lm(zt[2:i,1] ~ 0 + zt[1:i-1,1] + zt[1:i-1,2] + zt[1:i-1,3])
  pred[1+i-train] <- as.numeric(out$coefficients) %*% t(zt[i,])
}

oos <- data.frame(Actual = actual, Prediction = pred) %>% 
  mutate(Date = df$Date[(train+1):t]) 
gather(oos, Actual:Prediction, key='yeah', value='V') %>% 
  ggplot(aes(x=Date, y=V, col=yeah)) +
  geom_line() +
  labs(x = 'Date', y= 'Variables',
       title = 'Predicted vs Actual Returns ') +
  theme(plot.title = element_text(hjust = 0.5))