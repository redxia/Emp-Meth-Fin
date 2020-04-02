library(tidyverse)
library(lubridate)
library(ggplot2)
library(sandwich)

# CRSP data
sp <- read_csv('sp500.csv') 
sp <- sp %>% 
  mutate(div = (vwretd - vwretx) * usdval) %>% 
  mutate(div_yield = double(nrow(sp)))
sp$div_yield[1:12] <- NA  
for (i in 13:nrow(sp)){
  sp$div_yield[i] = (sum(sp$div[(i-1) : (i-12)]) / sp$usdval[i])
}

tb <- read_csv('t-bill.csv') %>% 
  mutate(DATE = caldt)
df <- inner_join(sp, tb, by='DATE') %>% 
  mutate(excess_return = log(vwretd+1) - log(t30ret+1)) %>% 
  select(DATE, div_yield, excess_return) %>% 
  mutate(DATE = floor_date(ymd(DATE), 'month'))


# FRED data
t_sp <- read_csv('T10YFFM.csv') %>% 
  mutate(T10YFFM = T10YFFM/100) %>% 
  rename(term_spread = T10YFFM)
aaa <- read_csv('AAAFFM.csv')
baa <- read_csv('BAAFFM.csv')
d_sp <- inner_join(aaa, baa, by='DATE') %>%
  mutate(default_spread = BAAFFM/100 - AAAFFM/100)

spread <- inner_join(t_sp, d_sp, by='DATE') %>% 
  select(DATE, term_spread, default_spread) 

# all needed data
df <- inner_join(df, spread, by='DATE') 

# plots
gather(df, div_yield : default_spread, key='yeah', value='Variables') %>% 
  ggplot() + 
  geom_line(aes(x=DATE, y=Variables)) + 
  facet_grid(yeah~., scale='free')

# regression
lm1 <- lm(lead(excess_return) ~ div_yield + term_spread + default_spread, data = df)
out1<- summary(lm1)
coef1<- as.numeric(out1$coefficients[,1])[-1]
ols1 <- as.numeric(out1$coefficients[,2])[-1]
reg1 <- c(coef1[1], ols1[1], NA, coef1[2], ols1[2], NA,
          coef1[3], ols1[3], NA, out1$r.squared)

roll_func <- function(window){
  roll = double(nrow(df))
  for (i in 1 : (nrow(df)-window)){
    roll[i] = sum(df$excess_return[(i+1) : (i+window)])
  }
  for (i in (nrow(df)-window+1): nrow(df)){
    roll[i] = NA
  }
  return (roll)
}

df$mo3 <- roll_func(3)
df$mo12 <- roll_func(12)
df$mo24 <- roll_func(24)
df$mo60 <- roll_func(60)

reg_func <- function(y, lags){
  lm <- lm(y ~ div_yield + term_spread + default_spread, data = df)
  out <- summary(lm)
  coef <- c(as.numeric(out$coefficients[,1])[-1])
  ols <- c(as.numeric(out$coefficients[,2])[-1])
  nw <- NeweyWest(lm, lag=(lags-1)*1.5, prewhite=F, adjust=T)
  nw_se <- as.numeric(sqrt(diag(nw))[-1])9
  result <- c(coef[1], ols[1], nw_se[1], coef[2], ols[2], nw_se[2], 
              coef[3], ols[3], nw_se[3], out$r.squared)
  return (result)
}

result <- data.frame(M1 = reg1, M3 = reg_func(df$mo3,3), M12 = reg_func(df$mo12,12), 
                     M24 = reg_func(df$mo24,24), M60 = reg_func(df$mo60,60))

row.names(result) <- c('Div Yield', 'OLS_DY', 'NW_DY', 'Term Spread', 'OLS_TS', 'NW_TS', 
                       'Default Spread', 'OLS_DS', 'NW_DS', 'R-squared')

# 12-month
lm12 <- lm(mo12 ~ div_yield + term_spread + default_spread, data = df)
pred12 <- df %>% 
  select(DATE, mo12, div_yield, term_spread, default_spread) 
pred12 <- pred12 %>% 
  mutate(pred = predict(lm12, pred12[,3:5])) %>% 
  drop_na()

ggplot(pred12) +
  geom_line(aes(DATE, pred), colour = 'purple') +
  geom_line(aes(DATE, mo12), colour = 'yellow') +
  labs(y = 'Excess Return', title = '12-Month Excess Return') +
  theme(plot.title = element_text(hjust = 0.5)) 

