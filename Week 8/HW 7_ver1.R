library(tidyverse)
library(ggplot2)

ff <- read_csv('F-F_Research_Data_Factors.csv', skip=3)%>% 
  filter(X1>=196001 & X1<=201512) 

ind <- read_csv('48_Industry_Portfolios.csv', skip=11) %>% 
  filter(X1>=196001 & X1<=201512) %>% 
  slice(1:nrow(ff)) %>% 
  mutate_if(is.numeric, list(~na_if(., "-99.99"))) %>% 
  select_if(~!any(is.na(.))) %>% 
  mutate(RF = ff$RF) 
ind[2:44] = ind[2:44] - ind$RF


# Question 1
pca <- prcomp(ind[2:44])
eigen_value <- pca$sdev^2
eigen_value
ev_frac <- summary(pca)$importance[2,]
barplot(ev_frac, main='Fraction of Variance')

# Question 2
# a)
# cumulative variance 
summary(pca)$importance[3,3]

# b)
# use eigenvector as weight
wt <- pca$rotation[,1:3]

# sample mean
ind_mean <- pca$center
pca_mean <- t(wt) %*% ind_mean
as.numeric(pca_mean)

# sample SD
pca_sd <- pca$sdev[1:3]
pca_sd

# correlation
sigma <- cov(ind[2:44])
cov12 <- t(wt[,1]) %*% sigma %*% wt[,2]
cor12 <- cov12 / (pca_sd[1] * pca_sd[2])
cor12
cov13 <- t(wt[,1]) %*% sigma %*% wt[,3]
cor13 <- cov13 / (pca_sd[1] * pca_sd[3])
cor13
cov23 <- t(wt[,2]) %*% sigma %*% wt[,3]
cor23 <- cov23 / (pca_sd[2] * pca_sd[3])
cor23

# c)
# Calculating E[Rt] and E[Ft]. Beta equals to weights
E.Rt = wt %*% pca_mean
act.ret = apply(ind[,2:44], 2, mean)

df2 = data.frame(matrix(ncol = 0, nrow = length(E.Rt))) %>%
  mutate(Industry = colnames(ind[,2:44])) %>%
  mutate(Ex.Return = E.Rt) %>%
  mutate(Act.Return = act.ret)

plot2 = ggplot(data = df2, aes(x = Act.Return, y = Ex.Return, label = Industry)) + geom_point()
plot2 = plot2 + geom_abline(intercept = 0) + xlim(c(0.1,1)) + ylim(c(0.2,0.8))
plot2 = plot2 + geom_text_repel()
plot2

# d)
R_sq <- 1 - var(mf$Actual-mf$Predict) / var(mf$Actual)
R_sq

# Question 3
# a)
ff25 <- read_csv('25_Portfolios_5x5.CSV', skip=15)%>% 
  filter(X1>=196001 & X1<=201512) %>% 
  slice(1:nrow(ff))
ff25[2:26] = ff25[2:26] - ind$RF

pca25 <- prcomp(ff25[2:26])
eigen_value25 <- pca25$sdev^2
eigen_value25
ev_frac25 <- summary(pca25)$importance[2,]
barplot(ev_frac25, main='Fraction of Variance')

# b)
summary(pca25)$importance[3,]
