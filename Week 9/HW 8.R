library(knitr)

# 1
mean_rm = 0.05
sd_rm = 0.15

mean_r1 = 0.01 + 0.9 * mean_rm
mean_r2 = -0.015 + 1.2 * mean_rm
mean_r3 = 0.005 + 1.0 * mean_rm

var_r1 = (0.9^2) * (sd_rm^2) + (0.1^2)
var_r2 = (1.2^2) * (sd_rm^2) + (0.15^2)
var_r3 = (1.0^2) * (sd_rm^2) + (0.05^2)
sd_r1 = sqrt(var_r1)
sd_r2 = sqrt(var_r2)
sd_r3 = sqrt(var_r3)

sr_r1 = mean_r1 / sd_r1
sr_r2 = mean_r2 / sd_r2
sr_r3 = mean_r3 / sd_r3

result1 <- data.frame(sample_mean=c(mean_r1,mean_r2,mean_r3),
                      standard_deviation=c(sd_r1,sd_r2,sd_r3),
                      sharpe_ratio=c(sr_r1,sr_r2,sr_r3))
kable(result1, caption='Sample Data Summary')

# 2
result2 <- data.frame(sample_mean=c(0.01,-0.015,0.005),
                      standard_deviation=c(0.1,0.15,0.05),
                      sharpe_ratio=c(0.01/0.1, 0.015/-0.15, 0.005/0.05))
kable(result2, caption='Market Neutral Version Summary')

# 3
avg_re1 <- result2[,1]
omega1 <- matrix(c(0.1^2,0,0,0,0.15^2,0,0,0,0.05^2), nrow=3, byrow=T)
sr_mve1 <- sqrt(t(avg_re1)%*%solve(omega1)%*%avg_re1)
as.numeric(sr_mve1)

# 4
avg_re2 <- c(result2[,1],0.05)
omega2 <- matrix(c(0.1^2,0,0,0, 0,0.15^2,0,0, 0,0,0.05^2,0, 0,0,0,0.15^2), 
                 nrow=4, byrow=T)
sr_mve2 <- sqrt(t(avg_re2)%*%solve(omega2)%*%avg_re2)
as.numeric(sr_mve2)

# 5/a)
k <- as.numeric(0.15 / sr_mve2)
wt_mve <- k * solve(omega3)%*%avg_re3

# 5/b)
er_mve <- t(wt_mve)%*%c(result1[,1],0.05)
sd_mve <- k*sr_mve2
sr_mve <- sr_mve2
result3 <- data.frame(sample_average=er_mve,
                      standard_deviation=sd_mve,
                      sharpe_ratio=sr_mve)
kable(result3, caption='MVE Portfolio Summary')



