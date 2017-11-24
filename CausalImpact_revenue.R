# Developed by Adriano M. Yoshino - amyoshino@nyu.edu

# Installing packages
# install.packages("CausalImpact")

# Loading libraries
library(CausalImpact)
library(ggplot2)

#Load Data
data<-read.csv("DATA")

# Creating Android and iPhone datasets
android <- data[,c(10,9,11,7)]
iphone <- data[,c(7,6,8)]

# Setting data as time series (index is a timeserie)
time.points <- seq.Date(as.Date("2014-12-14"), by = 1, length.out = 59)
android <- zoo(android, time.points)
iphone <- zoo(iphone, time.points) 
head(android)

# Setting pre and post treatment periods
pre.period <- as.Date(c("2014-12-14", "2015-01-31"))
post.period <- as.Date(c("2015-02-01", "2015-02-10"))

# Running analysis for Android
# Expected: the impact of the new version of the app on revenue 
impact_android <- CausalImpact(android, pre.period, post.period)
plot(impact_android)
summary(impact_android)
summary(impact_android, "report")

# Running analysis for Android
# Expected: show that there were no effects found, due to no change in the iPhone product
impact_iphone <- CausalImpact(iphone, pre.period, post.period)
plot(impact_iphone)
summary(impact_iphone)
summary(impact_iphone, "report")

# Features used to build the model
plot(impact_android$model$bsts.model, "coefficients")

# Estimating impact on iPhone
iphone_est <- data[1:49,c(1,7,6,8)]
incremental <- mean(iphone_est$revenue_iphone)*0.29
iphone$revenue_iphone_adj <- iphone$revenue_iphone
iphone$revenue_iphone_adj[50:59] <- iphone$revenue_iphone_adj[50:59] + incremental

# Plotting results for iPhone estimation
plot(iphone$revenue_iphone_adj, type="l",lty = 'dashed', col="darkblue", lwd = 2, ann = FALSE )
title(main="Revenue iPhone",xlab="Time", ylab="Revenue")
lines(iphone$revenue_iphone, type="l",col="black", lwd = 2 )
abline(h = mean(iphone_est$revenue_iphone), col="gray", lwd = 2  )
grid(NULL, NULL, lwd = 1.5 )


  