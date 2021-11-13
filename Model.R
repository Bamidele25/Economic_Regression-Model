#read in the data set 
mydata = read.csv('AS.csv')
head(mydata)
#summarize the data set 
summary(mydata)
plot(mydata$GDP)
#timeseries plot 
mydata_ts <- ts(mydata,frequency = 4, start = c(1980,1))
head(mydata_ts)
#timeseries plot 
plot.ts(diff(mydata_ts), pch= 18, main = "1st Diff Series of the Variables", xlab = "Year", ylab = "GDP", type = "l", col ="red")
#regression models 
library(tidyverse) # metapackage with lots of helpful functions
library(gridExtra) # to plot multiple ggplots aside
library(car) # package for regression diagnostics

#model one 
mymod = lm(GDP ~ VOA, data = mydata)
summary(mymod)
#model two
mymod2 = lm(VOA ~ FGEA, data = mydata)
summary(mymod2)
#model three 
mymod3 = lm(VOA ~ DMBLA, data = mydata)
summary(mymod3)
#mymodel four
mymod4 = lm(VOA ~ DMBLA + FGEA, data = mydata)
summary(mymod4)

##Diagonastic Test
#durbin waston 
dwt(mymod)
dwt(mymod2)
dwt(mymod3)
dwt(mymod4)
##residual assumption 
plot(mymod)
plot(mymod2)
plot(mymod3)
#Shapiro Wilk Test
shapiro.test(mymod$residuals)
shapiro.test(mymod2$residuals)
shapiro.test(mymod3$residuals)

