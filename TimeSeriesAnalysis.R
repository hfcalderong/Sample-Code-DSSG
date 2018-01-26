#############################
# PREPARATION
#############################

#Packages
libraries = c("forecast","tseries","TTR","dplyr")
lapply(libraries, require, character.only = T)

#Preparatory
setwd("~/Documents/Nitesh/Restaurant/R") #Sets working directory
restaurant <- read_excel("~/Documents/Nitesh/Restaurant/Data.xlsx") #imports data
restaurant = restaurant[-c(1:10),]

sample = 1:28
test_all = 29:34

#####################
#EXPLORATORY ANALYSIS
#####################
#Graphical analysis
plot.ts(restaurant$y)
plot.ts(restaurant$x1)
plot.ts(restaurant$x2)
plot.ts(restaurant$x3)
plot(restaurant)

#Autocorrelation and correlation matrix
cor(restaurant[,-1]) #Correlation matrix of dependent and independent variables
eigen(var(restaurant[,-c(1:2)])) #PCA on the regressors
acf(restaurant[,-1]) #Autocorrelation function
pacf(restaurant$y) #Partial autocorrelation function
ccf(x = restaurant$x1, y = restaurant$y) #Crosscorrelation function

#Seasonality
spectrum(restaurant$y,log="no") #Spectrum analysis

######################
#UNIVARIATE ANALYSIS
######################
y = ts(restaurant$y,frequency = 1)
summary(y)

#ARIMA
adf.test(y[sample]) #Unit root test. It seems to require differentiation.
m = auto.arima(y[sample]) #The result shows that the series is differentiated 1 time.
summary(m)
c = forecast(m,h = length(test_all)) #Forecast in the testing set
plot(c,col="gray")
lines(y)
lines(c$fitted,col="blue")
sqrt(var(y[test_all]-c$mean)) #MSE of Forecast in the testing set

#Holt-Winters - Exponential Smoothing V1
m = HoltWinters(y[sample], beta=F, gamma=F)
c = forecast(m,h = length(test_all))
plot(c,col="gray")
lines(y)
lines(m$fitted[,1],col = "blue")
sqrt(var(y[test_all]-c$mean))

#Holt-Winters - Exponential Smoothing V2
m = HoltWinters(y[sample], beta=T, gamma=F)
c = forecast(m,h = length(test_all))
plot(c,col="gray")
lines(y)
lines(m$fitted[,1],col = "blue")
sqrt(var(y[test_all]-c$mean))

#Holt - Exponential Smoothing
m = holt(y[sample])
c = forecast(m,h = length(test_all))
plot(c,col="gray")
lines(y)
lines(m$fitted,col = "blue")
sqrt(var(y[test_all]-c$mean))

#Simple Moving Average
m = SMA(y[sample],n = 5)
c = forecast(m,h = length(test_all))
plot(c,col="gray",ylim = c(0,200),xlim = c(0,50))
lines(y)
lines(m,col = "blue")
sqrt(var(y[test_all]-c$mean))

######################
#MULTIVARIATE ANALYSIS
######################

#We won't use the third variable so that we avoid multicollinearity

#Forecast for x1
m = nnetar(restaurant$x1[sample],p=1,repeats = 20) #Recursive Neural Network
c = forecast(m, h = 20)
plot(restaurant$x1,col = "gray",type="l",xlim = c(0,60))
lines(c(m$fitted,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(restaurant$x1[test_all]-c$mean[1:length(test_all)]))
aux1 = c$mean

#Forecast for X2
m = nnetar(restaurant$x1[sample],p=1,repeats = 20) #Recursive Neural Network
c = forecast(m, h = 20)
plot(restaurant$x1,col = "gray",type="l",xlim = c(0,60))
lines(c(m$fitted,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(restaurant$x1[test_all]-c$mean[1:length(test_all)]))
aux2 = c$mean


#Models using only x1 as regressors
aux_reg = restaurant[,c(2,3)]

#Regression Analysis: y = a1*x1
aux_reg = restaurant[,c(2,3)]
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y,col = "gray")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all]-c$mean))

c = forecast(m,newdata = aux1)
plot(y,col = "gray")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))


#Regression Analysis: y(t) = a1*x1(t) + a2*y(t-1)
aux_reg = restaurant[,c(2,3)]
c = diff(aux_reg$y,1) #First lag of the variable
aux_reg = aux_reg[-1,]
aux_reg$dy1 = c
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y[-1],col = "gray",type="l")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all][-1]-c$mean[-8]))

#Regression Analysis: y(t) = a1*x1(t) + a2*x1(t-1)
aux_reg = restaurant[,c(2,3)]
c = diff(aux_reg$x1,1)
aux_reg = aux_reg[-1,]
aux_reg$dx1 = c
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y[-1],col = "gray",type="l")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all][-1]-c$mean[-8]))

#Regression Analysis: y(t) = a1*x1(t) + a2*y(t-1)+a3*x1(t-1)
aux_reg = restaurant[,c(2,3)]
c = diff(aux_reg$x1,1)
d = diff(aux_reg$y,1)
aux_reg = aux_reg[-1,]
aux_reg$dx1 = c
aux_reg$dy1 = d
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y[-1],col = "gray",type="l")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all][-1]-c$mean[-8]))

#Models using only x2 as regressors
#Regression Analysis: y = a1*x2
aux_reg = restaurant[,c(2,3)]
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y,col = "gray")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all]-c$mean))

c = forecast(m,newdata = aux2)
plot(y,col = "gray")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))

#Regression Analysis: y(t) = a1*x2(t) + a2*y(t-1)
aux_reg = restaurant[,c(2,3)]
tail(restaurant)
c = diff(aux_reg$y,1)
aux_reg$y
aux_reg = aux_reg[-1,]
aux_reg$dy1 = c
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y[-1],col = "gray",type="l")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all][-1]-c$mean[-8]))

#Regression Analysis: y(t) = a1*x2(t) + a2*x2(t-1)
aux_reg = restaurant[,c(2,3)]
c = diff(aux_reg$x1,1)
aux_reg = aux_reg[-1,]
aux_reg$dx2 = c
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y[-1],col = "gray",type="l")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all][-1]-c$mean[-8]))

#Regression Analysis: y(t) = a1*x2(t) + a2*y(t-1)+a3*x2(t-1)
aux_reg = restaurant[,c(2,3)]
c = diff(aux_reg$x1,1)
d = diff(aux_reg$y,1)
aux_reg = aux_reg[-1,]
aux_reg$dx2 = c
aux_reg$dy1 = d
m = lm(y~.,data = aux_reg[sample,])
c = forecast(m,newdata = aux_reg[test_all,-1])
plot(y[-1],col = "gray",type="l")
lines(c(m$fitted.values,c$mean))
abline(v=max(sample),col="blue")
summary(m)
sqrt(var(y[test_all][-1]-c$mean[-8]))

#ARIMA with X1 as external variable
d = restaurant$x1[sample]
m = arima(y[sample],order = c(0,1,1),xreg = d)
d = restaurant$x1[test_all]
c = forecast(m, h = 8, xreg = d)
plot(y,col = "gray")
lines(c(y - m$residuals,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean))

d = aux1
c = forecast(m, h = length(test_all), xreg = d)
plot(y,col = "gray")
lines(c(y - m$residuals,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))


#ARIMA with X2 as external variable
d = restaurant$x2[sample]
m = arima(y[sample],order = c(0,1,1),xreg = d)
d = restaurant$x2[test_all]
c = forecast(m, h = length(test_all), xreg = d)
plot(y,col = "gray")
lines(c(y - m$residuals,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean))

d = aux2
c = forecast(m, h = length(test_all), xreg = d)
plot(y,col = "gray")
lines(c(y - m$residuals,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))


#ARIMA with X1 and X2 as external variables
d = restaurant[sample,c(3,4)]
m = arima(y[sample],order = c(0,1,1),xreg = d)
d = restaurant[test_all,c(3,4)]
c = forecast(m, h = length(test_all), xreg = d)
plot(y,col = "gray")
lines(c(y - m$residuals,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))

d = cbind(aux1,aux2)
c = forecast(m, h = length(test_all), xreg = d)
plot(y,col = "gray")
lines(c(y - m$residuals,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))

#Neural Network Autoregressive Model - NNAR(p)
m = nnetar(y[sample],p=3,repeats = 20)
c = forecast(m, h = 8)
plot(y,col = "gray")
lines(c(m$fitted,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean))


#Neural Network Autoregressive Model with external variable- NNARX
d = restaurant$x1[sample]
m = nnetar(y[sample], p = 1,repeats = 30,xreg = d)
d = restaurant$x1[test_all]
#d = c(restaurant$x1[test_all],aux[9:20])
c = forecast(m, h = length(test_all),xreg = d)
plot(y,col = "gray",xlim = c(0,length(sample)+length(d)))
lines(c(m$fitted,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))

d = aux1
c = forecast(m, h = length(test_all),xreg = d)
plot(y,col = "gray",xlim = c(0,length(sample)+length(d)))
lines(c(m$fitted,c$mean))
abline(v=max(sample),col="blue")
sqrt(var(y[test_all]-c$mean[1:length(test_all)]))
