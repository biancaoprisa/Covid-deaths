library(dplyr)
library(forecast)
library(zoo)
library(ggplot2)
library(GGally)
library(gridExtra)

covid <- read.csv("covid19_subset.csv")
covid$date <- as.Date(covid$dateRep, format = "%Y-%m-%d")

str(covid)

#splitting the data into different subsets for each country 
nl.data <- covid[covid$countriesAndTerritories == "Netherlands",]
t.nl <- nl.data$date 
nl.data$day.no <- 1:length(t.nl)

nl.training <- nl.data[nl.data$date < "2020-05-16",]
nl.test <- nl.data[-c(1:62),]

dim(nl.training)
dim(nl.test)

peru.data <- covid[covid$countriesAndTerritories == "Peru",]
peru.training <- peru.data[peru.data$date < "2020-05-16",]
peru.test <- peru.data[-c(1:49),]
t.peru <- peru.data$date
peru.data$day.no <- 1:length(t.peru)

ro.data <- covid[covid$countriesAndTerritories == "Romania",]
ro.training <- ro.data[ro.data$date < "2020-05-16",]

nl.peru.data <- covid[-c(144:211),]
#Exploratory Analysis
summary(nl.peru.data)
sum(nl.training$cases)
sum(nl.training$deaths)

ggplot(nl.peru.data, aes(date, deaths)) + geom_line(aes(color = factor(countriesAndTerritories))) +
  scale_x_date(date_labels = "%d-%b") + xlab("Date") + ylab("Deaths")

#Netherlands
summary(nl.training[, -c(1,2,3,4,7,8,9,10,11,13)])

ggpairs(nl.training[, -c(1,2,3,4,7,8,9,10,11,14)]) +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1))

nl.p <- ggplot(nl.training, aes(date, deaths)) + geom_line(color = "#41b6c4") +
  scale_x_date(date_labels = "%d-%b") + xlab("Date") + ylab("Deaths") +
  ggtitle("Number of deaths in the Netherlands")
nl.p

#there seems to be a trend present and the variance seems to be increasing with time 

nl.p +
  geom_line(aes(y=cases), color = "#d73027") + xlab("Date") + ylab("Deaths & Cases") +
  ggtitle("Number of deaths and cases in Netherlands")


nl.p1 <- ggplot(nl.training, aes(date, cases)) + geom_line(color = "#d73027") +
  scale_x_date(date_labels = "%d-%b") + xlab("Date") + ylab("Cases") +
  ggtitle("Number of cases in Netherlands")
nl.p1

nl.p2 <- ggplot(nl.training, aes(cases, deaths)) + geom_point(color="#d73027") +
  xlab("Cases") + ylab("Deaths") +
  ggtitle("Netherlands")
nl.p2

nl.p3 <- ggplot(nl.training, aes(Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, deaths)) + geom_point(color="#d73027") +
  xlab("Cumulative Cases") + ylab("Deaths") +
  ggtitle("Netherlands")
nl.p3

nl.p4 <- ggplot(nl.training, aes(date, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)) + geom_point(color="#d73027") +
  xlab("Date") + ylab("Cumulative cases") +
  ggtitle("Netherlands")
nl.p4

grid.arrange(nl.p3, nl.p4, nrow = 2)

#Peru
summary(peru.training[, -c(1,2,3,4,7,8,9,10,11,13)])
sum(peru.training$cases)
sum(peru.training$deaths)

ggpairs(peru.training[, -c(1,2,3,4,7,8,9,10,11,14)]) +
  theme(plot.background = element_rect(
    fill = "transparent",
    colour = NA,
    size = 1))

peru.p <- ggplot(peru.training, aes(date, deaths)) + geom_line(color = "#41b6c4") +
  scale_x_date(date_labels = "%d-%b") + xlab("Date") + ylab("Deaths") +
  ggtitle("Number of deaths in Peru")
peru.p

peru.p + geom_line(aes(y=cases), color = "#d73027") + xlab("Date") + ylab("Deaths & Cases") +
  ggtitle("Number of deaths and cases in Peru")


peru.p1 <- ggplot(peru.training, aes(date, cases)) + geom_line(color = "#d73027") +
  scale_x_date(date_labels = "%d-%b") + xlab("Date") + ylab("Cases") +
  ggtitle("Number of cases in the Peru")
peru.p1

peru.p2 <- ggplot(peru.training, aes(cases, deaths)) + geom_point(color="#d73027") +
  xlab("Cases") + ylab("Deaths") +
  ggtitle("Peru")
peru.p2

peru.p3 <- ggplot(peru.training, aes(Cumulative_number_for_14_days_of_COVID.19_cases_per_100000, deaths)) + geom_point(color="#d73027") +
  xlab("Cumulative Cases") + ylab("Deaths") +
  ggtitle("Peru")
peru.p3

peru.p4 <- ggplot(peru.training, aes(date, Cumulative_number_for_14_days_of_COVID.19_cases_per_100000)) + geom_point(color="#d73027") +
  xlab("Date") + ylab("Cumulative cases") +
  ggtitle("Peru")
peru.p4

grid.arrange(peru.p3, peru.p4, nrow = 2)

#Time Series Modelling 

#Peru
peruacf1 <- autoplot(acf(peru.training$deaths))

#we have a trend present 

#I will try linear regression
linear.fit <- lm(peru.training$deaths ~ peru.training$day.no)
summary(linear.fit)

#obtaining the residuals in order to plot them
resid.peru <- peru.training$deaths - linear.fit$fitted.values

p.resid <- ggplot(peru.training, aes(day.no, resid.peru)) +
  geom_line(color = "#4575b4", alpha=0.7) + ylab("Residuals") + xlab("Day")
acf.p <- autoplot(acf(resid.peru), main = "ACF")
pacf.p <- autoplot(pacf(resid.peru), main = "PACF")
grid.arrange(p.resid, acf.p, pacf.p)
#from the acf and pacf plots it looks like a MA(1) process could be suitable to model the short-term correlation

auto.arima(ts(peru.training$deaths, start = 1))

model.ar.peru <- arima(resid.peru, order= c(0,0,1))
summary(model.ar.peru)

acf3 <- autoplot(acf(model.ar.peru$residuals), main = "ACF")
pacf3 <- autoplot(pacf(model.ar.peru$residuals), main = "PACF")

grid.arrange(acf3, pacf3)
#the acf and pacf plots look like a purely random process now

#prediction with linear regression
x <- peru.training$deaths
time <- 1:49
model <- lm(x~time)
time.predict<- 50:65
model.predict<- predict(model, newdata=data.frame(time<- time.predict),
                        se.fit = TRUE, interval="prediction")
model.predict

plot(c(1:65), c(model$fitted.values, model.predict$fit[ ,1]), type="l",
   ylab="Deaths", main="Prediction with linear regression Peru",
     xlab="Time", col="#fc4e2a")
points(x, pch=19, col=alpha("#b10026",0.7))
lines(50:65, model.predict$fit[ ,2], col="#fd8d3c", lty=2)
lines(50:65, model.predict$fit[ ,3], col="#fd8d3c", lty=2)

RMSE <- sqrt(sum((model.predict$fit - peru.test$deaths)^2)/16)
RMSE

#prediction for MA(1) with linear trend
day.p <- 1:49
day.predict <- 50:65
model.ma1 <- arima(peru.training$deaths, order=c(0,0,1), xreg=day.p, include.mean=TRUE)

predict.ma1 <- predict(model.ma1, n.ahead = 16, newxreg = day.predict, se.fit = TRUE)

ma1.LCI <- predict.ma1$pred - 1.96*predict.ma1$se
ma1.UCI <- predict.ma1$pred + 1.96*predict.ma1$se


plot(1:65, c(peru.training$deaths, predict.ma1$pred), type="l", xlab="Day",
     ylab="Number of deaths", main="Forecast for MA(1) with linear trend")
lines(50:65, predict.ma1$pred, col="#fc4e2a")
lines(50:65, ma1.LCI, lty=2, col="#fd8d3c")
lines(50:65, ma1.UCI, lty=2, col="#fd8d3c")

predict.ma1$pred
RMSE <- sqrt(sum((predict.ma1$pred - peru.test$deaths)^2)/16)
RMSE

#Netherlands

t.ts <- nl.training$date 
nl.ts <- data.frame(date = t.ts, day.no = 1:length(t.ts), deaths = nl.training$deaths)
deaths.ts <- ts(nl.training$deaths, start = 1)

#checking the ACF plot
autoplot(acf(deaths.ts), main = "ACF for Netherlands")

#fitting a linear regression spline
t1 <- ts(nl.training$day.no, start = 1)
t.break1 <- 15
t.break2 <- 30  

#creating the break points
tb1 <- ts(pmax(0,t1-t.break1),start=1)
tb2 <- ts(pmax(0,t1-t.break2),start=1)  

#fitting the spline
nl.pw <- tslm(deaths.ts ~ t1+tb1+tb2, lambda = 0)
summary(nl.pw)

#plotting the fit
autoplot(deaths.ts) +
  autolayer(fitted(nl.pw), series = "piecewise")

#removing the trend 
resid.nl <- deaths.ts - nl.pw$fitted.values

#now we will look at the acf and pacf plots to see if there is any short - term correlation
resid.nl.p <- autoplot(resid.nl)
acf.nl <- autoplot(acf(resid.nl))
pacf.nl <- autoplot(pacf(resid.nl))
grid.arrange(resid.nl.p, acf.nl, pacf.nl)

#it appears that there is some short term correlation at lag 12 and lag 15 on the acf plot.
#on the pacf plot there is some short term correlation at lag 12.
#Perhaps we can apply a MA(1) to model the short term correlation.

auto.arima(deaths.ts)
#auto.arima also suggesting MA(1) could be suitable for modelling the short-term correlation.

#fitting an MA(1)
model.ar.nl <- arima(resid.nl, order= c(0,0,1))

#plotting the residuals 
p3 <- autoplot(model.ar.nl$residuals, ylab = "MA(1) residuals series")
acf2 <- autoplot(acf(model.ar.nl$residuals), main = "ACF")
pacf2 <- autoplot(pacf(model.ar.nl$residuals, plot = FALSE), main = "PACF")
grid.arrange(p3, acf2, pacf2, nrow = 3)

#prediction

#with the regression spline
t.new <- t1[length(t1)]+seq(16)
tb1.new <- tb1[length(tb1)]+seq(16)
tb2.new <- tb2[length(tb2)]+seq(16)
newdata <- as.data.frame(cbind(t1=t.new,tb1=tb1.new,tb2=tb2.new))

#obtaining forecast for the piecewise regression spline
fcasts.pw <- forecast(nl.pw,newdata=newdata)
fcasts.pw

#calculating the RMSE for piecewise log-transformed spline
fcasts.pw %>% accuracy(nl.test$deaths)


#fitting a model without the log transformation
nl.pw2 <- tslm(deaths.ts ~ t1+tb1+tb2)
fcasts.pw2 <- forecast(nl.pw2, newdata = newdata)

#RMSE for non-log transformed spline
fcasts.pw2 %>% accuracy(nl.test$deaths)


#exponential smoothing
predict2.nl <- ets(ts(nl.training$deaths),model="AAN")

fit.ets <- ets(nl.training$deaths)
checkresiduals(fit.ets)

p.nl <- predict2.nl %>% forecast(h = length(nl.test$deaths))

#calculating the RMSE
f2 %>%
  accuracy(nl.test$deaths)

#plotting the fits and confidence intervals
autoplot(deaths.ts, ylab = "Deaths") +
  autolayer(fitted(nl.pw), series = "piecewise.log") +
  autolayer(fitted(nl.pw2), series = "piecewise") +
  autolayer(fitted(predict2.nl), series = "exponential smoothing") +
  autolayer(fcasts.pw2, series = "piecewise pred", alpha = 0.5) +
  autolayer(f2, series = "exponential pred", alpha = 0.6) +
  autolayer(fcasts.pw, series = "piecewise log pred", alpha = 0.5)










