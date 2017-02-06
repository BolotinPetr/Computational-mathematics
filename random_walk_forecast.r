
library("lubridate")

library("sandwich")
library("lmtest")
library("car")

library("zoo")
library("xts")
library("forecast")

library("dplyr")


library("broom")

library("ggplot2")

library("stats")

WFData <- read.table("C:/WunderFund_data2.csv", header=TRUE, 
  	sep=",")

price <- WFData$price
tsdisplay(price, lag.max = 100)

dprice = diff(price)
tsdisplay(dprice)

model_a <- auto.arima(price)
summary(model_a)

forecast_price <- forecast(model_a, h = 400)
plot(forecast_price)

N <- 100000
profit <- c()
for (i in 1:N) {
  s = simulate(model_a, nsim = 400)[400]
  if (s > 300) {
    profit <- append(profit, s-300)
  }
}

final_hist <- hist(profit, breaks = 100)

print(final_hist)

pr <- ((final_hist$counts%*%final_hist$mids)/N)[1,1]

print(pr)


