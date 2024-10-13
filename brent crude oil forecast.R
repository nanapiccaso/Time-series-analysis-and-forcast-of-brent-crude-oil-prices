#Data
library(readr)
Brent_Spot_Price1 <- read_csv("C:/Users/michael bamfo/Downloads/YellowElephantProductions.58264AF513589_p3e1zgp7z7szg!App/Downloads/Brent Spot Price1.csv")
View(Brent_Spot_Price1)
class(Brent_Spot_Price1)

brents <- Brent_Spot_Price1 %>% 
  select(-year)

brent_ts <- ts(brents, start = c(1990,1), end=c(2022,12), frequency = 12 )

class(brent_ts)
start(brent_ts)
end(brent_ts)
summary(brent_ts)


#plotting
plot(brent_ts)
abline(lm(brent_ts~time(brent_ts)))
plot(decompose(brent_ts))

#cyclicalpattern
boxplot(brent_ts ~cycle(brent_ts))

#making it stationary
#making variance constant

plot(log(brent_ts))
abline(lm(log(brent_ts)~time(log(brent_ts))))

#making mean constant

plot(diff(log(brent_ts)))

abline(lm(diff(log(brent_ts))~time(diff(log(brent_ts)))))

#using ARIMA MODEL
acf(brent_ts)
acf(diff(log(brent_ts)))
pacf(diff(log(brent_ts)))

arima(log(brent_ts),c(0,1,1), seasonal = list(order=c(0,1,0),period=12)) -> brent_time
predict(brent_time,10*12)->brent_predict
brent_predict

2.718^brent_predict$pred -> brent_predict
brent_predict

#plotting prediction

ts.plot(brent_ts,brent_predict, log="y", lty =c(1,3))
