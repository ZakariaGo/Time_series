library('ggplot2')
library('forecast')
library('tseries')

daily_data = read.csv('/home/hajji/day.csv', header=TRUE, stringsAsFactors=FALSE)

daily_data$Date = as.Date(daily_data$dteday)

ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")


count_ts = ts(daily_data[, c('cnt')])

daily_data$clean_cnt = tsclean(count_ts)

ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')



daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)


ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)


adf.test(count_ma, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_ma
# Dickey-Fuller = -0.2557, Lag order = 8, p-value = 0.99
# alternative hypothesis: stationary



Acf(count_ma, main='')

Pacf(count_ma, main='')

count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
adf.test(count_d1, alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  count_d1
# Dickey-Fuller = -9.9255, Lag order = 8, p-value = 0.01
# alternative hypothesis: stationary



Acf(count_d1, main='ACF for Differenced Series')
Pacf(count_d1, main='PACF for Differenced Series')




auto.arima(deseasonal_cnt, seasonal=FALSE)

# Series: deseasonal_cnt
# ARIMA(1,1,1)
# 
# Coefficients:
#   ar1      ma1
# 0.5510  -0.2496
# s.e.  0.0751   0.0849
# 
# sigma^2 estimated as 26180:  log likelihood=-4708.91
# AIC=9423.82   AICc=9423.85   BIC=9437.57


fit<-auto.arima(deseasonal_cnt, seasonal=FALSE)
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')




fit2 = arima(deseasonal_cnt, order=c(1,1,7))

fit2

tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')

# Call:
#   arima(x = deseasonal_cnt, order = c(1, 1, 7))
# 
# Coefficients:
#   ar1     ma1     ma2     ma3     ma4     ma5     ma6      ma7
# 0.2803  0.1465  0.1524  0.1263  0.1225  0.1291  0.1471  -0.8353
# s.e.  0.0478  0.0289  0.0266  0.0261  0.0263  0.0257  0.0265   0.0285
# 
# sigma^2 estimated as 14392:  log likelihood = -4503.28,  aic = 9024.56




fcast <- forecast(fit2, h=30)
plot(fcast)


hold <- window(ts(deseasonal_cnt), start=700)

fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))



fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

# Series: deseasonal_cnt
# ARIMA(2,1,2)(1,0,0)[30]
# 
# Coefficients:
#   ar1      ar2      ma1     ma2    sar1
# 1.3644  -0.8027  -1.2903  0.9146  0.0100
# s.e.  0.0372   0.0347   0.0255  0.0202  0.0388
# 
# sigma^2 estimated as 24810:  log likelihood=-4688.59
# AIC=9389.17   AICc=9389.29   BIC=9416.68


seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)







