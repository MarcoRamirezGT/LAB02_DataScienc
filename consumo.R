#Librerias
library("readxl")
library(tibbletime)
library(dplyr)
library(tidyverse)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)
library(lmtest)
library(prophet)
library(zoo)

#Leer xls files
consumo <- read_excel("Consumo.xlsx")

consumo$Fecha<-as.Date(consumo$Fecha, "%Y/%m/%d")
str(consumo)

#Resumen general
summary(consumo)

#### Fecha: Cuantitativa continua
#### Gasolina superior: Cuantitativa continua
#### Gasolina regular: Cuantitativa continua
#### Diesel alto azufre: Cuantitativa continua

#Gráficos
plot(consumo$Fecha)
hist(x = consumo$`Gasolina superior`)
hist(x = consumo$`Gasolina regular`)
hist(x = consumo$`Diesel alto azufre`)

## Correlación de las variables numéricas
df1<-data.frame(consumo$`Gasolina superior`, consumo$`Gasolina regular`, consumo$`Diesel alto azufre`)
corrplot::corrplot(cor(df1))

#PREGUNTAS
fecha<-consumo[,'Fecha']
diesel<-consumo[,'Diesel alto azufre']
super<-consumo[,'Gasolina superior']
regular<-consumo[,'Gasolina regular']

dieselc<-consumo[c('Fecha','Diesel alto azufre')]
superc<-consumo[c('Fecha','Gasolina superior')]
regularc<-consumo[c('Fecha','Gasolina regular')]

###  1. ¿Cuáles son los 10 meses que más consumo registraron de diesel? <br/>
q1<-data.frame(fecha, diesel)
ask1<-q1[order(-q1$Diesel.alto.azufre),]
ask1f<-head(ask1,n=10)

pregunta1<-ggplot(data=ask1f, aes(x=reorder(Fecha,-Diesel.alto.azufre) , y=Diesel.alto.azufre,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que más consumo registraron de diesel", x="Fecha", y="Diesel")
pregunta1

###  2. ¿Cuáles son los 10 meses que más consumo registraron de gasolina superior? <br/>
q2<-data.frame(fecha, super)
ask2<-q2[order(-q2$Gasolina.superior),]
ask2f<-head(ask2,n=10)

pregunta2<-ggplot(data=ask2f, aes(x=reorder(Fecha,-Gasolina.superior) , y=Gasolina.superior,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que más consumo registraron de gasolina superior", x="Fecha", y="Gasolina superior")
pregunta2

###  3. ¿Cuáles son los 10 meses que más consumo registraron de gasolina regular? <br/>
q3<-data.frame(fecha, regular)
ask3<-q3[order(-q3$Gasolina.regular),]
ask3f<-head(ask3,n=10)

pregunta3<-ggplot(data=ask3f, aes(x=reorder(Fecha,-Gasolina.regular) , y=Gasolina.regular,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que más consumo registraron de gasolina regular", x="Fecha", y="Gasolina regular")
pregunta3

###  4. ¿Cuáles son los 10 meses que menos consumo registraron de diesel? <br/>
q4<-data.frame(fecha, diesel)
ask4<-q4[order(q4$Diesel.alto.azufre),]
ask4f<-head(ask4,n=10)

pregunta4<-ggplot(data=ask4f, aes(x=reorder(Fecha,Diesel.alto.azufre) , y=Diesel.alto.azufre,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que menos consumo registraron de diesel", x="Fecha", y="Diesel")
pregunta4

###  5. ¿Cuáles son los 10 meses que menos consumo registraron de gasolina superior? <br/>
q5<-data.frame(fecha, super)
ask5<-q5[order(q5$Gasolina.superior),]
ask5f<-head(ask5,n=10)

pregunta5<-ggplot(data=ask5f, aes(x=reorder(Fecha,Gasolina.superior) , y=Gasolina.superior,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que menos consumo registraron de gasolina superior", x="Fecha", y="Gasolina superior")
pregunta5

###  6. ¿Cuáles son los 10 meses que menos consumo registraron de gasolina regular? <br/>
q6<-data.frame(fecha, regular)
ask6<-q6[order(q6$Gasolina.regular),]
ask6f<-head(ask6,n=10)

pregunta6<-ggplot(data=ask6f, aes(x=reorder(Fecha,Gasolina.regular) , y=Gasolina.regular,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que menos consumo registraron de gasolina regular", x="Fecha", y="Gasolina regular")
pregunta6





#Series de tiempo
#Diesel
diesel_ts<-ts(dieselc$`Diesel alto azufre`, start = c(2001,1),frequency = 12)
start(diesel_ts)
end(diesel_ts)
frequency(diesel_ts)
plot(diesel_ts)
abline(reg=lm(diesel_ts~time(diesel_ts)), col=c("red"))
plot(aggregate(diesel_ts,FUN=mean))
dec.Diesel<-decompose(diesel_ts)
plot(dec.Diesel)

train<-head(diesel_ts, round(length(diesel_ts) * 0.7))
h<-length(diesel_ts) - length(train)
test<-tail(diesel_ts, h)

#Aplicaremos una transformación logarítmica
logDiesel<-log(train)
plot(decompose(train))
plot(train)

adfTest(train)
unitrootTest(train)

adfTest(diff(train))
unitrootTest(diff(train))

#Gráfico de autocorrelación
acf(logDiesel,50)
pacf(logDiesel,50)

decTrain<-decompose(train)
plot(decTrain$seasonal)

acf(diff(logDiesel),36)
pacf(diff(logDiesel),36)

fitArima<-arima(logDiesel,order=c(2,1,2),seasonal = c(1,1,0))
fitAutoArima<-auto.arima(train)

coeftest(fitArima)
coeftest(fitAutoArima)

qqnorm(fitArima$residuals)
qqline(fitArima$residuals)
checkresiduals(fitArima)

qqnorm(fitAutoArima$residuals)
qqline(fitAutoArima$residuals)
checkresiduals(fitAutoArima)

# Hacer el modelo
auto.arima(diesel_ts)
fit<-arima(log(diesel_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 3)
ts.plot(diesel_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(diesel_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP1<-forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP1)

diesel_ts2018<-ts( diesel$`Diesel alto azufre`, start = c(2001,1), end=c(2020,12) ,frequency = 12)
auto.arima(diesel_ts2018)
fit<-arima(log(diesel_ts2018), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 3)
ts.plot(diesel_ts2018,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(diesel_ts2018), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP<-forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP)

df<-data.frame(ds=as.Date(as.yearmon(time(train))),y=as.matrix(train) )
testdf<-data.frame(ds=as.Date(as.yearmon(time(test))),y=as.matrix(test) )
head(df)
fitProphet<-prophet(df, yearly.seasonality = TRUE, weekly.seasonality = TRUE)
future<-make_future_dataframe(fitProphet,periods = h,freq = "month", include_history = T)
p<-predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)
pred<-tail(p,h)
pred$y<-testdf$y

ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")



#Super
super_ts<-ts(superc$`Gasolina superior`, start = c(2001,1),frequency = 12)
start(super_ts)
end(super_ts)
frequency(super_ts)
plot(super_ts)
abline(reg=lm(super_ts~time(super_ts)), col=c("red"))
plot(aggregate(super_ts,FUN=mean))
dec.Super<-decompose(super_ts)
plot(dec.Super)

train<-head(super_ts, round(length(super_ts) * 0.7))
h<-length(super_ts) - length(train)
test<-tail(super_ts, h)

#Aplicaremos una transformación logarítmica
logSuper<-log(train)
plot(decompose(train))
plot(train)

adfTest(train)
unitrootTest(train)

adfTest(diff(train))
unitrootTest(diff(train))

#Gráfico de autocorrelación
acf(logSuper,50)
pacf(logSuper,50)

decTrain<-decompose(train)
plot(decTrain$seasonal)

acf(diff(logSuper),36)
pacf(diff(logSuper),36)

fitArima<-arima(logSuper,order=c(2,1,2),seasonal = c(1,1,0))
fitAutoArima<-auto.arima(train)

coeftest(fitArima)
coeftest(fitAutoArima)

qqnorm(fitArima$residuals)
qqline(fitArima$residuals)
checkresiduals(fitArima)

qqnorm(fitAutoArima$residuals)
qqline(fitAutoArima$residuals)
checkresiduals(fitAutoArima)

# Hacer el modelo
auto.arima(super_ts)
fit<-arima(log(super_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 3)
ts.plot(super_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(super_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP2<-forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP2)

super_ts2018<-ts( super$`Gasolina superior`, start = c(2001,1), end=c(2020,12) ,frequency = 12)
auto.arima(super_ts2018)
fit<-arima(log(super_ts2018), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 3)
ts.plot(super_ts2018,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(super_ts2018), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP<-forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP)

df<-data.frame(ds=as.Date(as.yearmon(time(train))),y=as.matrix(train) )
testdf<-data.frame(ds=as.Date(as.yearmon(time(test))),y=as.matrix(test) )
head(df)
fitProphet<-prophet(df, yearly.seasonality = TRUE, weekly.seasonality = TRUE)
future<-make_future_dataframe(fitProphet,periods = h,freq = "month", include_history = T)
p<-predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)
pred<-tail(p,h)
pred$y<-testdf$y

ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")



#Regular
regular_ts<-ts(regularc$`Gasolina regular`, start = c(2001,1),frequency = 12)
start(regular_ts)
end(regular_ts)
frequency(regular_ts)
plot(regular_ts)
abline(reg=lm(regular_ts~time(regular_ts)), col=c("red"))
plot(aggregate(regular_ts,FUN=mean))
dec.Regular<-decompose(regular_ts)
plot(dec.Regular)

train<-head(regular_ts, round(length(regular_ts) * 0.7))
h<-length(regular_ts) - length(train)
test<-tail(regular_ts, h)

#Aplicaremos una transformación logarítmica
logRegular<-log(train)
plot(decompose(train))
plot(train)

adfTest(train)
unitrootTest(train)

adfTest(diff(train))
unitrootTest(diff(train))

#Gráfico de autocorrelación
acf(logRegular,50)
pacf(logRegular,50)

decTrain<-decompose(train)
plot(decTrain$seasonal)

acf(diff(logRegular),36)
pacf(diff(logRegular),36)

fitArima<-arima(logRegular,order=c(2,1,2),seasonal = c(1,1,0))
fitAutoArima<-auto.arima(train)

coeftest(fitArima)
coeftest(fitAutoArima)

qqnorm(fitArima$residuals)
qqline(fitArima$residuals)
checkresiduals(fitArima)

qqnorm(fitAutoArima$residuals)
qqline(fitAutoArima$residuals)
checkresiduals(fitAutoArima)

# Hacer el modelo
auto.arima(regular_ts)
fit<-arima(log(regular_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 3)
ts.plot(regular_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(regular_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP3<-forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP3)

regular_ts2018<-ts( regular$`Gasolina regular`, start = c(2001,1), end=c(2020,12) ,frequency = 12)
auto.arima(regular_ts2018)
fit<-arima(log(regular_ts2018), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 3)
ts.plot(regular_ts2018,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(regular_ts2018), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP<-forecast(fit2, level = c(95), h = 3)
autoplot(forecastAP)

df<-data.frame(ds=as.Date(as.yearmon(time(train))),y=as.matrix(train) )
testdf<-data.frame(ds=as.Date(as.yearmon(time(test))),y=as.matrix(test) )
head(df)
fitProphet<-prophet(df, yearly.seasonality = TRUE, weekly.seasonality = TRUE)
future<-make_future_dataframe(fitProphet,periods = h,freq = "month", include_history = T)
p<-predict(fitProphet,future)
p<-p[,c("ds","yhat","yhat_lower","yhat_upper")]
plot(fitProphet,p)
pred<-tail(p,h)
pred$y<-testdf$y

ggplot(pred, aes(x=ds, y=yhat)) +
  geom_line(size=1, alpha=0.8) +
  geom_ribbon(aes(ymin=yhat_lower, ymax=yhat_upper), fill="blue", alpha=0.2) +
  geom_line(data=pred, aes(x=ds, y=y),color="red")
