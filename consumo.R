#Librerias
library("readxl")
library(tibbletime)
library(dplyr)
library(tidyverse)
library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

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
plot(dec.Diesel$seasonal)

#Aplicaremos una transformación logarítmica
logDiesel<-log(diesel_ts)
plot(decompose(logDiesel))

#Ver el gráfico de la serie
plot(logDiesel)

#Para saber si hay raíces unitarias
adfTest(logDiesel)
adfTest(diff(logDiesel))

#Gráfico de autocorrelación
acf(logDiesel)

# funciones de autocorrelación y autocorrelación parcial
acf(diff(logDiesel),12)
pacf(diff(logDiesel))

# Hacer el modelo
auto.arima(diesel_ts)
fit<-arima(log(diesel_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 10*12)
ts.plot(diesel_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(diesel_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP1<-forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP1)



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
plot(dec.Super$seasonal)

#Aplicaremos una transformación logarítmica
logSuper<-log(super_ts)
plot(decompose(logSuper))

#Ver el gráfico de la serie
plot(logSuper)

#Para saber si hay raíces unitarias
adfTest(logSuper)
adfTest(diff(logSuper))

#Gráfico de autocorrelación
acf(logSuper)

# funciones de autocorrelación y autocorrelación parcial
acf(diff(logSuper),12)
pacf(diff(logSuper))

# Hacer el modelo
auto.arima(super_ts)
fit<-arima(log(super_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 10*12)
ts.plot(super_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(super_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP2<-forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP2)



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
plot(dec.Regular$seasonal)

#Aplicaremos una transformación logarítmica
logRegular<-log(regular_ts)
plot(decompose(logRegular))

#Ver el gráfico de la serie
plot(logRegular)

#Para saber si hay raíces unitarias
adfTest(logRegular)
adfTest(diff(logRegular))

#Gráfico de autocorrelación
acf(logRegular)

# funciones de autocorrelación y autocorrelación parcial
acf(diff(logRegular),12)
pacf(diff(logRegular))

# Hacer el modelo
auto.arima(regular_ts)
fit<-arima(log(regular_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred<-predict(fit, n.ahead = 10*12)
ts.plot(regular_ts,2.718^pred$pred, log = "y", lty = c(1,3))
fit2<-arima(log(regular_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))
forecastAP3<-forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP3)
