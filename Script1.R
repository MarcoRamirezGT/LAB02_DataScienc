
# Loading
library("readxl")
library(ggplot2)

# xls files
importacion <- read_excel("importacion.xlsx")


importacion$Fecha<-as.Date(importacion$Fecha, "%Y/%mm/%d")


nrow(importacion)
ncol(importacion)


#Que mes fue el que mas tuvo mas importacion de gasolina superior
superior <- importacion[order(-importacion$`Gasolina superior`),]
superior<- head(superior,10)
superior<- superior[,1:2]


#Que mes fue el que tuvo menos importacion  de gasolina inferior

inferior <- importacion[order(importacion$`Gasolina superior`),]
inferior<- head(inferior,10)
inferior<- inferior[,1:2]
View(inferior)

#Que mes fue el que hubo mayor importacion de gasolina diesel

dieselImpo <- importacion[order(-importacion$`Diesel alto azufre`),]
dieselImpo<- head(dieselImpo,10)
dieselImpo<- dieselImpo[c('Fecha','Diesel alto azufre')]
View(dieselImpo)

#Que mes fue el que hubo menos importacion de gasolina diesel

diesel_menor_impor <- importacion[order(importacion$`Diesel alto azufre`),]
diesel_menor_impor<- head(diesel_menor_impor,10)
diesel_menor_impor<- diesel_menor_impor[c('Fecha','Diesel alto azufre')]
View(diesel_menor_impor)

#Que mes fue el que hubo mayor importacion de gasolina regular

regular_impo <- importacion[order(-importacion$`Gasolina regular`),]
regular_impo<- head(regular_impo,10)
regular_impo<- regular_impo[c('Fecha','Gasolina regular')]
View(regular_impo)

#Que mes fue el que hubo menos importacion de gasolina regular 

regular_impo_menor <- importacion[order(importacion$`Gasolina regular`),]
regular_impo_menor<- head(regular_impo_menor,10)
regular_impo_menor<- regular_impo_menor[c('Fecha','Gasolina regular')]
View(regular_impo_menor)


#Que tipo de gasolina es la mas importada durante los años

ask1<-data.frame(Tipo=c('Superior','Regular','Diesel'),
                 Promedio=c(mean(importacion$`Gasolina superior`),mean(importacion$`Gasolina regular`),mean(importacion$`Diesel alto azufre`)))


ask1_chart<-ggplot(data=ask1, aes(x=Tipo, y=Promedio, fill=Tipo)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=as.integer(Promedio)), vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5)+
  labs(title="Promedio de importaciones por tipo de gasolina", y="Promedio")+
  theme(legend.position="none")


ask1_chart


#Año que mas importaciones hubo


chart_superior<-ggplot(importacion, aes(x = format(importacion$Fecha, format='%Y'), y = importacion$`Gasolina superior`, fill =format(importacion$Fecha, format='%Y') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por año de gasolina Superior", y="Importacion" , x='Año')+
  theme(legend.position="none")
#2021 Fue el año con mas importaciones
#Vemos a detalle que paso en ese año


mes_2021<-subset(importacion,Fecha<'2022-01-01' & Fecha>'2020-12-31')


chart_superior_mes<-ggplot(mes_2021, aes(x = format(mes_2021$Fecha, format='%m'), y = mes_2021$`Gasolina superior`, fill =format(mes_2021$Fecha, format='%m') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por mes de gasolina Superior en el año 2021", y="Importacion" , x='Mes')+
  theme(legend.position="none")
 
#Gasolina Regular

chart_regular<-ggplot(importacion, aes(x = format(importacion$Fecha, format='%Y'), y = importacion$`Gasolina regular`, fill =format(importacion$Fecha, format='%Y') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por año de gasolina Regular", y="Importacion" , x='Año')+
  theme(legend.position="none")

mes_2022<-subset(importacion,Fecha>'2021-12-31')

chart_regular_mes<-ggplot(mes_2022, aes(x = format(mes_2022$Fecha, format='%m'), y = mes_2022$`Gasolina regular`, fill =format(mes_2022$Fecha, format='%m') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por mes de gasolina Superior en el año 2022", y="Importacion" , x='Mes')+
  theme(legend.position="none")


#Gasolina Diesel

chart_diesel<-ggplot(importacion, aes(x = format(importacion$Fecha, format='%Y'), y = importacion$`Diesel alto azufre`, fill =format(importacion$Fecha, format='%Y') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por año de gasolina Diesel", y="Importacion" , x='Año')+
  theme(legend.position="none")

mes_2014<-subset(importacion,Fecha<'2015-01-01' & Fecha>'2013-12-31')
View(mes_2014)

chart_diesel_mes<-ggplot(mes_2014, aes(x = format(mes_2014$Fecha, format='%m'), y = mes_2014$`Gasolina regular`, fill =format(mes_2014$Fecha, format='%m') )) +
  geom_bar(stat = "identity",position=position_dodge())+labs(title="Importaciones por mes de gasolina Diesel en el año 2014", y="Importacion" , x='Mes')+
  theme(legend.position="none")

#Series de tiempo  


library(forecast)
library(tseries)
library(fUnitRoots)
library(ggfortify)

importacion <- read_excel("importacion.xlsx")

regular<-importacion[c('Fecha','Gasolina regular')]
diesel<-importacion[c('Fecha','Diesel alto azufre')]
super<-importacion[c('Fecha','Gasolina superior')]

regular_ts <- ts( regular$`Gasolina regular`, start = c(2001,1),frequency = 12)


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
logRegular <- log(regular_ts)
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

auto.arima(AirPassengers)

fit <- arima(log(regular_ts), c(0, 1, 1),seasonal = list(order = c(0, 1, 1), period = 12))
pred <- predict(fit, n.ahead = 10*12)
ts.plot(regular_ts,2.718^pred$pred, log = "y", lty = c(1,3))

fit2 <- arima(log(regular_ts), c(2, 1, 1),seasonal = list(order = c(0, 1, 0), period = 12))

forecastAP <- forecast(fit2, level = c(95), h = 120)
autoplot(forecastAP)
