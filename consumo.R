# Loading
library("readxl")
library(tibbletime)
library(dplyr)
library(tidyverse)

# xls files
consumo <- read_excel("Consumo.xlsx")

consumo$Fecha<-as.Date(consumo$Fecha, "%Y/%m/%d")
str(consumo)
view(consumo)

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
###  1. ¿Cuáles son los 10 meses que más consumo registraron de diesel? <br/>
fecha<-consumo[,'Fecha']
diesel<-consumo[,'Diesel alto azufre']
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
fecha<-consumo[,'Fecha']
super<-consumo[,'Gasolina superior']
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
fecha<-consumo[,'Fecha']
regular<-consumo[,'Gasolina regular']
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
fecha<-consumo[,'Fecha']
diesel<-consumo[,'Diesel alto azufre']
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
fecha<-consumo[,'Fecha']
super<-consumo[,'Gasolina superior']
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
fecha<-consumo[,'Fecha']
regular<-consumo[,'Gasolina regular']
q6<-data.frame(fecha, regular)
ask6<-q6[order(q6$Gasolina.regular),]
ask6f<-head(ask6,n=10)

pregunta6<-ggplot(data=ask6f, aes(x=reorder(Fecha,Gasolina.regular) , y=Gasolina.regular,fill=Fecha)) +
  geom_bar(stat="identity")+
  scale_y_continuous(labels=scales::dollar)+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1))+
  labs(title="Los 10 meses que menos consumo registraron de gasolina regular", x="Fecha", y="Gasolina regular")
pregunta6
